#!/usr/bin/env python
"""Convert the PDFs in documents/ to Markdown and extract each figure and table
as its own file.

For every <slug>.pdf in documents/ this writes:

    documents/<slug>/<slug>.md      full text as Markdown
    documents/<slug>/meta.json      page count, headings, figure/table inventory
    documents/<slug>/figures/fig-NN.png
    documents/<slug>/tables/tab-NN.png
    documents/<slug>/tables/tab-NN.csv

Figures and tables are located by finding their captions in the text layer and
then growing a bounding box over the drawing objects (curves/rects/lines/images)
that sit next to the caption.  That works for vector plots, which is what R and
Stata emit, and which pulling embedded rasters out of the PDF would miss.

Usage:
    .venv/bin/python tools/pdf_extract.py                # all PDFs
    .venv/bin/python tools/pdf_extract.py voters_veto    # named slugs only
"""

from __future__ import annotations

import csv
import json
import re
import statistics
import sys
from collections import Counter
from dataclasses import dataclass, field, asdict
from pathlib import Path

import pdfplumber
import pypdfium2 as pdfium

ROOT = Path(__file__).resolve().parent.parent
DOCS = ROOT / "documents"

RENDER_SCALE = 3.0  # 3.0 x 72dpi = 216 dpi
PAD = 6.0  # points of whitespace around a cropped region
# Some PDFs place words without emitting space glyphs; the default tolerance
# of 3 then runs whole sentences together. 1.5 recovers them safely.
X_TOL = 1.5

# Matches "Figure 1:", "Table A-1.", "Table C-10", and - because some PDFs
# position words without emitting space glyphs - "Table2:".
CAPTION_RE = re.compile(
    r"^\s*(?P<kind>Appendix\s+Figure|Appendix\s+Table|Figure|Fig\.|Table)"
    r"\s*(?P<num>(?:[A-Z][-.–]?)?\d+(?:[.-]\d+)*[a-z]?)"
    r"\s*[:.–—-]?\s*(?P<rest>.*)$"
)
NUMBERED_HEADING_RE = re.compile(r"^\d+(\.\d+)*\.?\s+[A-Z]")


# --------------------------------------------------------------------------
# geometry helpers
# --------------------------------------------------------------------------


def union(boxes):
    """Union of (x0, top, x1, bottom) tuples."""
    return (
        min(b[0] for b in boxes),
        min(b[1] for b in boxes),
        max(b[2] for b in boxes),
        max(b[3] for b in boxes),
    )


def obj_box(o):
    return (o["x0"], o["top"], o["x1"], o["bottom"])


def clamp(box, page):
    x0, top, x1, bottom = box
    return (
        max(x0 - PAD, page.bbox[0]),
        max(top - PAD, 0),
        min(x1 + PAD, page.bbox[2]),
        min(bottom + PAD, page.height),
    )


# --------------------------------------------------------------------------
# page furniture
# --------------------------------------------------------------------------


def drawing_objects(page):
    """Drawing primitives, minus page rules and full-width furniture."""
    out = []
    pw = page.width
    for o in list(page.curves) + list(page.rects) + list(page.lines) + list(page.images):
        w = o["x1"] - o["x0"]
        h = o["bottom"] - o["top"]
        if w > 0.95 * pw and h < 3:  # page rule
            continue
        if w <= 0.7 and h <= 0.7:  # stray dot
            continue
        if o["top"] < 0 or o["bottom"] > page.height:
            continue
        out.append(o)
    return out


def horizontal_rules(page, min_width=55.0):
    """Horizontal hairlines - booktabs \\toprule, \\midrule, \\bottomrule."""
    rules = []
    for o in list(page.rects) + list(page.lines):
        w = o["x1"] - o["x0"]
        h = o["bottom"] - o["top"]
        if h <= 2.5 and w >= min_width and w < 0.95 * page.width:
            rules.append(o)
    return sorted(rules, key=lambda o: o["top"])


def find_repeated_lines(pages_lines):
    """Running heads and footers: short lines that recur at the same edge."""
    counts = Counter()
    for lines in pages_lines:
        for ln in lines:
            t = ln["text"].strip()
            if 0 < len(t) < 90:
                counts[re.sub(r"\d+", "#", t)] += 1
    threshold = max(3, len(pages_lines) // 3)
    return {t for t, c in counts.items() if c >= threshold}


# --------------------------------------------------------------------------
# captions
# --------------------------------------------------------------------------


@dataclass
class Caption:
    kind: str  # "figure" | "table"
    label: str  # "Figure 3", "Table A1"
    text: str  # caption prose
    page: int  # 0-indexed
    box: tuple  # bbox of the whole caption block
    line_index: int


def collect_captions(lines, page_no):
    """Find caption blocks in a page's lines."""
    caps = []
    heights = [ln["bottom"] - ln["top"] for ln in lines] or [10]
    lh = statistics.median(heights)
    for i, ln in enumerate(lines):
        m = CAPTION_RE.match(ln["text"])
        if not m:
            continue
        rest = m.group("rest").strip()
        # "Figure 2 shows that ..." is a sentence, not a caption.
        if rest and re.match(r"^(shows?|displays?|reports?|presents?|plots?|"
                             r"and|is|are|in|of|we|the|above|below|here)\b",
                             rest, re.I):
            continue
        # Some papers have a summary table whose first column is "Figure 2",
        # "Figure 3", ... so every row of it looks like a caption. Such a row
        # ends in its numeric columns; a real caption ends in prose.
        if rest and re.search(r"(\s[-+(]?\d[\d.,%)]*){2,}\s*$", rest):
            continue
        kind = "table" if "table" in m.group("kind").lower() else "figure"
        label = f"{m.group('kind').strip()} {m.group('num').strip()}".replace("Fig.", "Figure")
        label = re.sub(r"\s+", " ", label)

        # continuation lines of the caption
        block = [obj_box(ln)]
        parts = [rest]
        j = i + 1
        while j < len(lines) and j - i <= 8:
            nxt = lines[j]
            gap = nxt["top"] - lines[j - 1]["bottom"]
            if gap > 1.4 * lh or CAPTION_RE.match(nxt["text"]):
                break
            block.append(obj_box(nxt))
            parts.append(nxt["text"].strip())
            j += 1
        caps.append(
            Caption(
                kind=kind,
                label=label,
                text=re.sub(r"\s+", " ", " ".join(p for p in parts if p)).strip(),
                page=page_no,
                box=union(block),
                line_index=i,
            )
        )
    # A label appears once per page; repeats are tabular false positives.
    seen, unique = set(), []
    for c in caps:
        if c.label in seen:
            continue
        seen.add(c.label)
        unique.append(c)
    return unique


# --------------------------------------------------------------------------
# region growing
# --------------------------------------------------------------------------


def grow_figure_region(page, cap, other_caption_boxes):
    """Cluster drawing objects adjacent to a figure caption."""
    objs = drawing_objects(page)
    if not objs:
        return None

    cap_top, cap_bottom = cap.box[1], cap.box[3]

    # Captions usually sit under the figure; fall back to above.
    for direction in ("above", "below"):
        if direction == "above":
            cand = [o for o in objs if o["bottom"] <= cap_top + 3]
            cand.sort(key=lambda o: -o["bottom"])
        else:
            cand = [o for o in objs if o["top"] >= cap_bottom - 3]
            cand.sort(key=lambda o: o["top"])
        if not cand:
            continue

        # start from the object nearest the caption, absorb neighbours
        box = obj_box(cand[0])
        edge = cap_top if direction == "above" else cap_bottom
        if abs((box[3] if direction == "above" else box[1]) - edge) > 90:
            continue
        for o in cand[1:]:
            ob = obj_box(o)
            gap = (box[1] - ob[3]) if direction == "above" else (ob[1] - box[3])
            if gap > 45:
                break
            box = union([box, ob])

        # pull in axis labels / legend text that lives inside the plot band
        for _ in range(2):
            words = [
                w
                for w in page.extract_words(x_tolerance=X_TOL)
                if w["top"] >= box[1] - 14
                and w["bottom"] <= box[3] + 14
                and w["x1"] >= box[0] - 60
                and w["x0"] <= box[2] + 60
            ]
            words = [
                w
                for w in words
                if not any(
                    w["top"] >= b[1] - 1 and w["bottom"] <= b[3] + 1 for b in other_caption_boxes
                )
            ]
            if words:
                box = union([box] + [obj_box(w) for w in words])

        if box[2] - box[0] < 45 or box[3] - box[1] < 32:
            continue
        return union([box, cap.box])  # keep the caption in the image
    return None


def grow_table_region(page, cap):
    """A LaTeX table is the band spanned by its horizontal rules."""
    rules = horizontal_rules(page)
    if not rules:
        return None
    cap_top, cap_bottom = cap.box[1], cap.box[3]

    for direction in ("below", "above"):
        if direction == "below":
            cand = [r for r in rules if r["top"] >= cap_bottom - 3]
        else:
            cand = [r for r in rules if r["bottom"] <= cap_top + 3]
        if not cand:
            continue
        edge = cap_bottom if direction == "below" else cap_top
        nearest = min(cand, key=lambda r: abs(r["top"] - edge))
        if abs(nearest["top"] - edge) > 90:
            continue

        group = [nearest]
        for r in sorted(cand, key=lambda r: r["top"]):
            if r is nearest:
                continue
            if any(abs(r["top"] - g["top"]) < 420 for g in group):
                group.append(r)
        box = union([obj_box(r) for r in group])

        words = [
            w
            for w in page.extract_words(x_tolerance=X_TOL)
            if w["top"] >= box[1] - 12 and w["bottom"] <= box[3] + 12
        ]
        # don't swallow the caption itself or body text far outside the rules
        words = [w for w in words if w["x1"] >= box[0] - 30 and w["x0"] <= box[2] + 30]
        if words:
            box = union([box] + [obj_box(w) for w in words])
        if box[3] - box[1] < 18:
            continue
        return union([box, cap.box])
    return None


# --------------------------------------------------------------------------
# markdown body
# --------------------------------------------------------------------------


def detect_columns(page):
    """Return column x-ranges. Two columns only on a genuine two-column layout.

    A wide-bodied table also leaves an empty vertical band down the middle, so
    an empty gutter alone is not enough - splitting on one interleaves the
    table's cells and shreds the captions.  The discriminator is justification:
    real column text runs to the column edge on most lines, table cells do not.
    """
    words = page.extract_words(x_tolerance=X_TOL)
    if len(words) < 60:
        return [(page.bbox[0], page.bbox[2])]
    mid_lo, mid_hi = page.width * 0.38, page.width * 0.62
    if len([w for w in words if w["x0"] < mid_lo and w["x1"] > mid_hi]) > 2:
        return [(page.bbox[0], page.bbox[2])]

    left = [w for w in words if w["x1"] <= page.width * 0.5]
    right = [w for w in words if w["x0"] >= page.width * 0.5]
    if len(left) < 40 or len(right) < 40:
        return [(page.bbox[0], page.bbox[2])]
    gutter_l = max(w["x1"] for w in left)
    gutter_r = min(w["x0"] for w in right)
    if gutter_r - gutter_l < 12:
        return [(page.bbox[0], page.bbox[2])]

    cols = [(page.bbox[0], gutter_l + 2), (gutter_r - 2, page.bbox[2])]
    for x0, x1 in cols:
        try:
            lines = page.crop((x0, 0, x1, page.height)).extract_text_lines(
                layout=False, x_tolerance=X_TOL
            ) or []
        except Exception:
            return [(page.bbox[0], page.bbox[2])]
        body = [l for l in lines if len(l["text"].strip()) > 2]
        if len(body) < 12:
            return [(page.bbox[0], page.bbox[2])]
        width = x1 - x0
        full = sum(1 for l in body if (l["x1"] - l["x0"]) >= 0.75 * width)
        if full / len(body) < 0.5:  # ragged - it is a table, not a column
            return [(page.bbox[0], page.bbox[2])]
    return cols


def page_lines(page):
    """Text lines in reading order, columns respected and tagged."""
    cols = detect_columns(page)
    out = []
    for ci, (x0, x1) in enumerate(cols):
        try:
            crop = page.crop((x0, 0, x1, page.height))
            lines = crop.extract_text_lines(layout=False, x_tolerance=X_TOL) or []
        except Exception:
            continue
        for ln in lines:
            ln["_col"] = ci
        out.extend(lines)
    return out


def local_right_edges(lines, window=6):
    """Per-line right margin, measured over neighbouring lines in the column.

    Paragraph breaks are detected by a line stopping short of its margin, so
    the margin has to be the text block's - not the page's.  A page mixes
    blocks of different widths (title, indented abstract, block quotes,
    footnotes), so a single page-wide margin shreds the narrow ones into
    one-line paragraphs.  A sliding window tracks each block instead.
    """
    edges = []
    for i, ln in enumerate(lines):
        col = ln.get("_col", 0)
        near = [
            l["x1"]
            for l in lines[max(0, i - window) : i + window + 1]
            if l.get("_col", 0) == col
        ]
        near.sort()
        edges.append(near[int(0.9 * (len(near) - 1))] if near else ln["x1"])
    return edges


def dehyphenate(prev, nxt):
    if prev.endswith("-") and not prev.endswith("--") and nxt[:1].islower():
        return prev[:-1] + nxt
    return prev + " " + nxt


def lines_to_markdown(lines, body_size, drop, captions_by_line, right_edges, title=""):
    """Assemble lines into paragraphs, headings and caption placeholders."""
    md, para = [], []
    title_norm = re.sub(r"\W+", "", title).lower()

    def flush():
        if para:
            md.append(" ".join(para).strip())
            para.clear()

    skip_until = -1
    for i, ln in enumerate(lines):
        if i < skip_until:
            continue
        text = ln["text"].strip()
        if not text:
            continue
        if re.sub(r"\d+", "#", text) in drop:
            continue
        if re.fullmatch(r"\d{1,4}", text):  # bare page number
            continue
        if len(re.sub(r"[^A-Za-z0-9]", "", text)) < 2:  # stray footnote marks
            continue

        cap = captions_by_line.get(i)
        if cap is not None:
            flush()
            md.append(cap["placeholder"])
            skip_until = cap["end_line"]
            continue

        sizes = [c["size"] for c in ln.get("chars", []) if c.get("size")]
        size = max(sizes) if sizes else body_size
        is_heading = (
            len(text) < 110
            and len(text.split()) <= 14
            and not text.endswith((".", ",", ";", ":"))
            and (size > body_size * 1.12 or NUMBERED_HEADING_RE.match(text))
        )
        if is_heading:
            # the title block on page 1 is already the H1
            if title_norm and re.sub(r"\W+", "", text).lower() in title_norm:
                continue
            flush()
            level = 2 if size > body_size * 1.3 else 3
            md.append("#" * level + " " + text)
            continue

        if para:
            para[-1] = dehyphenate(para[-1], text)
        else:
            para.append(text)

        # A line that stops short of the column's right margin ends the
        # paragraph. This is the only reliable signal in justified text.
        edge = right_edges[i]
        if ln["x1"] < edge - 12:
            flush()
    flush()
    return md


# --------------------------------------------------------------------------
# driver
# --------------------------------------------------------------------------


@dataclass
class Asset:
    kind: str
    label: str
    caption: str
    page: int
    image: str
    csv: str | None = None


@dataclass
class DocResult:
    slug: str
    pages: int
    title: str = ""
    words: int = 0
    figures: list = field(default_factory=list)
    tables: list = field(default_factory=list)
    headings: list = field(default_factory=list)
    warnings: list = field(default_factory=list)


def guess_title(page):
    """Largest text on page 1, in reading order."""
    try:
        lines = page.extract_text_lines(layout=False, x_tolerance=X_TOL) or []
    except Exception:
        return ""
    scored = []
    for ln in lines[:40]:
        sizes = [c["size"] for c in ln.get("chars", []) if c.get("size")]
        if not sizes:
            continue
        scored.append((round(max(sizes), 1), ln))
    if not scored:
        return ""
    top = max(s for s, _ in scored)
    picked = [ln["text"].strip() for s, ln in scored if s >= top - 0.4][:4]
    title = " ".join(picked).strip()
    return re.sub(r"\s+", " ", title)[:300]


def extract_csv(page, box):
    try:
        crop = page.crop(box)
        tbl = crop.extract_table(
            {
                "vertical_strategy": "text",
                "horizontal_strategy": "text",
                "text_x_tolerance": 2,
            }
        )
    except Exception:
        return None
    if not tbl or len(tbl) < 2:
        return None
    cleaned = []
    for row in tbl:
        cells = [(c or "").replace("\n", " ").strip() for c in row]
        if any(cells):
            cleaned.append(cells)
    return cleaned or None


def process(pdf_path: Path) -> DocResult:
    slug = pdf_path.stem
    outdir = DOCS / slug
    figdir, tabdir = outdir / "figures", outdir / "tables"
    outdir.mkdir(parents=True, exist_ok=True)

    pdoc = pdfium.PdfDocument(str(pdf_path))
    result = DocResult(slug=slug, pages=len(pdoc))

    with pdfplumber.open(pdf_path) as pdf:
        result.title = guess_title(pdf.pages[0]) if pdf.pages else ""

        all_lines = [page_lines(p) for p in pdf.pages]
        drop = find_repeated_lines(all_lines)

        sizes = Counter()
        for p in pdf.pages[: min(12, len(pdf.pages))]:
            for c in p.chars:
                if c.get("size"):
                    sizes[round(c["size"], 1)] += 1
        body_size = sizes.most_common(1)[0][0] if sizes else 10.0

        # ---- pass 1: locate captions and crop their regions -------------
        fig_n = tab_n = 0
        used_labels = set()
        per_page_caps = []
        for pno, page in enumerate(pdf.pages):
            caps = collect_captions(all_lines[pno], pno)
            per_page_caps.append(caps)

        for pno, page in enumerate(pdf.pages):
            caps = per_page_caps[pno]
            if not caps:
                continue
            other_boxes = [c.box for c in caps]
            for cap in caps:
                if cap.label in used_labels:  # first occurrence wins
                    continue
                if cap.kind == "figure":
                    box = grow_figure_region(page, cap, other_boxes)
                else:
                    box = grow_table_region(page, cap)
                if box is None:
                    result.warnings.append(
                        f"could not locate region for {cap.label} on p.{pno + 1}"
                    )
                    cap.box_final = None
                    continue
                cap.box_final = clamp(box, page)

                if cap.kind == "figure":
                    fig_n += 1
                    figdir.mkdir(parents=True, exist_ok=True)
                    name = f"fig-{fig_n:02d}.png"
                    dest = figdir / name
                    rel = f"figures/{name}"
                else:
                    tab_n += 1
                    tabdir.mkdir(parents=True, exist_ok=True)
                    name = f"tab-{tab_n:02d}.png"
                    dest = tabdir / name
                    rel = f"tables/{name}"

                # render the crop
                bmp = pdoc[pno].render(scale=RENDER_SCALE)
                img = bmp.to_pil()
                ox, oy = pdf.pages[pno].bbox[0], pdf.pages[pno].bbox[1]
                x0, top, x1, bottom = cap.box_final
                px = (
                    int(max(0, (x0 - ox) * RENDER_SCALE)),
                    int(max(0, top * RENDER_SCALE)),
                    int(min(img.width, (x1 - ox) * RENDER_SCALE)),
                    int(min(img.height, bottom * RENDER_SCALE)),
                )
                if px[2] - px[0] < 20 or px[3] - px[1] < 20:
                    result.warnings.append(f"{cap.label}: crop too small, skipped")
                    continue
                img.crop(px).save(dest, optimize=True)

                asset = Asset(
                    kind=cap.kind,
                    label=cap.label,
                    caption=cap.text,
                    page=pno + 1,
                    image=rel,
                )

                if cap.kind == "table":
                    rows = extract_csv(page, cap.box_final)
                    if rows:
                        csv_name = name.replace(".png", ".csv")
                        with open(tabdir / csv_name, "w", newline="") as fh:
                            csv.writer(fh).writerows(rows)
                        asset.csv = f"tables/{csv_name}"
                    else:
                        result.warnings.append(f"{cap.label}: no machine-readable CSV")

                cap.asset = asset
                used_labels.add(cap.label)
                (result.figures if cap.kind == "figure" else result.tables).append(asset)

        # ---- pass 2: markdown body --------------------------------------
        body = [f"# {result.title or slug}", ""]
        for pno, page in enumerate(pdf.pages):
            lines = all_lines[pno]
            caps = per_page_caps[pno]
            cap_map = {}
            for cap in caps:
                asset = getattr(cap, "asset", None)
                if asset is None:
                    continue
                # how many lines the caption block covered
                end = cap.line_index + 1
                while end < len(lines) and obj_box(lines[end])[3] <= cap.box[3] + 0.5:
                    end += 1
                ph = [f"![{asset.label}]({asset.image})", "", f"***{asset.label}.*** {asset.caption}"]
                if asset.csv:
                    ph.append("")
                    ph.append(f"[Download data as CSV]({asset.csv})")
                cap_map[cap.line_index] = {
                    "placeholder": "\n\n".join(ph),
                    "end_line": end,
                }

            chunks = lines_to_markdown(
                lines, body_size, drop, cap_map, local_right_edges(lines), result.title
            )
            body.extend(chunks)

        text = "\n\n".join(b for b in body if b.strip())
        text = re.sub(r"\n{3,}", "\n\n", text)
        result.words = len(text.split())
        result.headings = [
            l.lstrip("# ").strip() for l in text.splitlines() if l.startswith("#") and not l.startswith("# ")
        ][:60]

    (outdir / f"{slug}.md").write_text(text)
    meta = asdict(result)
    meta["figures"] = [asdict(a) if not isinstance(a, dict) else a for a in result.figures]
    meta["tables"] = [asdict(a) if not isinstance(a, dict) else a for a in result.tables]
    (outdir / "meta.json").write_text(json.dumps(meta, indent=2))
    return result


def main():
    targets = sys.argv[1:]
    pdfs = sorted(DOCS.glob("*.pdf"))
    if targets:
        pdfs = [p for p in pdfs if p.stem in targets]
    for pdf in pdfs:
        try:
            r = process(pdf)
            print(
                f"{r.slug:32s} {r.pages:4d}p  {r.words:6d}w  "
                f"{len(r.figures):2d} fig  {len(r.tables):2d} tab"
                + (f"  ({len(r.warnings)} warn)" if r.warnings else "")
            )
        except Exception as e:  # keep going through the batch
            print(f"{pdf.stem:32s} FAILED: {type(e).__name__}: {e}")


if __name__ == "__main__":
    main()
