#!/usr/bin/env python
"""Coverage check for pdf_extract.py.

Independently scans each PDF's text for caption labels and compares that list
against what actually got extracted, so silent misses show up as a number
rather than as a figure nobody notices is absent.

    .venv/bin/python tools/audit.py
"""

from __future__ import annotations

import json
import re
import sys
from pathlib import Path

import pdfplumber

sys.path.insert(0, str(Path(__file__).resolve().parent))
from pdf_extract import CAPTION_RE, DOCS, X_TOL  # noqa: E402


def labels_in_pdf(pdf_path):
    """Every caption label the text layer contains, by the same rules."""
    found = {}
    with pdfplumber.open(pdf_path) as pdf:
        for pno, page in enumerate(pdf.pages):
            for line in (page.extract_text(x_tolerance=X_TOL) or "").split("\n"):
                m = CAPTION_RE.match(line)
                if not m:
                    continue
                rest = m.group("rest").strip()
                if rest and re.match(
                    r"^(shows?|displays?|reports?|presents?|plots?|and|is|are|"
                    r"in|of|we|the|above|below|here)\b",
                    rest,
                    re.I,
                ):
                    continue
                if rest and re.search(r"(\s[-+(]?\d[\d.,%)]*){2,}\s*$", rest):
                    continue
                label = re.sub(
                    r"\s+",
                    " ",
                    f"{m.group('kind').strip()} {m.group('num').strip()}".replace(
                        "Fig.", "Figure"
                    ),
                )
                found.setdefault(label, pno + 1)
    return found


def main():
    rows, total_exp, total_got = [], 0, 0
    for pdf in sorted(DOCS.glob("*.pdf")):
        meta_path = DOCS / pdf.stem / "meta.json"
        if not meta_path.exists():
            continue
        meta = json.loads(meta_path.read_text())
        got = {a["label"] for a in meta["figures"] + meta["tables"]}
        expected = labels_in_pdf(pdf)
        missing = {k: v for k, v in expected.items() if k not in got}
        extra = got - set(expected)
        total_exp += len(expected)
        total_got += len(got & set(expected))
        rows.append((pdf.stem, len(expected), len(got), missing, extra))

    for slug, exp, got, missing, extra in rows:
        pct = 100.0 * (got if exp == 0 else len(set()) or (exp - len(missing))) / exp if exp else 100.0
        flag = "" if not missing else "  <-"
        print(f"{slug:30s} {exp - len(missing):3d}/{exp:3d} captions  {pct:5.1f}%{flag}")
        for label, page in sorted(missing.items())[:12]:
            print(f"      MISSING {label} (p.{page})")
        if extra:
            print(f"      extra: {sorted(extra)[:8]}")
    print(f"\nTOTAL {total_got}/{total_exp} = {100.0 * total_got / max(total_exp, 1):.1f}%")


if __name__ == "__main__":
    main()
