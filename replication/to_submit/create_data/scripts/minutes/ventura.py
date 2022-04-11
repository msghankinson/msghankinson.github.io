from datetime import datetime
import os
import random
import time
import re
from bs4 import BeautifulSoup, SoupStrainer
import requests
import urllib2, cookielib
import shutil
import sys
from PyPDF2 import PdfFileMerger
from os import listdir, path
from os.path import isfile, join

def fetch_content(url, years, catid):
    response = "".join([requests.post(url, data={'year': y, 'catID': catid}).text for y in years])
    return response

def build_links(page_text, label):
    soup = BeautifulSoup(page_text)
    links = soup.find_all(href=re.compile(label))
    urls = []
    for link in links:
        if link.has_attr('href'):
            urls.append("https://www.cityofventura.ca.gov" + link['href'])
    urls = list(dict.fromkeys(urls))    
    return urls

def download_pdf(target_dir, urls):
    os.mkdir(os.path.join(target_dir, "temp"))
    for u in urls:
        time.sleep(base_wait + random.randint(0, 5))
        print('Fetching: %s time: %s' % (u, datetime.now()))
        file_name = "minutes" + u.split('/')[-1] + ".pdf" 
        r = requests.get(u)
        with open(os.path.join(target_dir, "temp", file_name), 'wb') as code:
            code.write(r.content)

def ocr_pdfs(target_dir, year):
    os.chdir(os.path.join(target_dir, year))
    file = os.path.join(target_dir, year, 'temp')
    if not path.exists(file):
        os.mkdir('temp')
    os.system("parallel --tag -j 2 ocrmypdf '{}' 'temp/{}' ::: *.pdf")

def build_sorted_filelist(target_dir):
    temp = os.path.join(target_dir, "temp")
    files = [f for f in listdir(temp) if isfile(join(temp, f))]
    dates = [datetime(year=int(f.split('_')[1][4:8]), month=int(f.split('_')[1][0:2]), day=int(f.split('_')[1][2:4])) for f in files]
    files_dict = {}
    files_dates = zip(files, dates)
    for f, d in files_dates:
        files_dict[f] = d
    files_chron = sorted(files_dict.items(), key=lambda x: x[1])
    return files_chron

def build_sorted_filelist2(input_dir, dateform):
    files = [f for f in listdir(input_dir) if isfile(join(input_dir, f))]
    dates = [f.replace(".pdf", "") for f in files]
    dates = [datetime.strptime(d, dateform) for d in dates]
    files_dict = {}
    files_dates = zip(files, dates)
    for f, d in files_dates:
        files_dict[f] = d
    files_chron = sorted(files_dict.items(), key=lambda x: x[1])
    return files_chron

def pdf_merge(output_file, input_files, target_dir):
    pdf_merger = PdfFileMerger(strict=False)
    file_handles = []
    for input_file in input_files:
        print(input_file)
        # pdf_merger.append(os.path.join(target_dir, "temp", input_file)) # for
        # city council 
        pdf_merger.append(os.path.join(target_dir, input_file)) # for planning
    with open(os.path.join(target_dir, output_file), 'wb') as fileobj:
        pdf_merger.write(fileobj)

def files_combine_by_year(filelist, target_dir):
    years = set([f[1].year for f in filelist])
    for y in years:
        files_to_combine = [f[0] for f in filelist if f[1].year==y]
        pdf_merge(output_file=os.path.join(target_dir, "minutes" + str(y)) + ".pdf", input_files=files_to_combine, target_dir=target_dir)


if __name__ == '__main__':
    url = "https://www.cityofventura.ca.gov/AgendaCenter/UpdateCategoryList"
    years = ['2019', '2018', '2017', '2016', '2015', '2014', '2013']
    base_wait = 10 # in seconds
    dir_name = os.getenv("HOME") + "/Dropbox/cvra/data/minutes/ventura"

    # council minutes
    target_dir = os.path.join(dir_name, "city_council")
    page_text = fetch_content(url=url, years=years, catid=25)
    urls = build_links(page_text=page_text, label="Minutes")
    #download_pdf(target_dir=target_dir, urls=urls)
    #ocr here
    downloaded_files = build_sorted_filelist(target_dir=target_dir)
    files_combine_by_year(filelist=downloaded_files, target_dir=target_dir)

    # planning minutes
    # first download minutes for 2017-18 (others done manually) 
    years = ['2017', '2018']
    for y in years:
        target_dir = os.path.join(dir_name, "planning")
        page_text = fetch_content(url=url, years=years, catid=19)
        urls = build_links(page_text=page_text, label="Minutes")
        download_pdf(target_dir=os.path.join(target_dir, y), urls=urls)
    # manually put these into separate folders and rename to match other years

    # ocr pdfs
    years = ['2018', '2017', '2016', '2015', '2014', '2013']
    for y in years:
        ocr_pdfs(target_dir=target_dir, year=y)
    # visually inspect and manually move ocr pdfs into main folder, replace

    # combine files by year
    for y in years :
        input_dir = os.path.join(target_dir, y)
        file_list = build_sorted_filelist2(input_dir, dateform="%b-%d-%y")
        pdf_merge(output_file=os.path.join(target_dir, "minutes" + y + ".pdf"),
                input_files=[f[0] for f in file_list],
                target_dir=os.path.join(target_dir, y))
