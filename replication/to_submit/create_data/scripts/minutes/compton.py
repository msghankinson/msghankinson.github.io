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

def fetch_content(url, year, headers):
    response = requests.post(url + year + ".asp", headers=headers).text 
    return response

def build_links(page_text):
    soup = BeautifulSoup(page_text)
    links = soup.findAll('a', text=re.compile("city council agenda",
        flags=re.IGNORECASE))
    links = [l.get('href') for l in links]
    links = filter(lambda x:'civicax' in x, links)
    return links  

def download_pdf(target_dir, links, year):
    base = "http://www.comptoncity.org/"
    for l in links:
        time.sleep(base_wait + random.randint(0, 5))
        print('Fetching: %s time: %s' % (l, datetime.now()))
        file_name = "minutes" + l.split('=')[1] + ".pdf"
        r = requests.get(base + l)
        with open(os.path.join(target_dir, "city_council", "20" + year, file_name), 'wb') as code:
            code.write(r.content)
            
def build_sorted_filelist(target_dir, mintype, year):
    directory = os.path.join(target_dir, mintype, '20' + year)
    files = [f for f in listdir(directory) if isfile(join(directory, f))]
    files.sort()
    return files

def pdf_merge(output_file, input_files, target_dir, mintype, year):
    pdf_merger = PdfFileMerger(strict=False)
    file_handles = []
    for input_file in input_files:
        print(input_file)
        pdf_merger.append(os.path.join(target_dir, mintype, '20' + year, input_file))
    with open(os.path.join(target_dir, mintype, output_file), 'wb') as fileobj:
        pdf_merger.write(fileobj)

def main():
    sys.setrecursionlimit(3000)
    headers = {
        'user-agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/56.0.2924.87 Safari/537.36',
        }
    url = "http://www.comptoncity.org/officials/clerk/agendas/archives/cca/acca"
    #years = ['10', '11', '12', '13', '14', '15', '16', '17', '18']
    years = ['17', '18']
    base_wait = 10 # in seconds
    dir_name = os.getenv("HOME") + "/Dropbox/cvra/data/minutes/compton"

    # for city council 
    for y in years:
        page_text = fetch_content(url, y, headers)
        links = build_links(page_text=page_text)
        # download_pdf(target_dir=dir_name, links=links, year=y)
        downloaded_files = build_sorted_filelist(target_dir=dir_name,
        mintype="city_council", year=y)
        pdf_merge(output_file="minutes20" + y + ".pdf",
        input_files=downloaded_files, target_dir=dir_name, mintype="city_council",
        year=y) 

    # for planning
    years = ['10', '11', '12', '13', '14', '15', '16', '17']
    for y in years: 
        downloaded_files = build_sorted_filelist(target_dir=dir_name, mintype="planning", year=y)
        pdf_merge(output_file="minutes20" + y + ".pdf", input_files=downloaded_files, target_dir=dir_name, mintype="planning", year=y)
