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
import PyPDF2

def fetch_content(url):
    response = requests.post(url)
    return response.text

def build_links(page_text):
    soup = BeautifulSoup(page_text)
    links = soup.find_all(href=re.compile("Minutes"))
    urls = []
    for link in links:
        if link.has_attr('href'):
            urls.append(link['href'])
    urls = list(dict.fromkeys(urls))    
    return urls

def download_pdf(target_dir, urls):
    count = 1
    for u in urls:
        time.sleep(base_wait + random.randint(0, 5))
        print('Fetching: %s time: %s' % (count, datetime.now()))
        file_name = "minutes" + str(count) + ".pdf" 
        r = requests.get(u)
        with open(os.path.join(target_dir, file_name), 'wb') as code:
            code.write(r.content)
        count = count + 1

def get_council_minutes(target_dir):
    keeps = []
    for i in range(1, 1609):
        pdfFileObj = open(os.path.join(target_dir, 'minutes' + str(i) + '.pdf'))
        pdfReader = PyPDF2.PdfFileReader(pdfFileObj)
        pageObj = pdfReader.getPage(0)
        head = pageObj.extractText()
        head = head.replace("\n", "")
        tofind = 'city council'
        if tofind.upper() in head:
            keeps.append(i) 
    return(keeps)

def generate_pdf(keeplist, target_dir):
    files = [os.path.join(target_dir, "minutes" + str(f) + ".pdf") for f in filelist] 
    pdf_merger = PdfFileMerger(strict=False)
    file_handles = []
    for input_file in files:
        print(input_file)
        pdf_merger.append(input_file) 
    with open(os.path.join(target_dir, "city_council"), 'wb') as fileobj:
        pdf_merger.write(fileobj)

if __name__ == '__main__':
    base_wait = 10 # in seconds
    url = "https://glendale.granicus.com/ViewPublisher.php?view_id=47"
    target_dir = os.getenv("HOME") + "/Dropbox/cvra/data/minutes/glendale/city_council/" 
    page_text = fetch_content(url)
    links = build_links(page_text)
    download_pdf(target_dir=target_dir, urls=links)
    keeplist = get_council_minutes(target_dir)
    generate_pdf(keeplist, target_dir)

