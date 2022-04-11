from datetime import datetime
import os
import random
import time
import re
import BeautifulSoup
import requests
import urllib2, cookielib
import shutil
import sys
from PyPDF2 import PdfFileMerger
from os import listdir, path
from os.path import isfile, join

def build_sorted_filelist(target_dir, mintype, year):
    directory = os.path.join(target_dir, mintype, year)
    files = [f for f in listdir(directory) if isfile(join(directory, f))]
    files.sort()
    return files

def pdf_merge(output_file, input_files, target_dir):
    pdf_merger = PdfFileMerger(strict=False)
    file_handles = []
    for input_file in input_files:
        print(input_file)
	    pdf_merger.append(os.path.join(target_dir, input_file))
    with open(os.path.join(target_dir, output_file), 'wb') as fileobj:
        pdf_merger.write(fileobj)

if __name__ == '__main__':
    
    dir_name = os.getenv("HOME") + "/Dropbox/cvra/data/minutes/anaheim/"
    
    # merge pdfs by year 
    
    # city council
    for y in ['2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018'] :
        file_list = build_sorted_filelist(target_dir=dir_name, mintype="city_council", year=y)
        pdf_merge(output_file=os.path.join(dir_name, "city_council", "minutes" + y + ".pdf"),
                input_files=file_list, target_dir=os.path.join(dir_name, "city_council", y))

    # planning
    for y in ['2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017'] :
        file_list = build_sorted_filelist(target_dir=dir_name, mintype="planning", year=y)
        pdf_merge(output_file=os.path.join(dir_name, "planning", "minutes" + y + ".pdf"),
                input_files=file_list, target_dir=os.path.join(dir_name, "planning", y))

