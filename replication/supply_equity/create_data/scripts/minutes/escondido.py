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

def ocr_pdfs(target_dir, year):
    os.chdir(os.path.join(target_dir, year))    
    file = os.path.join(target_dir, year, 'temp')
    if not path.exists(file): 
        os.mkdir('temp')
    os.system("parallel --tag -j 2 ocrmypdf '{}' 'temp/{}' ::: *.PDF")

def build_sorted_filelist(input_dir, dateform): 
    files = [f for f in listdir(input_dir) if isfile(join(input_dir, f))]
    dates = [f.replace("CC MIN ", "") for f in files]
    dates = [d.replace("PC MIN ", "") for d in dates]
    dates = [d.replace(" PC MIN", "") for d in dates]
    dates = [d.replace(".PDF", "") for d in dates]
    dates = [d.replace(".pdf", "") for d in dates]
    dates = [d.replace("SEPT", "SEP") for d in dates]
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
	pdf_merger.append(os.path.join(target_dir, input_file))
    with open(os.path.join(target_dir, output_file), 'wb') as fileobj:
        pdf_merger.write(fileobj)

if __name__ == '__main__':
    
    # city council minutes 
    dir_name = os.getenv("HOME") + "/Dropbox/cvra/data/minutes/escondido/city_council"
    
    # convert pdfs to machine-readable format
    for y in ['2010', '2011', '2012', '2013', '2014'] :
        ocr_pdfs(target_dir=dir_name, year=y)
    
    # merge pdfs by year 
    for y in ['2010', '2011', '2012', '2013', '2014'] :
        input_dir = os.path.join(dir_name, y, "temp")
        file_list = build_sorted_filelist(input_dir=input_dir, dateform="%b %d %Y")
        pdf_merge(output_file=os.path.join(dir_name, "minutes" + y + ".pdf"),
                input_files=[f[0] for f in file_list],
                target_dir=os.path.join(dir_name, y, "temp"))

    for y in ['2015', '2016', '2017', '2018', '2019'] :
        input_dir = os.path.join(dir_name, y)
        file_list = build_sorted_filelist(input_dir=input_dir, dateform="%b %d %Y")
        pdf_merge(output_file=os.path.join(dir_name, "minutes" + y + ".pdf"),
                input_files=[f[0] for f in file_list],
                target_dir=os.path.join(dir_name, y))

    # combine 2014 
    file_list = build_sorted_filelist(input_dir=os.path.join(dir_name, "2014"),
            dateform="%b %d %Y")
    input_files = [f[0] for f in file_list]
    input_files = filter(lambda x:'pdf' in x, input_files)
    pdf_merge(output_file=os.path.join(dir_name, "minutes2014_2.pdf"),
            input_files=input_files, target_dir=os.path.join(dir_name, "2014"))
    file_list = ["minutes2014.pdf", "minutes2014_2.pdf"]
    pdf_merge(output_file=os.path.join(dir_name, "minutes2014_full.pdf"), input_files=file_list, target_dir=os.path.join(dir_name))
    os.remove(os.path.join(dir_name, "minutes2014.pdf"))
    os.remove(os.path.join(dir_name, "minutes2014_2.pdf"))
    os.rename(os.path.join(dir_name, "minutes2014_full.pdf"),
            os.path.join(dir_name, "minutes2014.pdf"))


    # planning minutes
    dir_name = os.getenv("HOME") + "/Dropbox/cvra/data/minutes/escondido/planning"
  
    # convert pdfs to machine-readable format
    for y in ['2010', '2011', '2012', '2013'] :
        ocr_pdfs(target_dir=dir_name, year=y)

    # merge pdfs by year
    for y in ['2010', '2011', '2012', '2013'] :
        input_dir = os.path.join(dir_name, y, "temp")
        file_list = build_sorted_filelist(input_dir=input_dir, dateform="%b %d %Y")
        pdf_merge(output_file=os.path.join(dir_name, "minutes" + y + ".pdf"),
                input_files=[f[0] for f in file_list],
                target_dir=os.path.join(dir_name, y, "temp"))

    for y in ['2014', '2015'] :
        input_dir = os.path.join(dir_name, y)
        file_list = build_sorted_filelist(input_dir, dateform="%b %d %Y")
        pdf_merge(output_file=os.path.join(dir_name, "minutes" + y + ".pdf"),
                input_files=[f[0] for f in file_list],
                target_dir=os.path.join(dir_name, y))    

    for y in ['2016', '2017', '2018', '2019'] :
        input_dir = os.path.join(dir_name, y)
        file_list = build_sorted_filelist(input_dir, dateform="%m-%d-%Y")
        pdf_merge(output_file=os.path.join(dir_name, "minutes" + y + ".pdf"),
                input_files=[f[0] for f in file_list],
                target_dir=os.path.join(dir_name, y)) 

    # combine 2013
    file_list = build_sorted_filelist(input_dir=os.path.join(dir_name, "2013"),
            dateform="%b %d %Y")
    input_files = [f[0] for f in file_list]
    input_files = filter(lambda x:'pdf' in x, input_files)
    pdf_merge(output_file=os.path.join(dir_name, "minutes2013_2.pdf"),
            input_files=input_files, target_dir=os.path.join(dir_name, "2013"))
    file_list = ["minutes2013.pdf", "minutes2013_2.pdf"]
    pdf_merge(output_file=os.path.join(dir_name, "minutes2013_full.pdf"), input_files=file_list,   target_dir=os.path.join(dir_name))
    os.remove(os.path.join(dir_name, "minutes2013.pdf"))
    os.remove(os.path.join(dir_name, "minutes2013_2.pdf"))
    os.rename(os.path.join(dir_name, "minutes2013_full.pdf"),
            os.path.join(dir_name, "minutes2013.pdf"))
