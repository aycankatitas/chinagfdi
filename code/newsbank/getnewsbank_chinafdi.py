#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Newsbank_scraping 

Version 1

Downloading files in 50 company increments to catch mistakes
"""


import csv
import re
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from webdriver_manager.chrome import ChromeDriverManager

import time
import os
import shutil

# rootpath is the directory for temporarily storing downloaded files. 
rootpath = '/Users/aycankatitas/Desktop/Webscrape/'
# filepath stores the downloaded files
filepath = '/Users/aycankatitas/Desktop/Webscrape/chinagfdiright/news1220'
# Company info csv file lies in
infopath = '/Users/aycankatitas/Desktop/Webscrape/chinagfdiright/scandoubles.csv'
# Log csv directory
logpath = '/Users/aycankatitas/Desktop/Webscrape/chinagfdiright/process20.csv'

# Check how long it takes to start the downloading after you click the download button.
# Then change the "sleeptime" accordingly
sleeptime = 25


# A function that holds up the webdriver until the downloading finishes
def downloads_done(rootpath):
    for i in os.listdir(rootpath):
        if ".crdownload" in i:
            time.sleep(1)
            downloads_done()

# Check how many pages the results spread          
def getpagenum(resultnum):
    # Input is an integer 
    if resultnum%20 !=0 :
        totalpage = resultnum//20 +1
    else:
        totalpage = resultnum//20
    return totalpage

# Get the time period according to the date column in the csv
# changed the original code here since my computer read - as / in dates     
# searching for the two months after announcement
def daterange(string):
    # Input is a string like "2009-09-01"
    year = string.split('-')[0]
    month = string.split('-')[1]
    
    monthlist = ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
    
    startmonth = int(month)
    if startmonth <= 11:
        endmonth = startmonth + 1
        endyear = year
    else:
        endmonth = 1
        endyear = str(int(year)+1)
    
    drange = monthlist[startmonth-1]+" "+year+" - "+monthlist[endmonth-1]+" "+endyear
    return drange

# Get the first page url of a company searching and 
# a part of url that is needed to generate the sequential pages
def genmainurl(line):
    # Input is each line in the csv file
    p1_1 = "https://infoweb.newsbank.com/apps/news/results?"
    p1_2 = "sort=YMD_date%3AD&p=WORLDNEWS&t=continent%3ANorth%2BAmerica%21North%2BAmerica/country%3AUSA%21USA/state%3A"
    p2 = "%21USA%2B-%2B"
    p3 = "&maxresults=20&f=advanced&val-base-0=%22"
    p4 = "%22&fld-base-0=alltext&bln-base-1=and&val-base-1="
    p5 = "&fld-base-1=YMD_date"
    
    stateshort = line[5].split('(')[1].split(')')[0]
    state = line[4]
    companyname = line[1]
    drange = daterange(line[0])
    
    mainurl = p1_1 + p1_2 + stateshort + p2 + state + p3 + companyname + p4 + drange + p5
    suburl = p1_2 + stateshort + p2 + state + p3 + companyname + p4 + drange + p5
    return mainurl,suburl

# Download the data
def download(driver,sleeptime,rootpath,filepath,company,state,drange,page):
    
    wait = WebDriverWait(driver,10)
    
    button0 = wait.until(EC.element_to_be_clickable((By.CSS_SELECTOR,'#main-content > div.search-hits__selectall--wrapper > div > input')))
    button0.click()
    
    button1 = wait.until(EC.element_to_be_clickable((By.CSS_SELECTOR,'#main-content > div.panel-pane.pane-panels-mini.pane-action-bar-search-results > div.actions-bar__item.actions-bar__item--download > button')))
    button1.click()
    
    button2 = wait.until(EC.element_to_be_clickable((By.CSS_SELECTOR,'body > div.gnus-multidoc-container > div > footer > div > button')))
    button2.click()
    
    time.sleep(sleeptime)
    downloads_done(rootpath)
    
    # For doubles I add drange to the downloaded file name
    # Downloaded files do not have a informative name. Need to rename it and move it to the right directory
    filename = max([f for f in os.listdir(rootpath)], key=lambda xa: os.path.getctime(os.path.join(rootpath,xa)))
    newname = company.replace('.','').replace(',','') +'_'+ state +'_'+ str(page) +  drange +  '.pdf'
    shutil.move(os.path.join(rootpath,filename),os.path.join(filepath,newname))
    print("Get "+newname)
    
    driver.quit()
    

# Get the sequential pages if any
def gensequrl(suburl,totalpage):
    sequrls = []
    
    for i in range(0,totalpage):
        newurl = "https://infoweb.newsbank.com/apps/news/results?page=" + str(i) + "&" + suburl
        sequrls.append(newurl)
        
    return sequrls

# Set up a csv file to record the process
mcsvf = open(logpath,'w',newline='')
writer2 = csv.writer(mcsvf)


# Read in the company info csv file
namef = open(infopath,'r')
reader = csv.reader(namef)

namedata = []
for item in reader:
    if reader.line_num == 1:
        continue
    namedata.append(item)

#line=namedata[0]
# Start the main process    
for line in namedata:
    print(line)
    
    # Get the state abbreviations 
    stateshort = line[5].split('(')[1].split(')')[0]
    # Comapny name
    company = line[1]
    
    mainurl,suburl = genmainurl(line)
    
    options = webdriver.ChromeOptions()
    prefs = {'profile.default_content_settings.popups': 0,'download.default_directory': rootpath}
    options.add_experimental_option('prefs', prefs)   
    
    # Open the first searched page for a company
    path_to_chromedriver = '/Users/aycankatitas/Desktop/Webscrape/chromedriver' # change path as needed
    #driver = webdriver.Chrome(ChromeDriverManager().install(),options=options)
    driver = webdriver.Chrome(executable_path = path_to_chromedriver,options=options)
    driver.get(mainurl)

    
    try:
        # Check if there is any result. If yes, this part of process will continue
        resultnum = driver.find_element_by_xpath('/html/body/div[2]/div[3]/section[3]/div[1]/div').text.split()[0].replace(',','')
        rn = int(resultnum)
        totalpage = getpagenum(rn)
        print("There are "+str(totalpage)+ " pages for " + company + " at "+ stateshort)
        row = "There are "+str(totalpage)+ " pages for " + company + " at "+ stateshort
        writer2.writerow(row)
        
        drange=daterange(line[0])
        
        download(driver,sleeptime,rootpath,filepath,company,stateshort,drange,0)
        
        if totalpage >1:
            print("There are sequential pages")
            sequrls = gensequrl(suburl,totalpage)
            for j in range(1,totalpage):
                print(j)
                print("Download page "+str(j+1))
                suburl = sequrls[j]
                subdriver = webdriver.Chrome(executable_path = path_to_chromedriver,options=options)
                subdriver.get(suburl)
                download(subdriver,sleeptime,rootpath,filepath,company,stateshort,drange,j)
        else:
            continue
    except:
        print("No searching results for " + company + " at " +stateshort)
        row1 = "No searching results for " + company + " at " +stateshort
        writer2.writerow(row1)
        driver.quit()
        
    
mcsvf.close()   
  
    

    
