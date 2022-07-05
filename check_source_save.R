# This script checks to see if there are sentinel acquisition updates and, if there are, downloads the new files for the EPSCoR-Blooms lakes.

library(rvest)
library(stringr)
library(sf)
library(rgdal)
library(xml2)
library(tidyverse)
library(lubridate)

##  CHECK WEB FOR NEW DATA ----

#point to website
sntl_ap_web = 'https://sentinel.esa.int/web/sentinel/missions/sentinel-2/acquisition-plans'

#point to upstream download from website
dlpath = 'https://sentinel.esa.int/'

tmp = 'tmp/'
if(!dir.exists(tmp)){dir.create(tmp)}

#save as html page 
pg <- read_html(sntl_ap_web)

#get the section we're interested in
list2a <- html_nodes(pg, xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "sentinel-2a", " " ))] | //h4')[1]
list2b <- html_nodes(pg, xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "sentinel-2b", " " ))] | //h5')[1]

#save to list, splitting at href
list2a <- as.list(strsplit(as.character(list2a), 'href'))[[1]]
list2b <- as.list(strsplit(as.character(list2b), 'href'))[[1]]

# select the two with kmls
list2a <- list2a[grepl('kml', list2a)]
list2b <- list2b[grepl('kml', list2b)]

# get the filenames
list2a_files <- str_extract(list2a,'documents...*.kml')
list2b_files <- str_extract(list2b,'documents...*.kml')

# load current filenames
existing_2a_files <- readRDS(file.path('checkdates/list2a_files.RDS'))
existing_2b_files <- readRDS(file.path('checkdates/list2b_files.RDS'))

# see if there are new files
download_2a <- list2a_files[!(list2a_files %in% existing_2a_files)]
download_2b <- list2b_files[!(list2b_files %in% existing_2b_files)]

#if there are new files, process them in the download and extract script
if(length(download_2a>0)|length(download_2b)>0){
  source('download_extract.R')
}

# save the file names for next check
saveRDS(list2a_files, file.path('checkdates/list2a_files.RDS'))
saveRDS(list2b_files, file.path('checkdates/list2b_files.RDS'))

# move old files to archive if new file available
if(length(download_2a>0)|length(download_2b)>0){
  comp_files <- list.files(pattern = 'future')
  move_file <- comp_files[!(grepl(Sys.Date(), comp_files))]
  file.copy(move_file, 
            to = paste0('archive/', move_file))
  file.remove(move_file)
}
