# script for auto compile of Sentinel 2 overpass for EPSCoRBlooms project

library(rvest)
library(stringr)
library(raster)
library(sf)
library(rgdal)
library(stars)
library(xml2)
library(tidyverse)

# point to directories
lakedir = 'shapefiles/'

# LAKE FILES PROCESSING ----
me_lakes=read_sf(file.path(lakedir, 'NHDLakeShapefiles.gdb'), 'me_lakes')
nh_lakes=read_sf(file.path(lakedir, 'NHDLakeShapefiles.gdb'), 'nh_lakes')
ri_lakes=read_sf(file.path(lakedir, 'NHDLakeShapefiles.gdb'), 'ri_lakes')
sc_lakes=read_sf(file.path(lakedir, 'NHDLakeShapefiles.gdb'), 'sc_lakes')

epscor_lakes = rbind(me_lakes, nh_lakes) %>% 
  rbind(., ri_lakes) %>% 
  rbind(., sc_lakes)

epscor_lakes = st_transform(epscor_lakes,crs = 'EPSG:4326')

epscor_lakes_meta = st_drop_geometry(epscor_lakes) %>% 
  select('OBJECTID', 'Permanent_', 'GNIS_Name')

# SENTINEL DOWNLOAD AND EXTRACT ----

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

#get the filenames
list2a_files <- str_extract(list2a,'documents...*.kml')
list2b_files <- str_extract(list2b,'documents...*.kml')

#create a composite sf layer and grab all the extended metadata in the kml file

for(i in 1:length(list2a_files)){
  download.file(url = paste0(dlpath, list2a_files[i]),
                destfile = file.path(tmp, paste0('2a_', i, '.kml')))
  layers <- ogrListLayers(file.path(tmp, paste0('2a_', i, '.kml')))
  
  #deal with multiple kml layers
  for(j in 1:length(layers)){
      kml_file = read_sf(file.path(tmp, paste0('2a_', i, '.kml')),layer = layers[j])
      #clip to extent
      kml_clip = st_crop(kml_file, 
                         st_bbox(epscor_lakes))
      if(j == 1){
        kml_comp = kml_clip
      } else {
        kml_comp <- rbind(kml_comp, kml_clip)
      }
  }
  
  ids = unique(kml_comp$Name)
  
  xml_collate = NULL
  
  #grab extended data from file - this is a little laborious, but works. Someday if libkml is avail for Windows, we won't need this step.
  for(j in 1:length(layers)){
    xml_file = read_xml(file.path(tmp, paste0('2a_', i, '.kml')),layer = layers[j])
    listnodes = xml_find_all(xml_file,".//d1:ExtendedData")
    for(k in 1:length(listnodes)){
      if(k == 1){
      xml_names <- as_tibble(unlist(xml_attrs(xml_children(listnodes[k]))))
      xml_names <- xml_names$value
      }
      #get values; bind with names
      xml_values <- xml_text(xml_children(listnodes[k]))
      xml_extract <- as_tibble(cbind(xml_names, xml_values)) %>% 
        pivot_wider(names_from = xml_names, values_from = xml_values)
      if(k == 1) {
        xml_collate = merge(xml_collate, xml_extract)
      }
      if(xml_extract$ID %in% ids){
      xml_collate = full_join(xml_extract, xml_collate)
      }
    }
  }
  
  #get the intersections
  sent_inter <- st_intersection(epscor_lakes, kml_comp)
  sent_inter <- sent_inter %>% 
    rename(ID = Name)
  
  #drop geo and add metadata
  sent_inter_meta <- left_join(sent_inter, xml_collate) %>% 
    st_drop_geometry() %>% 
    mutate(acqusition_date = format(as.POSIXct(ObservationTimeStart), '%Y-%m-%d')) %>% 
    select(ID, acqusition_date, OBJECTID) %>% 
    left_join(., epscor_lakes_meta)
  
  #join
  if(i == 1){
    sent2a_dates <- sent_inter_meta
  } else {
    sent2a_dates <- full_join(sent2a_dates, sent_inter_meta)
  }
    
}

#create a composite sf layer and grab all the extended metadata in the kml file

for(i in 1:length(list2b_files)){
  download.file(url = paste0(dlpath, list2b_files[i]),
                destfile = file.path(tmp, paste0('2b_', i, '.kml')))
  layers <- ogrListLayers(file.path(tmp, paste0('2b_', i, '.kml')))
  
  #deal with multiple kml layers
  for(j in 1:length(layers)){
    kml_file = read_sf(file.path(tmp, paste0('2b_', i, '.kml')),layer = layers[j])
    #clip to extent
    kml_clip = st_crop(kml_file, 
                       st_bbox(epscor_lakes))
    if(j == 1){
      kml_comp = kml_clip
    } else {
      kml_comp <- rbind(kml_comp, kml_clip)
    }
  }
  
  ids = unique(kml_comp$Name)
  
  xml_collate = NULL
  
  #grab extended data from file - this is a little laborious, but works. Someday if libkml is avail for Windows, we won't need this step.
  for(j in 1:length(layers)){
    xml_file = read_xml(file.path(tmp, paste0('2b_', i, '.kml')),layer = layers[j])
    listnodes = xml_find_all(xml_file,".//d1:ExtendedData")
    for(k in 1:length(listnodes)){
      if(k == 1){
        xml_names <- as_tibble(unlist(xml_attrs(xml_children(listnodes[k]))))
        xml_names <- xml_names$value
      }
      #get values; bind with names
      xml_values <- xml_text(xml_children(listnodes[k]))
      xml_extract <- as_tibble(cbind(xml_names, xml_values)) %>% 
        pivot_wider(names_from = xml_names, values_from = xml_values)
      if(k == 1) {
        xml_collate = merge(xml_collate, xml_extract)
      }
      if(xml_extract$ID %in% ids){
        xml_collate = full_join(xml_extract, xml_collate)
      }
    }
  }
  
  #get the intersections
  sent_inter <- st_intersection(epscor_lakes, kml_comp)
  sent_inter <- sent_inter %>% 
    rename(ID = Name)
  
  #drop geo and add metadata
  sent_inter_meta <- left_join(sent_inter, xml_collate) %>% 
    st_drop_geometry() %>% 
    mutate(acqusition_date = format(as.POSIXct(ObservationTimeStart), '%Y-%m-%d')) %>% 
    select(ID, acqusition_date, OBJECTID) %>% 
    left_join(., epscor_lakes_meta)
  
  #join
  if(i == 1){
    sent2b_dates <- sent_inter_meta
  } else {
    sent2b_dates <- full_join(sent2b_dates, sent_inter_meta)
  }
  
}

#save last files names


unlink(tmp, recursive = T)
