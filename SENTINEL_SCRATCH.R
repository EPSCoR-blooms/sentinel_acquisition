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
      if(j == 1){
        kml_comp = kml_file
      } else {
        kml_comp <- rbind(kml_comp, kml_file)
      }
  }

  #grab extended data from file
  for(j in 1:length(layers)){
    ext_data <- data.frame(matrix(ncol = 10))
    xml_file = read_xml(file.path(tmp, paste0('2a_', i, '.kml')),layer = layers[j])
    listnodes = xml_find_all(xml_file,".//d1:ExtendedData")
    for(k in 1:length(listnodes)){
      if(k == 1){
      xml_names <- as_tibble(unlist(xml_attrs(xml_children(listnodes[k]))))
      xml_names <- xml_names$value
      }
      xml_values <- xml_text(xml_children(listnodes[k]))
      xml_extract <- as_tibble(cbind(xml_names, xml_values)) %>% 
        pivot_wider(names_from = xml_names, values_from = xml_values)
      if(k == 1) {
        xml_collate <- xml_extract
      } else {
        xml_collate <- full_join(xml_collate, xml_extract)
      }
    }
  }
  
  
}



NEED TO GET EXTENDED DATA THAT HAS THE INFO!

# folder_path <- "//Users//myname//Downloads//cb_2014_39_tract_500k"
# 
# original_kml_file_name <- "cb_2014_39_tract_500k.kml"
# 
# new_kml_file_name <- "kml_fix.kml"
# 
# #Find these attributes in the <th></th> rows by looking at the original KML in a text editor
# 
# attributes <- c(
#   
#   "STATEFP",
#   
#   "COUNTYFP",
#   
#   "TRACTCE",
#   
#   "AFFGEOID",
#   
#   "GEOID",
#   
#   "NAME",
#   
#   "LSAD",
#   
#   "ALAND",
#   
#   "AWATER"
#   
# )
# 
# #####################
# 
# setwd(folder_path)
# 
# txt <- readLines(original_kml_file_name)
# 
# txt.df <- data.frame("kml_txt"=txt)
# 
# txt.df$row_num <- as.numeric(rownames(txt.df))
# 
# attributes_tags <- unlist(lapply(attributes, function(x) paste0("<th>", x, "</th>")))
# 
# matches <- txt.df[txt.df$kml_txt %in% attributes_tags, ]
# 
# matches$attribute_value_rows <- matches$row_num + 1
# 
# #Drop column before merge
# 
# matches$row_num <- NULL
# 
# vals <- merge(x=txt.df, y=matches, by.x='row_num', by.y='attribute_value_rows')
# 
# vals$kml_txt.x <- gsub("<td>|</td>", "", vals$kml_txt.x)
# 
# vals$group_num <- unlist(lapply(1:nrow(vals[vals$kml_txt.y == paste0('<th>', attributes[1], '</th>'), ]),
#                                 
#                                 function(i) rep(i, length(attributes))
#                                 
# ))
# 
# val_list <- data.frame("dlm"=unique(ave(vals$kml_txt.x, vals$group_num, FUN = function(x)
#   
#   paste0('<name>', paste(x,collapse = "|"),'</name>')))
#   
# )
# 
# val_list$row_num <- as.numeric(rownames(val_list))
# 
# placemark_rows <- txt.df[substr(txt.df$kml_txt,1,6) == '<Place', ]
# 
# placemark_rows$name_row <- placemark_rows$row_num + 1
# 
# vals_with_target_row <- data.frame(cbind(placemark_rows$name_row, as.character(val_list$dlm)))
# 
# colnames(vals_with_target_row) <- c("name_row", "dlm")
# 
# txt.df.temp <- merge(x=txt.df, y=vals_with_target_row, by.x='row_num', by.y='name_row', all.x=T)
# 
# txt.df.temp$final <- ifelse(is.na(txt.df.temp$dlm) == T, as.character(txt.df.temp$kml_txt), as.character(txt.df.temp$dlm))