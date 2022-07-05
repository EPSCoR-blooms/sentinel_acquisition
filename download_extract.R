# script for auto compile of Sentinel 2 overpass for EPSCoRBlooms project

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
  select('OBJECTID', 'Permanent_', 'GNIS_Name') %>% 
  mutate(LakeName = case_when(GNIS_Name == 'The Basin' ~ 'Lake Auburn',
                              GNIS_Name == 'Custer Pond' ~ 'Sabattus Pond',
                              GNIS_Name == 'Sunapee Lake' ~ 'Lake Sunapee',
                              GNIS_Name == 'Wateree Lake' ~ 'Lake Wateree',
                              TRUE ~ GNIS_Name),
         LakeID = case_when(LakeName == 'Lake Auburn' ~ 'AUB',
                            LakeName == 'Great Pond' ~ 'GRT',
                            LakeName == 'Long Pond' ~ 'LNG',
                            LakeName == 'Panther Pond' ~ 'PAN',
                            LakeName == 'Sabattus Pond' ~ 'SAB',
                            LakeName == 'China Lake' ~ 'CHN',
                            LakeName == 'Lake Sunapee' ~ 'SUN',
                            LakeName == 'Indian Lake' ~ 'IND',
                            LakeName == 'Yawgoo Pond' ~ 'YAW',
                            LakeName == 'Barber Pond' ~ 'BAR',
                            LakeName == 'Lake Murray' ~ 'MUR',
                            LakeName == 'Lake Wateree' ~ 'WAT',
                            TRUE ~ NA_character_))

# SENTINEL DOWNLOAD AND EXTRACT ----

# read in current list
current <- read.csv(list.files(pattern = 'future'))

#create a composite sf layer and grab all the extended metadata in the kml file
if(length(download_2a)>0){
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
      mutate(datetime = as.POSIXct(ObservationTimeStart, tz = 'UTC', format = '%Y-%m-%dT%H:%M:%S'),
             local_datetime = with_tz(datetime, 'Etc/GMT+5'),
             acquisition_date = format(local_datetime, '%Y-%m-%d'),
             acquisition_hour = as.numeric(format(local_datetime, '%H'))) %>% 
      select(ID, acquisition_date, acquisition_hour, OBJECTID) %>% 
      left_join(., epscor_lakes_meta) %>% 
      mutate(source = list2a_files[i],
             sat = 'S2a')
    
    #join
    if(i == 1){
      sent2a_dates <- sent_inter_meta
    } else {
      sent2a_dates <- full_join(sent2a_dates, sent_inter_meta)
    }
    
  }
} else {
  sent2a_dates = current %>% 
    filter(grepl('2a', sat))
}

#create a composite sf layer and grab all the extended metadata in the kml file
if(length(download_2b>0)){
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
      mutate(datetime = as.POSIXct(ObservationTimeStart, tz = 'UTC', format = '%Y-%m-%dT%H:%M:%S'),
             local_datetime = with_tz(datetime, 'Etc/GMT+5'),
             acquisition_date = format(local_datetime, '%Y-%m-%d'),
             acquisition_hour = as.numeric(format(local_datetime, '%H'))) %>% 
      select(ID, acquisition_date, acquisition_hour, OBJECTID) %>% 
      left_join(., epscor_lakes_meta)%>% 
      mutate(source = list2b_files[i],
             sat = 'S2b')
    
    #join
    if(i == 1){
      sent2b_dates <- sent_inter_meta
    } else {
      sent2b_dates <- full_join(sent2b_dates, sent_inter_meta)
    }
    
  }
} else {
    sent2b_dates = current %>% 
      filter(grepl('2b', sat))
  }

sentdates <- full_join(sent2a_dates, sent2b_dates) %>% 
  arrange(acquisition_date, acquisition_hour)

write.csv(sentdates, paste0('EPSCoRBlooms_Sentinel_future_acquistion_v', Sys.Date(), '.csv'), row.names = F)

unlink(tmp, recursive = T)
