
# data and package --------------------------------------------------------

library(tidyverse)
library(sf)

city_CB = st_read('C:/UMN/Projects/PedCrash/PedCrash_GIS/Reproduction_of_crash_model_construction/Pop_city.shp',
                  stringsAsFactors = F) %>% 
  select(BLOCKID10) %>% 
  st_zm() %>% 
  st_transform(crs = 26915) %>% 
  st_make_valid()

count_location = st_read('C:/UMN/Projects/PedCrash/PedCrash_GIS/data_backup/count location/Final_Count_Locations.shp', stringsAsFactors = F) %>% 
  select(ID_12) %>% 
  st_make_valid()

job = st_read('C:/UMN/Projects/PedCrash/PedCrash_GIS/Reproduction_of_crash_model_construction/job.shp',
              stringsAsFactors = F) %>% 
  st_drop_geometry() %>% 
  rename(job = c000)

pop = st_read('C:/UMN/Projects/PedCrash/PedCrash_GIS/Reproduction_of_crash_model_construction/Twin_cities_pop_2010.shp',
              stringsAsFactors = F) %>% 
  select(POP10, BLOCKID10) %>% 
  left_join(job, by = c('BLOCKID10'='id') ) %>% 
  mutate(job = ifelse(is.na(job), 0, job)) %>% 
  mutate(area = unclass(st_area(.))/1e4) %>% 
  mutate(PopDen = POP10/area,
         JobDen = job/area) %>% 
  select(PopDen, JobDen, BLOCKID10) %>% 
  st_drop_geometry()



# count location located in CB --------------------------------------------

count_location_CB = st_intersection(count_location, city_CB) %>% 
  st_drop_geometry()

count_location_CB %>% 
  left_join(pop, by = 'BLOCKID10') %>% 
  select(PopDen, JobDen) %>% 
  summary()
