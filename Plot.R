
# package and data --------------------------------------------------------

library(tidyverse)
library(sf)

## intersection
intersection = st_read('C:/UMN/Projects/PedCrash/PedCrash_GIS/Reproduction_of_crash_model_construction/Intersections.shp',
                       stringsAsFactors = F) %>% 
  select(-FID0, -Legs) %>% 
  st_zm()

## mid-block
city_network = st_read('C:/UMN/Projects/PedCrash/PedCrash_GIS/Reproduction_of_crash_model_construction/Road_network_with_highway.shp',
                       stringsAsFactors = F) %>% 
  select(OFT, STREET_TYP) %>% 
  st_zm() %>% 
  mutate(STREET_TYP = ifelse(is.na(STREET_TYP), 'Local', STREET_TYP))

## connection between intersection and mid-block
int_block = st_read('C:/UMN/Projects/PedCrash/PedCrash_GIS/Reproduction_of_crash_model_construction/Intersection_intersect_with_block.shp',
                    stringsAsFactors = F) %>% 
  st_drop_geometry() %>% 
  select(JuncID, OFT)

# create intersection and mid-block shp file for plot ---------------------

city_network %>% 
  mutate(class = case_when(STREET_TYP == 'Local' | STREET_TYP == 'Activity Center' |
                             STREET_TYP == 'Parkway' | STREET_TYP == 'Null' ~ 'Local',
                           STREET_TYP == 'Commerce' | STREET_TYP == 'Community' |
                             STREET_TYP == 'Industrial' | STREET_TYP == 'Neighborhood' ~ 'Secondary',
                           STREET_TYP == 'Commuter' ~ 'Main',
                           STREET_TYP == 'Highway' ~ 'Highway',
                           T ~ 'Local')) %>% 
  filter(class != 'Highway') %>% 
  st_write('./result/city_network_road_class.shp')

## road function
## Local: Local, Activity Center, Parkway, Null, NA
## Secondary: Commerce, Community, Industrial, Neighborhood
## Main: Commuter
## Highway: Highway
road_function = city_network %>% 
  st_drop_geometry() %>% 
  mutate(class = case_when(STREET_TYP == 'Local' | STREET_TYP == 'Activity Center' |
                             STREET_TYP == 'Parkway' | STREET_TYP == 'Null' ~ 'Local',
                           STREET_TYP == 'Commerce' | STREET_TYP == 'Community' |
                             STREET_TYP == 'Industrial' | STREET_TYP == 'Neighborhood' ~ 'Secondary',
                           STREET_TYP == 'Commuter' ~ 'Main',
                           STREET_TYP == 'Highway' ~ 'Highway',
                           T ~ 'Local'),
         value = 1) %>% 
  right_join(int_block, by = 'OFT') %>%
  select(-OFT, -STREET_TYP) %>% 
  group_by(JuncID) %>% 
  summarise(MainNum = sum(class == 'Main'),
            SecondNum = sum(class == 'Secondary'),
            LocalNum = sum(class == 'Local'),
            HighwayNum = sum(class == 'Highway'))

## join road function to intersection
## calculate the number of road segments for different road classes
## set dummy variables for different road classes
intersection %>% 
  left_join(road_function, by = 'JuncID') %>% 
  filter(!(MainNum == 0 & SecondNum == 0 & LocalNum == 0 & HighwayNum > 0)) %>% 
  mutate(Main = ifelse(MainNum > 0, 1, 0),
         Second = ifelse(SecondNum > 0, 1, 0),
         Highway = ifelse(HighwayNum > 0, 1, 0),
         Local = ifelse(LocalNum > 0, 1, 0)) %>% 
  st_write('./result/intersection_without_highway.shp')
