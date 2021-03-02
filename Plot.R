
# package and data --------------------------------------------------------

library(tidyverse)
library(sf)
library(gridExtra)
library(grid)
library(rlang)
library(ggspatial)
library(ggmap)

## intersection
intersection = st_read('C:/UMN/Projects/PedCrash/PedCrash_GIS/Reproduction_of_crash_model_construction/Intersections.shp',
                       stringsAsFactors = F) %>% 
  select(-FID0, -Legs) %>% 
  st_zm()

## mid-block
city_network = st_read('C:/UMN/Projects/PedCrash/PedCrash_GIS/Reproduction_of_crash_model_construction/Road_network_with_highway.shp',
                       stringsAsFactors = F) %>% 
  select(OFT) %>% 
  st_zm()

## city boundary, ACP50 boundary, CBD boundary
city_bdry <- st_read('C:/UMN/Projects/PedCrash/PedCrash_GIS/Reproduction_of_crash_model_construction/City_boundary.shp')
acp_bdry <- st_read('C:/UMN/Projects/PedCrash/PedCrash_GIS/Reproduction_of_crash_model_construction/ACP50_boundary.shp')
CBD_bdry <- st_read('C:/UMN/Projects/PedCrash/PedCrash_GIS/Reproduction_of_crash_model_construction/Downtown_boundary_dissolved.shp')

## data
new_intersection_result = read.csv('./result/new_intersection_result.csv',
                                   stringsAsFactors = F)
new_block_result = read.csv('./result/new_block_result.csv',
                            stringsAsFactors = F)

# high-risk location plot -------------------------------------------------

# get legend of the ggplot
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# compare function
compare <- function(person, location){
  
  ## dataset
  if(location == 'int'){
    data = new_intersection_result
    ID = data$JuncID
  } else {
    data = new_block_result
    ID = data$OFT
  }
  
  ## criteria for selection
  crit = nrow(data) * 0.01
  
  ## model and prediction
  model_loc = paste('./result/', person, '_', location, '_m.RDS', sep = '')
  m = readRDS(model_loc)
  pre = predict(m, data, type = 'response')
  
  no_exp_model_loc = paste('./result/', person, '_', location, '_no_exp.RDS', sep = '')
  no_exp = readRDS(no_exp_model_loc)
  no_exp_pre = predict(no_exp, data, type = 'response')
  
  ## combine prediction result
  result = cbind(ID,
                 pre,
                 no_exp_pre) %>% 
    as_tibble() %>% 
    mutate(rank = row_number(desc(pre)),
           rank_no_exp = row_number(desc(no_exp_pre))) %>% 
    mutate(both = ifelse(rank <= crit & rank_no_exp <= crit, 1, 0)) %>% 
    select(ID, rank, rank_no_exp, both)
  
  ## merge result to map
  if(location == 'int'){
    map = intersection %>% 
      left_join(result, by = c('JuncID'='ID'))
  } else {
    map = city_network %>% 
      left_join(result, by = c('OFT'='ID'))
  }
  
  ## select results for plotting
  plot_data <- map %>%
    filter(rank <= crit) %>% 
    mutate(both = ifelse(both == 1, 'Same Location', 'Different Location'))
  
  ## titles of subplots
  if(person == 'ped') {
    if (location == 'int') {
      title1 = '(a) Pedestrian Intersection Model with Exposure'
      title2 = '(b) Pedestrian Intersection Model without Exposure'
    } else {
      title1 = '(c) Pedestrian Mid-block Model with Exposure'
      title2 = '(d) Pedestrian Mid-block Model without Exposure'
    }
  } else {
    if (location == 'int') {
      title1 = '(a) Bicycle Intersection Model with Exposure'
      title2 = '(b) Bicycle Intersection Model without Exposure'
    } else {
      title1 = '(c) Bicycle Mid-block Model with Exposure'
      title2 = '(d) Bicycle Mid-block Model without Exposure'
    }
  }
  
  ## left figure (with exposure)
  p1 <- get_googlemap('Minneapolis', zoom = 11,
                      maptype = "roadmap",
                      style = c(feature = "all", element = "labels", visibility = 'off'),
                      color = "bw") %>%
    ggmap() +
    geom_sf(data = plot_data, aes(col = factor(both)), inherit.aes =  FALSE, size = 1, key_glyph = draw_key_rect) +
    coord_sf(crs = st_crs(4326)) +
    geom_sf(data = city_bdry, fill = NA, aes(col = 'City Boundary'), inherit.aes = FALSE) +
    coord_sf(crs = st_crs(4326)) +
    geom_sf(data = acp_bdry, fill = NA, aes(col = 'ACP50'), inherit.aes = FALSE) +
    coord_sf(crs = st_crs(4326)) +
    geom_sf(data = CBD_bdry, fill = NA, aes(col = 'CBD'), inherit.aes = FALSE) +
    coord_sf(crs = st_crs(4326)) +
    scale_colour_manual(
      values =  c('Blue', 'Black', 'Purple', 'Red', 'Green')
    ) +
    labs(title = title1,
         col = 'Legend') +
    theme(axis.text = element_blank(),
          rect = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          legend.position = 'top') +
    xlim(-93.34, -93.18) +
    ylim(44.89, 45.05) +
    annotation_scale(location = 'bl', width_hint = 0.4)
  
  legend <- get_legend(p1)
  
  p1 <- p1 + theme(legend.position = 'none')
  
  ## right figure (without exposure)
  plot_data <- map %>%
    filter(rank_no_exp < crit) %>% 
    mutate(both = ifelse(both == 1, 'Same Location', 'Different Location'))
    
  p2 <- get_googlemap('Minneapolis', zoom = 11,
                      maptype = "roadmap",
                      style = c(feature = "all", element = "labels", visibility = 'off'),
                      color = "bw") %>%
    ggmap() +
    geom_sf(data = plot_data, aes(col = factor(both)), inherit.aes =  FALSE, size = 1) +
    coord_sf(crs = st_crs(4326)) +
    geom_sf(data = city_bdry, fill = NA, aes(col = 'City Boundary'), inherit.aes = FALSE) +
    coord_sf(crs = st_crs(4326)) +
    geom_sf(data = acp_bdry, fill = NA, aes(col = 'ACP50'), inherit.aes = FALSE) +
    coord_sf(crs = st_crs(4326)) +
    geom_sf(data = CBD_bdry, fill = NA, aes(col = 'CBD'), inherit.aes = FALSE) +
    coord_sf(crs = st_crs(4326)) +
    scale_colour_manual(
      values =  c('Blue', 'Black', 'Purple', 'Red', 'Green')
    ) +
    labs(title = title2,
         col = 'Legend') +
    theme(axis.text = element_blank(),
          rect = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          legend.position = 'none') +
    xlim(-93.34, -93.18) +
    ylim(44.89, 45.05) +
    annotation_north_arrow(location = "bl", which_north = "true",
                           pad_x = unit(3.7, "in"),
                           pad_y = unit(5.7, "in"),
                           style = north_arrow_fancy_orienteering)
  
  ## combine two plots
  g <- grid.arrange(arrangeGrob(p1, p2, nrow = 1), legend, nrow = 2, heights = c(10, 1))
  
  ## save plot in two formats (jpg and emf)
  ggsave(paste('./result/', person, 'in', location, '.jpg'), g, width = 10, height = 8)
  ggsave(paste('./result/', person, 'in', location, '.emf'), g, width = 10, height = 8)
}

compare('ped', 'int')
compare('ped', 'block')
compare('bike', 'int')
compare('bike', 'block')

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
