
# package and data --------------------------------------------------------

library(tidyverse)
library(sf)

block_result = read.csv('./result/block_result.csv',
                        stringsAsFactors = F)
old_block_result = read.csv('C:/UMN/Projects/PedCrash/PedCrash_GIS/CrashNumPrediction/block_result_05_28_2019.csv',
                            stringsAsFactors = F)
city_network_connection = st_read('C:/UMN/Projects/PedCrash/PedCrash_GIS/Reproduction_of_crash_model_construction/Road_network_with_highway.shp',
                                      stringsAsFactors = F) %>% 
  st_drop_geometry() %>% 
  select(OBJECTID, OFT) %>% 
  mutate(OBJECTID = OBJECTID-1)

intersection_result = read.csv('./result/intersection_result.csv',
                               stringsAsFactors = F)
old_intersection_result = read.csv('C:/UMN/Projects/PedCrash/PedCrash_GIS/CrashNumPrediction/intersection_result_05_28_2019.csv',
                                   stringsAsFactors = F)


# compare -----------------------------------------------------------------

## pedestrian and bicycle count

# old_block_result %>%
#   select(TraStpNum, IfTraStp, IfTraStop, FID) %>%
#   left_join(city_network_connection, by = c('FID' = 'OBJECTID')) %>%
#   left_join(select(block_result, OFT, TraStpNum), by = 'OFT') %>%
#   View()

# combine -----------------------------------------------------------------

old_block_result %>% 
  select(-Main, -Local, -PopDen, -JobDen) %>% 
  rename(
    PedCrashNum = pedaccnumblock1535,
    BikeCrashNum = bikeaccnumblock1535,
    PedCount = ActPedCnt,
    BikeCount = ActBikeCnt,
    AADT_MPLS = AdjustedAADT,
    PedEstCount = PedCnt,
    BikeEstCount = BikeCnt,
    LnPedCount = ln_ped,
    LnBikeCount = ln_bike,
    LnAADT = ln_aadt,
    TraStop = IfTraStop,
    Downtown = IfDowntown,
    City = IfCity,
    ACP50 = IfACP50,
    Sidewalk = IfSideWalk,
    Bikelane = IfBikeLane,
    Main = ifmain,
    Second = ifsecondary,
    Local = iflocal
  ) %>% 
  select(
    FID, StrtTp, PedCrashNum, BikeCrashNum,
    PedCount, BikeCount, PedEstCount, BikeEstCount, AADT_MPLS, LnPedCount, LnBikeCount, LnAADT,
    IntNum, TraStop, (PerCommercial:PerOther),
    Downtown, City, ACP50,
    Sidewalk, Bikelane, Main, Second, Local,
    (PerChild:AvgVeh),
    -PerPoverty
  ) %>% 
  left_join(city_network_connection, by = c('FID'='OBJECTID')) %>% 
  left_join(select(block_result,
                   OFT, Poverty, LandMix, LaneWidth, DisToMin, PopDen, JobDen),
            by = 'OFT') %>% 
  relocate(OFT, .after = FID) %>% 
  write.csv('./result/new_block_result.csv', row.names = F)

old_intersection_result %>% 
  select(-Sidewalk, -Main, -Local, -Bikelane, -Trail, -PopDen, -JobDen) %>% 
  rename(
    PedCrashNum = pedaccnumint1535,
    BikeCrashNum = bikeaccnumint1535,
    PedCount = AdjustedActPedCnt,
    BikeCount = AdjustedActBikeCnt,
    AADT_MPLS = AdjustedAADT,
    PedEstCount = PedCnt,
    BikeEstCount = BikeCnt,
    LnPedCount = ln_ped,
    LnBikeCount = ln_bike,
    LnAADT = ln_aadt,
    TraStop = IfTraStop,
    Downtown = IfDowntown,
    City = IfCity,
    ACP50 = IfACP50,
    Sidewalk = IfSidewalk,
    Bikelane = IfBikelane,
    TraSignal = iftrfsig,
    StLight = ifstrtlit,
    Trail = IfTrail,
    Main = ifmain,
    Second = ifsecondary,
    Local = iflocal
  ) %>% 
  mutate(StopSign = ifelse(is.na(StpSignNum), 0, 1)) %>% 
  select(
    JuncID, PedCrashNum, BikeCrashNum,
    PedCount, BikeCount, PedEstCount, BikeEstCount, AADT_MPLS, LnPedCount, LnBikeCount, LnAADT,
    IntNum, TraStop, (PerCommercial:PerOther),
    Downtown, City, ACP50,
    Sidewalk, Bikelane, TraSignal, StLight, Trail, Main, Second, Local, StopSign,
    (PerChild:AvgVeh),
    -PerPoverty
  ) %>% 
  left_join(select(intersection_result,
                   JuncID, LandMix, Poverty, LaneWidth, DisToMin, PopDen, JobDen,
                   MainNum, SecondNum, LocalNum, LegNum),
            by = 'JuncID') %>% 
  write.csv('./result/new_intersection_result.csv', row.names = F)


































