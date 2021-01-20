
# package -----------------------------------------------------------------

library(tidyverse)
library(car)
library(pastecs)
library(reshape2)

new_intersection_result = read.csv('./result/new_intersection_result.csv')
new_block_result = read.csv('./result/new_block_result.csv')

intersection_data = new_intersection_result %>% 
  filter(City > 0,
         PedCount > 0)

block_data = new_block_result %>% 
  filter(City > 0,
         PedCount > 0)

# desc --------------------------------------------------------------------

## desc statistics
intersection_data_summary = intersection_data %>%
  select(-JuncID) %>% 
  stat.desc(basic = T) %>%
  slice(9L, 13L, 4L, 5L) %>%
  round(2) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column()

block_data_summary = block_data %>% 
  select(-OFT, -StrtTp, -FID) %>% 
  stat.desc(basic = T) %>%
  slice(9L, 13L, 4L, 5L) %>%
  round(2) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column()

## correlation
vari_int_list = c('LnPedCount', 'LnBikeCount', 'LnAADT',
              'PopDen', 'JobDen', 'IntNum', 'TraStop', 'PerCommercial', 'PerOffice', 'PerIndustrial', 'PerOpenspace', 'LandMix',
              'Downtown', 'LaneWidth', 'Sidewalk', 'Bikelane', 'Trail', 'StLight', 'TraSignal', 
              'MainNum', 'SecondNum', 'LegNum',
              'PerChild', 'PerOld', 'PerMale', 'AvgHHSize', 'PerWhite', 'Poverty')

cor_matrix = cor(subset(intersection_data, select = vari_int_list))

vari_block_list = c('LnPedCount', 'LnBikeCount', 'LnAADT',
                  'PopDen', 'JobDen', 'IntNum', 'TraStop', 'PerCommercial', 'PerOffice', 'PerIndustrial', 'PerOpenspace', 'LandMix',
                  'Downtown', 'LaneWidth', 'Sidewalk', 'Bikelane', 
                  'Main', 'Second',
                  'PerChild', 'PerOld', 'PerMale', 'AvgHHSize', 'PerWhite', 'Poverty')

cor_matrix = cor(subset(block_data, select = vari_block_list))

get_upper_tri = function(cormat){
  cormat[lower.tri(cormat)]= NA
  return(cormat)
}

upper_tri = get_upper_tri(cor_matrix)

melted_cormat = melt(upper_tri, na.rm = TRUE)

ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 11, hjust = 1))+
  coord_fixed() +
  geom_text(aes(Var2, Var1, label = round(value, 2)), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

# model -------------------------------------------------------------------

## pedestrian crash model at intersection #################################
## formula
ped_int_form = PedCrashNum ~ LnPedCount + LnBikeCount + LnAADT +
  PopDen + JobDen + IntNum + TraStop + PerCommercial + PerOffice + PerIndustrial + PerOpenspace + LandMix +
  Downtown + 
  LaneWidth + Sidewalk + Trail + StLight + TraSignal + MainNum + SecondNum + LegNum +
  PerChild + PerOld + PerMale + AvgHHSize + PerWhite + Poverty

## VIF
ped_int_lm = lm(ped_int_form,
                data = intersection_data)
vif(ped_int_lm)

## negative binomial model
ped_int_m = MASS::glm.nb(ped_int_form,
                         data = intersection_data,
                         control = glm.control(maxit = 50))
summary(ped_int_m)

## deviance R2
1-ped_int_m$deviance/ped_int_m$null.deviance

## save model
saveRDS(ped_int_m, './result/ped_int_m.RDS')

## model without two exposure variables
ped_int_no_exp = update(ped_int_m, . ~ . - LnPedCount - LnBikeCount)
summary(ped_int_no_exp)
1-ped_int_no_exp$deviance/ped_int_no_exp$null.deviance
saveRDS(ped_int_no_exp, './result/ped_int_no_exp.RDS')

# ## test for number of legs
# ped_int_form = PedCrashNum ~ LegNum + LnAADT + LnPedCount
# 
# ## negative binomial model
# ped_int_m = MASS::glm.nb(ped_int_form,
#                          data = intersection_data)
# summary(ped_int_m)


## pedestrian crash model at mid-block ####################################
## formula
ped_block_form = PedCrashNum ~ LnPedCount + LnBikeCount + LnAADT +
  PopDen + JobDen + IntNum + TraStop + PerCommercial + PerOffice + PerIndustrial + PerOpenspace + LandMix +
  Downtown + 
  LaneWidth + Sidewalk + Main + Second + 
  PerChild + PerOld + PerMale + AvgHHSize + PerWhite + Poverty

## VIF
ped_block_lm = lm(ped_block_form,
                  data = block_data)
vif(ped_block_lm)

## negative binomial model
ped_block_m = MASS::glm.nb(ped_block_form,
                         data = block_data,
                         control = glm.control(maxit = 175))
summary(ped_block_m)

## deviance R2
1-ped_block_m$deviance/ped_block_m$null.deviance

## save model
saveRDS(ped_block_m, './result/ped_block_m.RDS')

## model without two exposure variables
ped_block_no_exp = update(ped_block_m, . ~ . - LnPedCount - LnBikeCount,
                          control = glm.control(maxit = 200))
summary(ped_block_no_exp)
1-ped_block_no_exp$deviance/ped_block_no_exp$null.deviance
saveRDS(ped_block_no_exp, './result/ped_block_no_exp.RDS')

## bicycle crash model at intersection ######################################
## formula
bike_int_form = BikeCrashNum ~ LnPedCount + LnBikeCount + LnAADT +
  PopDen + JobDen + IntNum + TraStop + PerCommercial + PerOffice + PerIndustrial + PerOpenspace + LandMix +
  Downtown + 
  LaneWidth + Bikelane + Trail + StLight + TraSignal + MainNum + SecondNum + LegNum +
  PerChild + PerOld + PerMale + AvgHHSize + PerWhite + Poverty
bike_int_lm = lm(bike_int_form,
                 data = intersection_data)

## VIF
vif(bike_int_lm)

## negative binomial model
bike_int_m = MASS::glm.nb(bike_int_form,
                         data = intersection_data)
summary(bike_int_m)

## deviance R2
1-bike_int_m$deviance/bike_int_m$null.deviance

## save model
saveRDS(bike_int_m, './result/bike_int_m.RDS')

## model without two exposure variables
bike_int_no_exp = update(bike_int_m, . ~ . - LnPedCount - LnBikeCount)
summary(bike_int_no_exp)
1-bike_int_no_exp$deviance/bike_int_no_exp$null.deviance
saveRDS(bike_int_no_exp, './result/bike_int_no_exp.RDS')

## bicycle crash model at mid-block #########################################
## formula
bike_block_form = BikeCrashNum ~ LnPedCount + LnBikeCount + LnAADT +
  PopDen + JobDen + IntNum + TraStop + PerCommercial + PerOffice + PerIndustrial + PerOpenspace + LandMix +
  Downtown + 
  LaneWidth + Bikelane  + Main + Second + 
  PerChild + PerOld + PerMale + AvgHHSize + PerWhite + Poverty

## VIF
bike_block_lm = lm(bike_block_form,
                   data = block_data)
vif(bike_block_lm)

## negative binomial model
bike_block_m = MASS::glm.nb(bike_block_form,
                          data = block_data,
                          control = glm.control(maxit = 50))
summary(bike_block_m)

## deviance R2
1-bike_block_m$deviance/bike_block_m$null.deviance

## save model
saveRDS(bike_block_m, './result/bike_block_m.RDS')

# ## test for travel width of lane
# bike_block_form = BikeCrashNum ~ LaneWidth + LnAADT
# 
# ## negative binomial model
# bike_block_m = MASS::glm.nb(bike_block_form,
#                             data = block_data)
# summary(bike_block_m)

## model without two exposure variables
bike_block_no_exp = update(bike_block_m, . ~ . - LnPedCount - LnBikeCount)
summary(bike_block_no_exp)
1-bike_block_no_exp$deviance/bike_block_no_exp$null.deviance
saveRDS(bike_block_no_exp, './result/bike_block_no_exp.RDS')


# combine result ----------------------------------------------------------

vari_list = c('LnPedCount', 'LnBikeCount', 'LnAADT',
              'PopDen', 'JobDen', 'IntNum', 'TraStop', 'PerCommercial', 'PerOffice', 'PerIndustrial', 'PerOpenspace', 'LandMix',
              'Downtown', 'LaneWidth', 'Sidewalk', 'Bikelane', 'Trail', 'StLight', 'TraSignal', 
              'Main', 'Second', 'MainNum', 'SecondNum', 'LegNum',
              'PerChild', 'PerOld', 'PerMale', 'AvgHHSize', 'PerWhite', 'Poverty', '(Intercept)')

vari_name_list = c('Ln(Actual pedestrian count)', 'Ln(Actual bike count)', 'Ln(Actual AADT)',
                   'Population density', 'Job density', 'Number of intersections', 'Presence of transit stop',
                   'Share of commercial area', 'Share of office area', 'Share of industrial area', 'Share of open space', 'Land use entropy',
                   'Downtown', 'Travel width of lane', 'Presence of sidewalk', 'Presence of bike lane',
                   'Presence of trail', 'Presence of street light', 'Presence of traffic signal',
                   'Presence of main road', 'Presence of secondary road', 'Number of main roads', 'Number of secondary roads', 'Number of legs',
                   'Share of children', 'Share of seniors', 'Share of men',
                   'Average household size', 'Share of white population', 'Share of poverty population', 'Constant')

## models with two exposure variables
model_result = data.frame(id = 1:length(vari_list),
                          variable = vari_list,
                          variable.name = vari_name_list) %>% 
  left_join(rownames_to_column(as.data.frame(coef(summary(ped_int_m))[, c(1,4)])), by = c('variable'='rowname')) %>% 
  rename(ped_int = Estimate, ped_int_p = 'Pr(>|z|)') %>% 
  mutate(ped_int_p_mark = case_when(
    ped_int_p < 0.001 ~ '***',
    ped_int_p < 0.01 ~ '**',
    ped_int_p < 0.05 ~ '*'
  )) %>% 
  left_join(rownames_to_column(as.data.frame(coef(summary(ped_block_m))[, c(1,4)])), by = c('variable'='rowname')) %>% 
  rename(ped_block = Estimate, ped_block_p = 'Pr(>|z|)') %>% 
  mutate(ped_block_p_mark = case_when(
    ped_block_p < 0.001 ~ '***',
    ped_block_p < 0.01 ~ '**',
    ped_block_p < 0.05 ~ '*'
  )) %>% 
  left_join(rownames_to_column(as.data.frame(coef(summary(bike_int_m))[, c(1,4)])), by = c('variable'='rowname')) %>% 
  rename(bike_int = Estimate, bike_int_p = 'Pr(>|z|)') %>% 
  mutate(bike_int_p_mark = case_when(
    bike_int_p < 0.001 ~ '***',
    bike_int_p < 0.01 ~ '**',
    bike_int_p < 0.05 ~ '*'
  )) %>% 
  left_join(rownames_to_column(as.data.frame(coef(summary(bike_block_m))[, c(1,4)])), by = c('variable'='rowname')) %>% 
  rename(bike_block = Estimate, bike_block_p = 'Pr(>|z|)') %>% 
  mutate(bike_block_p_mark = case_when(
    bike_block_p < 0.001 ~ '***',
    bike_block_p < 0.01 ~ '**',
    bike_block_p < 0.05 ~ '*'
  )) %>% 
  select(-id, -ped_int_p, -ped_block_p, -bike_int_p, -bike_block_p) %>% 
  left_join(intersection_data_summary, by = c('variable' = 'rowname')) %>% 
  rename(int_mean = mean) %>% 
  select(-(std.dev:max)) %>% 
  left_join(block_data_summary, by = c('variable' = 'rowname')) %>% 
  rename(block_mean = mean) %>% 
  select(-(std.dev:max)) %>% 
  mutate(ped_int_elas = ifelse(variable %in% c('LnPedCount', 'LnBikeCount', 'LnAADT'), ped_int, ped_int*int_mean),
         ped_block_elas = ifelse(variable %in% c('LnPedCount', 'LnBikeCount', 'LnAADT'), ped_block, ped_block*block_mean),
         bike_int_elas = ifelse(variable %in% c('LnPedCount', 'LnBikeCount', 'LnAADT'), bike_int, bike_int*int_mean),
         bike_block_elas = ifelse(variable %in% c('LnPedCount', 'LnBikeCount', 'LnAADT'), bike_block, bike_block*block_mean)) %>% 
  select(variable.name,
         ped_int, ped_int_p_mark, ped_int_elas,
         ped_block, ped_block_p_mark, ped_block_elas,
         bike_int, bike_int_p_mark, bike_int_elas,
         bike_block, bike_block_p_mark, bike_block_elas)
write.csv(model_result, './result/model_result.csv', row.names = F, na = '')

## models without two exposure variables
model_no_exp_result = data.frame(id = 1:length(vari_list),
                          variable = vari_list,
                          variable.name = vari_name_list) %>% 
  left_join(rownames_to_column(as.data.frame(coef(summary(ped_int_no_exp))[, c(1,4)])), by = c('variable'='rowname')) %>% 
  rename(ped_int = Estimate, ped_int_p = 'Pr(>|z|)') %>% 
  mutate(ped_int_p_mark = case_when(
    ped_int_p < 0.001 ~ '***',
    ped_int_p < 0.01 ~ '**',
    ped_int_p < 0.05 ~ '*'
  )) %>% 
  left_join(rownames_to_column(as.data.frame(coef(summary(ped_block_no_exp))[, c(1,4)])), by = c('variable'='rowname')) %>% 
  rename(ped_block = Estimate, ped_block_p = 'Pr(>|z|)') %>% 
  mutate(ped_block_p_mark = case_when(
    ped_block_p < 0.001 ~ '***',
    ped_block_p < 0.01 ~ '**',
    ped_block_p < 0.05 ~ '*'
  )) %>% 
  left_join(rownames_to_column(as.data.frame(coef(summary(bike_int_no_exp))[, c(1,4)])), by = c('variable'='rowname')) %>% 
  rename(bike_int = Estimate, bike_int_p = 'Pr(>|z|)') %>% 
  mutate(bike_int_p_mark = case_when(
    bike_int_p < 0.001 ~ '***',
    bike_int_p < 0.01 ~ '**',
    bike_int_p < 0.05 ~ '*'
  )) %>% 
  left_join(rownames_to_column(as.data.frame(coef(summary(bike_block_no_exp))[, c(1,4)])), by = c('variable'='rowname')) %>% 
  rename(bike_block = Estimate, bike_block_p = 'Pr(>|z|)') %>% 
  mutate(bike_block_p_mark = case_when(
    bike_block_p < 0.001 ~ '***',
    bike_block_p < 0.01 ~ '**',
    bike_block_p < 0.05 ~ '*'
  )) %>% 
  select(-id, -ped_int_p, -ped_block_p, -bike_int_p, -bike_block_p) %>% 
  left_join(intersection_data_summary, by = c('variable' = 'rowname')) %>% 
  rename(int_mean = mean) %>% 
  select(-(std.dev:max)) %>% 
  left_join(block_data_summary, by = c('variable' = 'rowname')) %>% 
  rename(block_mean = mean) %>% 
  select(-(std.dev:max)) %>% 
  mutate(ped_int_elas = ifelse(variable %in% c('LnPedCount', 'LnBikeCount', 'LnAADT'), ped_int, ped_int*int_mean),
         ped_block_elas = ifelse(variable %in% c('LnPedCount', 'LnBikeCount', 'LnAADT'), ped_block, ped_block*block_mean),
         bike_int_elas = ifelse(variable %in% c('LnPedCount', 'LnBikeCount', 'LnAADT'), bike_int, bike_int*int_mean),
         bike_block_elas = ifelse(variable %in% c('LnPedCount', 'LnBikeCount', 'LnAADT'), bike_block, bike_block*block_mean)) %>% 
  select(variable.name,
         ped_int, ped_int_p_mark, ped_int_elas,
         ped_block, ped_block_p_mark, ped_block_elas,
         bike_int, bike_int_p_mark, bike_int_elas,
         bike_block, bike_block_p_mark, bike_block_elas)
write.csv(model_no_exp_result, './result/model_no_exp_result.csv', row.names = F, na = '')
