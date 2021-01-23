
# package -----------------------------------------------------------------

library(tidyverse)
library(car)
library(pastecs)
library(reshape2)

new_intersection_result = read.csv('./result/new_intersection_result.csv')

intersection_data = new_intersection_result %>% 
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


# model -------------------------------------------------------------------

## pedestrian crash model at intersection #################################
## formula
ped_int_form = PedCrashNum ~ LnPedCount + BikeCount + I(BikeCount^2) + LnAADT +
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

## save results
vari_list = c('LnPedCount', 'BikeCount', 'I(BikeCount^2)', 'LnAADT',
              'PopDen', 'JobDen', 'IntNum', 'TraStop', 'PerCommercial', 'PerOffice', 'PerIndustrial', 'PerOpenspace', 'LandMix',
              'Downtown', 'Sidewalk', 'Trail', 'StLight', 'TraSignal', 
              'LaneWidth', 'LegNum', 'MainNum', 'SecondNum', 
              'PerChild', 'PerOld', 'PerMale', 'AvgHHSize', 'PerWhite', 'Poverty', '(Intercept)')

vari_name_list = c('Ln(Actual pedestrian count)', 'Actual bike count', '(Actual bike count)^2', 'Ln(Actual AADT)',
                   'Population density', 'Job density', 'Number of intersections', 'Presence of transit stop',
                   'Share of commercial area', 'Share of office area', 'Share of industrial area', 'Share of open space', 'Land use entropy',
                   'Downtown', 'Presence of sidewalk', 'Presence of trail',
                   'Presence of street light',
                   'Presence of traffic signal',
                   'Travel width of lane', 'Number of legs', 'Number of main roads', 'Number of secondary roads', 
                   'Share of children', 'Share of seniors', 'Share of men',
                   'Average household size', 'Share of white population', 'Share of poverty population', 'Constant')


model_two_form_result = data.frame(id = 1:length(vari_list),
                          variable = vari_list,
                          variable.name = vari_name_list) %>% 
  left_join(rownames_to_column(as.data.frame(coef(summary(ped_int_m))[, c(1,4)])), by = c('variable'='rowname')) %>% 
  rename(ped_int = Estimate, ped_int_p = 'Pr(>|z|)') %>% 
  mutate(ped_int_p_mark = case_when(
    ped_int_p < 0.001 ~ '***',
    ped_int_p < 0.01 ~ '**',
    ped_int_p < 0.05 ~ '*'
  )) %>% 
  select(variable.name,
         ped_int, ped_int_p_mark)

write.csv(model_two_form_result, './result/model_two_form_result.csv', row.names = F, na = '')

