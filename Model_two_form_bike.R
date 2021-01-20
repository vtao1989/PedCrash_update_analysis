
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

