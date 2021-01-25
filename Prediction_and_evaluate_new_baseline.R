
# package and data --------------------------------------------------------

library(tidyverse)

new_intersection_result = read.csv('./result/new_intersection_result.csv')
new_block_result = read.csv('./result/new_block_result.csv')


# prediction --------------------------------------------------------------

## function to calculate numbers
calc_ratio = function(per, p) {
  q = nrow(filter(p, actual_rank <= per*nrow(p)))
  r1.1 = nrow(filter(p, rank <= q, actual_rank <= q))/q
  r1.2 = nrow(filter(p, rank_no_exp <= q, actual_rank <= q))/q
  m = nrow(filter(p, actual_rank <= per*nrow(p), ACP50 == 1))
  r2.1 = nrow(filter(p, ACP50 == 1, rank <= m, rank <= m))/m
  r2.2 = nrow(filter(p, ACP50 == 1, rank <= m, rank_no_exp <= m))/m
  n0 = nrow(filter(p, ACP50 == 1, actual_rank <= m))
  n1 = nrow(filter(p, ACP50 == 1, rank <= m))
  n2 = nrow(filter(p, ACP50 == 1, rank_no_exp <= m))
  d1 = n1/n0 - 1
  d2 = n2/n0 - 1
  return(c(r1.1, r1.2,
           r2.1, r2.2,
           n0, n1, n2,
           d1, d2))
}

## function to read models, predict crashes, calculate numbers
calc_result = function(person, location){
  
  ## read models and predict crashes
  if(location == 'int'){
    data = new_intersection_result
  } else {
    data = new_block_result
  }
  
  model_loc = paste('./result/', person, '_', location, '_m.RDS', sep = '')
  m = readRDS(model_loc)
  pre = predict(m, data, type = 'response')
  
  no_exp_model_loc = paste('./result/', person, '_', location, '_no_exp.RDS', sep = '')
  no_exp = readRDS(no_exp_model_loc)
  no_exp_pre = predict(no_exp, data, type = 'response')
  
  ## combine results
  if(person == 'ped'){
    CrashNum = data$PedCrashNum
  } else {
    CrashNum = data$BikeCrashNum
  }
  
  result = cbind(pre,
                no_exp_pre,
                CrashNum,
                data$ACP50) %>% 
    as_tibble() %>% 
    rename(ACP50 = V4) %>% 
    mutate(rank = row_number(desc(pre)),
           rank_no_exp = row_number(desc(no_exp_pre)),
           actual_rank = row_number(desc(CrashNum)))
  
  ## calculate numbers
  re1 = calc_ratio(0.01, result)
  re2 = calc_ratio(0.05, result)
  re3 = calc_ratio(0.1, result)
  
  var1 = sum((result$pre - result$CrashNum)^2)/nrow(result)
  var2 = sum((result$no_exp_pre - result$CrashNum)^2)/nrow(result)
  
  return(list(
    var = t(t(c(var1, var2))),
    ratio = rbind(re1, re2, re3)
  ))
}

## calculate numbers
ped_int = calc_result('ped', 'int')
ped_block = calc_result('ped', 'block')
bike_int = calc_result('bike', 'int')
bike_block = calc_result('bike', 'block')

## city level comparison
cbind(ped_int$ratio[, 1:2],
  ped_block$ratio[, 1:2],
  bike_int$ratio[, 1:2],
  bike_block$ratio[, 1:2]) %>% 
  write.csv('./result/city_comparison.csv')

## ACP50 level comparison
cbind(ped_int$ratio[, 3:4],
   ped_block$ratio[, 3:4],
   bike_int$ratio[, 3:4],
   bike_block$ratio[, 3:4]) %>% 
  write.csv('./result/ACP50_comparison.csv')

## ACP50 level number comparison
cbind(ped_int$ratio[, 5:9],
   ped_block$ratio[, 5:9],
   bike_int$ratio[, 5:9],
   bike_block$ratio[, 5:9]) %>% 
  write.csv('./result/ACP50_num_comparison.csv')

## variance comparison
cbind(ped_int$var,
   ped_block$var,
   bike_int$var,
   bike_block$var) %>% 
  write.csv('./result/var_comparison.csv')
