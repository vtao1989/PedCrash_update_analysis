
# package and data --------------------------------------------------------

library(tidyverse)

new_intersection_result = read.csv('./result/new_intersection_result.csv')
new_block_result = read.csv('./result/new_block_result.csv')


# prediction --------------------------------------------------------------

## function to calculate numbers
calc_ratio = function(per, p) {
  q = per * nrow(p)
  r1 = nrow(filter(p, rank < q, rank_no_exp < q))/q
  r2 = nrow(filter(p, ACP50 == 1, rank < q, rank_no_exp < q))/nrow(filter(p, ACP50 == 1, rank < q))
  n1 = nrow(filter(p, ACP50 == 1, rank < q))
  n2 = nrow(filter(p, ACP50 == 1, rank_no_exp < q))
  d1 = nrow(filter(p, ACP50 == 1, rank_no_exp < q))/nrow(filter(p, ACP50 == 1, rank < q)) - 1
  return(c(r1, r2, n1, n2, d1))
}

## function to read models, predict crashes, calculate numbers
calc_result = function(person, location, data){
  
  ## read models and predict crashes
  model_loc = paste('./result/', person, '_', location, '_m.RDS', sep = '')
  m = readRDS(model_loc)
  pre = predict(m, data, type = 'response')
  model_loc = paste('./result/', person, '_', location, '_no_exp.RDS', sep = '')
  no_exp = readRDS(model_loc)
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
           rank_no_exp = row_number(desc(no_exp_pre)))
  
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
ped_int = calc_result('ped', 'int', new_intersection_result)
ped_block = calc_result('ped', 'block', new_block_result)
bike_int = calc_result('bike', 'int', new_intersection_result)
bike_block = calc_result('bike', 'block', new_block_result)

## city level comparison
cbind(ped_int$ratio[, 1],
  ped_block$ratio[, 1],
  bike_int$ratio[, 1],
  bike_block$ratio[, 1]) %>% 
  write.csv('./result/city_comparison.csv')

## ACP50 level comparison
cbind(ped_int$ratio[, 2],
   ped_block$ratio[, 2],
   bike_int$ratio[, 2],
   bike_block$ratio[, 2]) %>% 
  write.csv('./result/ACP50_comparison.csv')

## ACP50 level number comparison
cbind(ped_int$ratio[, 3:5],
   ped_block$ratio[, 3:5],
   bike_int$ratio[, 3:5],
   bike_block$ratio[, 3:5]) %>% 
  write.csv('./result/ACP50_num_comparison.csv')

## variance comparison
cbind(ped_int$var,
   ped_block$var,
   bike_int$var,
   bike_block$var) %>% 
  write.csv('./result/var_comparison.csv')
