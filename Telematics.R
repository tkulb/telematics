library(dplyr)

library(e1071)



trainS1 = read.csv(file="/data/0_comp_data/train_data.csv",header=TRUE)

# testS1 = read.csv(file="/data/0_comp_data/train_data.csv",header=TRUE)

model_data = read.csv(file="/data/0_comp_data/model_data.csv",header=TRUE)





# 87470 different trips

# 47026 different trips to test on

# even training on 10,000 could be useful?

# trainS2 = slice(trainS1,1:38000)

# trainS3 = slice(trainS1,38001:47026)

# trainS4 = slice(trainS1,10001:10000)





# FEATURE CREATION



Mode <- function(x) {
  
  ux <- unique(x)
  
  ux[which.max(tabulate(match(x, ux)))]
  
}







# trainS4 = slice(trainS1,10001:20000)

# input_data = trainS4

# input_data = trainS1



# train_filter = trainS1

# train_filter = slice(trainS1,10001:40000)

# feature1 = model_data %>% filter(train_ind == 1) %>% right_join(train_filter,by="trip_id")

# saveRDS(feature1,'/data/OverFitBit/files/feature1.rds')





# competition test data is here

feature1 = model_data %>% filter(train_ind == 0)

saveRDS(feature1,'/data/OverFitBit/files/Hfeature1.rds')



feature2 = transform(feature1,speed_num = as.numeric(speed))

# feature3 = feature2 %>% slice(1:500)



# STEP 1. calc row differences

feature_rowdiff = feature2 %>% mutate(
  
  time_diff = time - lag(time,default=time[1])
  
  ,bearing_diff = (bearing - lag(bearing,default=bearing[1])) / time_diff
  
  
  
  ,bearing_diff_corrected = ifelse(bearing_diff > 180 , bearing_diff - 360, ifelse(bearing_diff < -180, bearing_diff + 360,bearing_diff))
  
  
  
  
  
  ,speed_diff = (speed_num - lag(speed_num,default=speed_num[1])) / time_diff
  
  
  
  ,bearing_direction = ifelse(abs(bearing_diff + lag(bearing_diff,default=bearing_diff[1])) <
                                
                                (abs(bearing_diff) + abs(lag(bearing_diff,default=bearing_diff[1]))),1,0)
  
  
  
  ,bearing_direction_corrected = ifelse(abs(bearing_diff_corrected + lag(bearing_diff_corrected,default=bearing_diff_corrected[1])) <
                                          
                                          (abs(bearing_diff_corrected) + abs(lag(bearing_diff_corrected,default=bearing_diff_corrected[1]))),1,0)
  
  
  
  
  
  ,speed_direction = ifelse(abs(speed_diff + lag(speed_diff,default=speed_diff[1])) <
                              
                              (abs(speed_diff) + abs(lag(speed_diff,default=speed_diff[1]))),1,0)
  
  
  
  ,distance_crow =    sqrt((lat^2+lon^2))
  
  ,dist_crow_diff = distance_crow - lag(distance_crow)
  
  ,lat_plus_lon = abs(lat)+abs(lon)
  
  ,max_speed_diff = speed_num - ifelse(speedLimit == -1, 65,speedLimit)
  
  ,min_speed_diff = ifelse(speedLimit == -1, 65,speedLimit)  - speed_num
  
  # ,accel = sqrt(diff(diff(ifelse(lat=NA,0,lat)))^2+diff(diff(ifelse(lon=NA,0,lon))^2))
  
  ,accel_speed = speed_num - lag(speed_num,default=speed_num[1])
  
  
  
  ,accel_direction = ifelse(accel_speed * lag(accel_speed) < 1,1,0)
  
  
  
  ,jerk_speed = accel_speed - lag(accel_speed,default=accel_speed[1])
  
  ,horsepower_mass = accel_speed * speed_num
  
  
  
  ,speed_lat = (lat-lag(lat,default=lat[1])) / time_diff
  
  ,speed_lon = (lon-lag(lon,default=lon[1])) / time_diff
  
  ,accel_lat = (speed_lat -lag(speed_lat,default=speed_lat[1])) / time_diff
  
  ,accel_lon = (speed_lon -lag(speed_lon,default=speed_lat[1])) / time_diff
  
  
  
  
  
  ,dist_crow_chg = ifelse(abs(dist_crow_diff + lag(dist_crow_diff,default=dist_crow_diff[1])) <
                            
                            (abs(dist_crow_diff) + abs(lag(dist_crow_diff,default=dist_crow_diff[1]))),1,0)
  
)



# EDA <- df_status(feature_rowdiff,print_results = FALSE)

# EDA2 = kable(EDA,longtable=TRUE, booktabs=TRUE,row.names = TRUE)

# saveRDS(feature_rowdiff,'/data/OverFitBit/files/feature_rowdiff.rds')

# saveRDS(feature_rowdiff,'/data/OverFitBit/files/Hfeature_rowdiff.rds')



# STEP 2. join on time 0 info for each trip

feature_time_zero = feature2 %>% filter(time == 0) %>% mutate(trip_speed_start = speed_num
                                                              
                                                              ,bearing_start = bearing ) %>%
  
  select(trip_id,trip_speed_start,bearing_start)



feature3 = feature2 %>% filter(time == 0)



feature_time_zero2 = left_join(feature3,feature_time_zero,by="trip_id") %>%
  
  select(-speed,-delta_time,-time,-lat,-lon,-train_ind)



# saveRDS(feature_time_zero2,'/data/OverFitBit/files/feature_time_zero2.rds')

# saveRDS(feature3,'/data/OverFitBit/files/feature3.rds')



# saveRDS(feature_time_zero2,'/data/OverFitBit/files/Hfeature_time_zero2.rds')

# saveRDS(feature3,'/data/OverFitBit/files/Hfeature3.rds')



# Time Max

feature_time_max_group = feature2 %>% group_by(trip_id) %>% summarize(max_time = max(time))



feature_time_max = feature_rowdiff %>% right_join(feature_time_max_group,by=c("trip_id"="trip_id","time"="max_time")) %>%
  
  mutate(trip_distance_crow = distance_crow
         
         , speed_end = speed_num) %>%
  
  select(trip_id,max_time = time,trip_distance_crow,speed_end)





# saveRDS(feature_time_max,'/data/OverFitBit/files/feature_time_max.rds')

# saveRDS(feature_time_max,'/data/OverFitBit/files/Hfeature_time_max.rds')



# feature_time_max2 = feature2 %>% left_join(feature_time_max,by="trip_id")







# lat / long Diff Lapses (gaps in trip  lat long) - work in progress, similar to turns

dist_lapses = feature_rowdiff %>%  mutate(dist_lapse_pt1 = ifelse(abs(speed_lat + speed_lon) > .1,1,0)
                                          
                                          ,dist_lapse_pt01 = ifelse(abs(speed_lat + speed_lon) > .01,1,0)
                                          
                                          
                                          
                                          
                                          
                                          ,dist_lapse_pt3_4sec = ifelse(abs(speed_lat + speed_lon)
                                                                        
                                                                        +abs(lag(speed_lat)+lag(speed_lon))
                                                                        
                                                                        +abs(lag(speed_lat,k=2)+lag(speed_lon,k=2))
                                                                        
                                                                        +abs(lag(speed_lat,k=3)+lag(speed_lon,k=3))
                                                                        
                                                                        
                                                                        
                                                                        > .3,1,0)
                                          
)



dist_lapses2 = dist_lapses %>% filter(time > 0) %>% group_by(trip_id) %>% summarize(dist_lapse_pt1_sum = sum(dist_lapse_pt1)
                                                                                    
                                                                                    ,dist_lapse_pt01_sum = sum(dist_lapse_pt01)
                                                                                    
                                                                                    ,dist_lapse_pt3_4sec_sum = sum(dist_lapse_pt3_4sec)
                                                                                    
                                                                                    
                                                                                    
                                                                                    
                                                                                    
                                                                                    ,dist_lapse_pt1_sum_norm = sum(dist_lapse_pt1) / n()
                                                                                    
                                                                                    ,dist_lapse_pt3_4sec_sum_norm = sum(dist_lapse_pt01) / n()
                                                                                    
                                                                                    ,dist_lapse_pt3_4sec_sum_norm = sum(dist_lapse_pt3_4sec) / n()
                                                                                    
                                                                                    
                                                                                    
)



# saveRDS(dist_lapses2,'/data/OverFitBit/files/dist_lapses2.rds')

# saveRDS(dist_lapses2,'/data/OverFitBit/files/Hdist_lapses2.rds')





# Time Diff Lapses (gaps in trip diff time (should be 1 sec)) - work in progress, similar to turns

time_lapses = feature_rowdiff %>% mutate(time_diff_2 = ifelse(time_diff >= 2,1,0)
                                         
                                         ,time_diff_5 = ifelse(time_diff >= 5,1,0)
                                         
                                         ,time_diff_10 = ifelse(time_diff >= 10,1,0)
                                         
                                         ,time_diff_60 = ifelse(time_diff >= 60,1,0)
                                         
                                         ,time_diff_120 = ifelse(time_diff >= 120,1,0)
                                         
                                         ,time_diff_250 = ifelse(time_diff >= 250,1,0)
                                         
                                         
                                         
)



time_lapses2 = time_lapses %>% filter(time > 0) %>% group_by(trip_id) %>% summarize(time_diff_2_sum = sum(time_diff_2)
                                                                                    
                                                                                    ,time_diff_5_sum = sum(time_diff_5)
                                                                                    
                                                                                    ,time_diff_10_sum = sum(time_diff_10)
                                                                                    
                                                                                    ,time_diff_60_sum = sum(time_diff_60)
                                                                                    
                                                                                    ,time_diff_120_sum = sum(time_diff_120)
                                                                                    
                                                                                    ,time_diff_250_sum = sum(time_diff_250)
                                                                                    
                                                                                    
                                                                                    
                                                                                    ,time_diff_2_sum_norm = sum(time_diff_2) / n()
                                                                                    
                                                                                    ,time_diff_5_sum_norm = sum(time_diff_5) / n()
                                                                                    
                                                                                    ,time_diff_10_sum_norm = sum(time_diff_10) / n()
                                                                                    
                                                                                    ,time_diff_60_sum_norm = sum(time_diff_60) / n()
                                                                                    
                                                                                    ,time_diff_120_sum_norm = sum(time_diff_120) / n()
                                                                                    
                                                                                    ,time_diff_250_sum_norm = sum(time_diff_250) / n()
                                                                                    
                                                                                    
                                                                                    
)



# saveRDS(time_lapses2,'/data/OverFitBit/files/time_lapses2.rds')

# saveRDS(time_lapses2,'/data/OverFitBit/files/Htime_lapses2.rds')



# While stopped - work in progress, similar to turns

feature_stops = feature_rowdiff %>% mutate(stop_below_1mph = ifelse(speed_num < 1,1,0)
                                           
                                           ,stop_below_5mph = ifelse(speed_num < 5,1,0)
                                           
                                           ,stop_below_10mph = ifelse(speed_num < 10,1,0)
                                           
                                           ,stop_below_15mph = ifelse(speed_num < 15,1,0)
                                           
                                           
                                           
                                           ,stop_below_1mph_5sec = ifelse((speed_num < 1
                                                                           
                                                                           && lag(speed_num) < 1
                                                                           
                                                                           && lag(speed_num,k=2) < 1
                                                                           
                                                                           && lag(speed_num,k=3) < 1
                                                                           
                                                                           && lag(speed_num,k=4) < 1)
                                                                          
                                                                          ,1,0)                
                                           
                                           ,stop_below_1mph_20sec = ifelse((speed_num < 1
                                                                            
                                                                            && lag(speed_num) < 1
                                                                            
                                                                            && lag(speed_num,k=2) < 1
                                                                            
                                                                            && lag(speed_num,k=3) < 1
                                                                            
                                                                            && lag(speed_num,k=4) < 1
                                                                            
                                                                            && lag(speed_num,k=5) < 1
                                                                            
                                                                            && lag(speed_num,k=6) < 1
                                                                            
                                                                            && lag(speed_num,k=7) < 1
                                                                            
                                                                            && lag(speed_num,k=8) < 1
                                                                            
                                                                            && lag(speed_num,k=9) < 1
                                                                            
                                                                            && lag(speed_num,k=10) < 1
                                                                            
                                                                            && lag(speed_num,k=11) < 1
                                                                            
                                                                            && lag(speed_num,k=12) < 1
                                                                            
                                                                            && lag(speed_num,k=13) < 1
                                                                            
                                                                            && lag(speed_num,k=14) < 1
                                                                            
                                                                            && lag(speed_num,k=15) < 1
                                                                            
                                                                            && lag(speed_num,k=16) < 1
                                                                            
                                                                            && lag(speed_num,k=17) < 1
                                                                            
                                                                            && lag(speed_num,k=18) < 1
                                                                            
                                                                            && lag(speed_num,k=19) < 1)
                                                                           
                                                                           ,1,0)
                                           
                                           ,stop_below_5mph_5sec = ifelse((speed_num < 5
                                                                           
                                                                           && lag(speed_num) < 5
                                                                           
                                                                           && lag(speed_num,k=2) < 5
                                                                           
                                                                           && lag(speed_num,k=3) < 5
                                                                           
                                                                           && lag(speed_num,k=4) < 5)
                                                                          
                                                                          ,1,0)                
                                           
                                           ,stop_below_5mph_20sec = ifelse((speed_num < 5
                                                                            
                                                                            && lag(speed_num) < 5
                                                                            
                                                                            && lag(speed_num,k=2) < 5
                                                                            
                                                                            && lag(speed_num,k=3) < 5
                                                                            
                                                                            && lag(speed_num,k=4) < 5
                                                                            
                                                                            && lag(speed_num,k=5) < 5
                                                                            
                                                                            && lag(speed_num,k=6) < 5
                                                                            
                                                                            && lag(speed_num,k=7) < 5
                                                                            
                                                                            && lag(speed_num,k=8) < 5
                                                                            
                                                                            && lag(speed_num,k=9) < 5
                                                                            
                                                                            && lag(speed_num,k=10) < 5
                                                                            
                                                                            && lag(speed_num,k=11) < 5
                                                                            
                                                                            && lag(speed_num,k=12) < 5
                                                                            
                                                                            && lag(speed_num,k=13) < 5
                                                                            
                                                                            && lag(speed_num,k=14) < 5
                                                                            
                                                                            && lag(speed_num,k=15) < 5
                                                                            
                                                                            && lag(speed_num,k=16) < 5
                                                                            
                                                                            && lag(speed_num,k=17) < 5
                                                                            
                                                                            && lag(speed_num,k=18) < 5
                                                                            
                                                                            && lag(speed_num,k=19) < 5)
                                                                           
                                                                           ,1,0)
                                           
                                           
                                           
                                           ,accel_stop_5mph_5sec_5 = ifelse(stop_below_5mph_5sec == 1,
                                                                            
                                                                            accel_speed + lead(accel_speed)
                                                                            
                                                                            +lead(accel_speed,k=2)
                                                                            
                                                                            +lead(accel_speed,k=3)
                                                                            
                                                                            +lead(accel_speed,k=2)
                                                                            
                                                                            ,0)
                                           
                                           ,accel_stop_1mph_5sec_5 = ifelse(stop_below_1mph_5sec == 1,
                                                                            
                                                                            accel_speed + lead(accel_speed)
                                                                            
                                                                            +lead(accel_speed,k=2)
                                                                            
                                                                            +lead(accel_speed,k=3)
                                                                            
                                                                            +lead(accel_speed,k=2)
                                                                            
                                                                            ,0)
                                           
)



# saveRDS(feature_stops,'/data/OverFitBit/files/feature_stops.rds')

# saveRDS(feature_stops,'/data/OverFitBit/files/Hfeature_stops.rds')





feature_stops2 = feature_stops %>% filter(time > 0) %>% group_by(trip_id) %>% summarize(stop_below_1mph_sum = sum(stop_below_1mph)
                                                                                        
                                                                                        ,stop_below_5mph_sum = sum(stop_below_5mph)
                                                                                        
                                                                                        ,stop_below_10mph_sum = sum(stop_below_10mph)
                                                                                        
                                                                                        ,stop_below_15mph_sum = sum(stop_below_15mph)
                                                                                        
                                                                                        ,stop_below_1mph_5sec_sum = sum(stop_below_1mph_5sec)
                                                                                        
                                                                                        ,stop_below_1mph_20sec_sum = sum(stop_below_1mph_20sec)
                                                                                        
                                                                                        ,stop_below_5mph_5sec_sum = sum(stop_below_5mph_5sec)
                                                                                        
                                                                                        ,stop_below_5mph_20sec_sum = sum(stop_below_5mph_20sec)
                                                                                        
                                                                                        
                                                                                        
                                                                                        ,accel_stop_1mph_5sec_5_sum = sum(accel_stop_1mph_5sec_5)
                                                                                        
                                                                                        ,accel_stop_5mph_5sec_5_sum = sum(accel_stop_5mph_5sec_5)
                                                                                        
                                                                                        
                                                                                        
                                                                                        ,stop_below_1mph_sum_norm = sum(stop_below_1mph) / n()
                                                                                        
                                                                                        ,stop_below_5mph_sum_norm = sum(stop_below_5mph) / n()
                                                                                        
                                                                                        ,stop_below_10mph_sum_norm = sum(stop_below_10mph) / n()
                                                                                        
                                                                                        ,stop_below_15mph_sum_norm = sum(stop_below_15mph) / n()
                                                                                        
                                                                                        ,stop_below_1mph_5sec_sum_norm = sum(stop_below_1mph_5sec) / n()
                                                                                        
                                                                                        ,stop_below_1mph_20sec_sum_norm = sum(stop_below_1mph_20sec) / n()
                                                                                        
                                                                                        ,stop_below_5mph_5sec_sum_norm = sum(stop_below_5mph_5sec) / n()
                                                                                        
                                                                                        ,stop_below_5mph_20sec_sum_norm = sum(stop_below_5mph_20sec) / n()
                                                                                        
                                                                                        
                                                                                        
)



# saveRDS(feature_stops2,'/data/OverFitBit/files/feature_stops2.rds')

# saveRDS(feature_stops2,'/data/OverFitBit/files/Hfeature_stops2.rds')



# While Turning

feature_turns = feature_rowdiff %>% filter(time > 0) %>% mutate(turn_5 = ifelse(abs(bearing_diff_corrected) > 5,1,0)
                                                                
                                                                ,turn_10 = ifelse(abs(bearing_diff_corrected) > 10,1,0)
                                                                
                                                                ,turn_20 = ifelse(abs(bearing_diff_corrected) > 20,1,0)
                                                                
                                                                ,turn_70_4sec = ifelse(abs(bearing_diff_corrected+lag(bearing_diff_corrected)
                                                                                           
                                                                                           +lag(bearing_diff_corrected,k=2) + lag(bearing_diff_corrected,k=3))
                                                                                       
                                                                                       > 70,1,0)
                                                                
                                                                ,turn_50_4sec = ifelse(abs(bearing_diff_corrected+lag(bearing_diff_corrected)
                                                                                           
                                                                                           +lag(bearing_diff_corrected,k=2) + lag(bearing_diff_corrected,k=3))
                                                                                       
                                                                                       > 50,1,0)
                                                                
                                                                ,turn_150_6sec = ifelse(abs(bearing_diff_corrected+lag(bearing_diff_corrected)
                                                                                            
                                                                                            +lag(bearing_diff_corrected,k=2) + lag(bearing_diff_corrected,k=3)
                                                                                            
                                                                                            +lag(bearing_diff_corrected,k=4) + lag(bearing_diff_corrected,k=5))
                                                                                        
                                                                                        > 150,1,0)
                                                                
)



# saveRDS(feature_turns,'/data/OverFitBit/files/feature_turns.rds')

# saveRDS(feature_turns,'/data/OverFitBit/files/Hfeature_turns.rds')



# feature_turns = readRDS('/data/OverFitBit/files/feature_turns.rds')



feature_turns2 = feature_turns[-(1:2),] %>% filter(time > 0) %>% filter(turn_5==1) %>% group_by(trip_id) %>%
  
  summarize(turn_5_speed_avg = mean(speed_num)
            
            ,turn_5_speed_median = median(speed_num)
            
            ,turn_5_speed_mode = Mode(speed_num)
            
            ,turn_5_speed_max = max(speed_num)
            
            ,turn_5_speed_sd = sd(speed_num)
            
            ,turn_5_speed_kurtosis = kurtosis(speed_num)
            
            ,turn_5_speed_skewness = skewness(speed_num)
            
            ,turn_5_speed_P95 = quantile(speed_num,0.95)
            
            ,turn_5_speed_P5 = quantile(speed_num,0.05)
            
            ,turn_5_speed_P80 = quantile(speed_num,0.80)
            
            ,turn_5_speed_P20 = quantile(speed_num,0.20)
            
            ,turn_5_speed_P99 = quantile(speed_num,0.99)
            
            ,turn_5_speed_P1 = quantile(speed_num,0.01)
            
            
            
            ,turn_5_accel_speed_avg = mean(accel_speed)
            
            ,turn_5_accel_speed_median = median(accel_speed)
            
            ,turn_5_accel_speed_mode = Mode(accel_speed)
            
            ,turn_5_accel_speed_max = max(accel_speed)
            
            ,turn_5_accel_speed_sd = sd(accel_speed)
            
            ,turn_5_accel_speed_kurtosis = kurtosis(accel_speed)
            
            ,turn_5_accel_speed_skewness = skewness(accel_speed)
            
            ,turn_5_accel_speed_P95 = quantile(accel_speed,0.95)
            
            ,turn_5_accel_speed_P5 = quantile(accel_speed,0.05)
            
            ,turn_5_accel_speed_P80 = quantile(accel_speed,0.80)
            
            ,turn_5_accel_speed_P20 = quantile(accel_speed,0.20)
            
            ,turn_5_accel_speed_P99 = quantile(accel_speed,0.99)
            
            ,turn_5_accel_speed_P1 = quantile(accel_speed,0.01)
            
            
            
            ,turn_5_speed_lat_avg = mean(speed_lat)
            
            ,turn_5_speed_lat_median = median(speed_lat)
            
            ,turn_5_speed_lat_mode = Mode(speed_lat)
            
            ,turn_5_speed_lat_max = max(speed_lat)
            
            ,turn_5_speed_lat_sd = sd(speed_lat)
            
            ,turn_5_speed_lat_kurtosis = kurtosis(speed_lat)
            
            ,turn_5_speed_lat_skewness = skewness(speed_lat)
            
            ,turn_5_speed_lat_P95 = quantile(speed_lat,0.95)
            
            ,turn_5_speed_lat_P5 = quantile(speed_lat,0.05)
            
            ,turn_5_speed_lat_P80 = quantile(speed_lat,0.80)
            
            ,turn_5_speed_lat_P20 = quantile(speed_lat,0.20)
            
            ,turn_5_speed_lat_P99 = quantile(speed_lat,0.99)
            
            ,turn_5_speed_lat_P1 = quantile(speed_lat,0.01)
            
            
            
            ,turn_5_speed_lon_avg = mean(speed_lon)
            
            ,turn_5_speed_lon_median = median(speed_lon)
            
            ,turn_5_speed_lon_mode = Mode(speed_lon)
            
            ,turn_5_speed_lon_max = max(speed_lon)
            
            ,turn_5_speed_lon_sd = sd(speed_lon)
            
            ,turn_5_speed_lon_kurtosis = kurtosis(speed_lon)
            
            ,turn_5_speed_lon_skewness = skewness(speed_lon)
            
            ,turn_5_speed_lon_P95 = quantile(speed_lon,0.95)
            
            ,turn_5_speed_lon_P5 = quantile(speed_lon,0.05)
            
            ,turn_5_speed_lon_P80 = quantile(speed_lon,0.80)
            
            ,turn_5_speed_lon_P20 = quantile(speed_lon,0.20)
            
            ,turn_5_speed_lon_P99 = quantile(speed_lon,0.99)
            
            ,turn_5_speed_lon_P1 = quantile(speed_lon,0.01)
            
            
            
            ,turn_5_accel_lon_avg = mean(accel_lon)
            
            ,turn_5_accel_lon_median = median(accel_lon)
            
            ,turn_5_accel_lon_mode = Mode(accel_lon)
            
            ,turn_5_accel_lon_max = max(accel_lon)
            
            ,turn_5_accel_lon_sd = sd(accel_lon)
            
            ,turn_5_accel_lon_kurtosis = kurtosis(accel_lon)
            
            ,turn_5_accel_lon_skewness = skewness(accel_lon)
            
            ,turn_5_accel_lon_P95 = quantile(accel_lon,0.95)
            
            ,turn_5_accel_lon_P5 = quantile(accel_lon,0.05)
            
            ,turn_5_accel_lon_P80 = quantile(accel_lon,0.80)
            
            ,turn_5_accel_lon_P20 = quantile(accel_lon,0.20)
            
            ,turn_5_accel_lon_P99 = quantile(accel_lon,0.99)
            
            ,turn_5_accel_lon_P1 = quantile(accel_lon,0.01)
            
            
            
            ,turn_5_accel_lat_avg = mean(accel_lat)
            
            ,turn_5_accel_lat_median = median(accel_lat)
            
            ,turn_5_accel_lat_mode = Mode(accel_lat)
            
            ,turn_5_accel_lat_max = max(accel_lat)
            
            ,turn_5_accel_lat_sd = sd(accel_lat)
            
            ,turn_5_accel_lat_kurtosis = kurtosis(accel_lat)
            
            ,turn_5_accel_lat_skewness = skewness(accel_lat)
            
            ,turn_5_accel_lat_P95 = quantile(accel_lat,0.95)
            
            ,turn_5_accel_lat_P5 = quantile(accel_lat,0.05)
            
            ,turn_5_accel_lat_P80 = quantile(accel_lat,0.80)
            
            ,turn_5_accel_lat_P20 = quantile(accel_lat,0.20)
            
            ,turn_5_accel_lat_P99 = quantile(accel_lat,0.99)
            
            ,turn_5_accel_lat_P1 = quantile(accel_lat,0.01)
            
            
            
            
            
  )



# saveRDS(feature_turns2,'/data/OverFitBit/files/feature_turns2.rds')

# saveRDS(feature_turns2,'/data/OverFitBit/files/Hfeature_turns2.rds')





feature_turns2B = feature_turns %>% filter(turn_10==1) %>% group_by(trip_id) %>%
  
  summarize(turn_10_speed_avg = mean(speed_num)
            
            ,turn_10_speed_median = median(speed_num)
            
            ,turn_10_speed_mode = Mode(speed_num)
            
            ,turn_10_speed_max = max(speed_num)
            
            ,turn_10_speed_sd = sd(speed_num)
            
            ,turn_10_speed_kurtosis = kurtosis(speed_num)
            
            ,turn_10_speed_skewness = skewness(speed_num)
            
            ,turn_10_speed_P95 = quantile(speed_num,0.95)
            
            ,turn_10_speed_P5 = quantile(speed_num,0.05)
            
            ,turn_10_speed_P80 = quantile(speed_num,0.80)
            
            ,turn_10_speed_P20 = quantile(speed_num,0.20)
            
            ,turn_10_speed_P99 = quantile(speed_num,0.99)
            
            ,turn_10_speed_P1 = quantile(speed_num,0.01)
            
            ,turn_10 = 1
            
            ,turn_10_count = n()
            
            
            
            ,turn_10_accel_speed_avg = mean(accel_speed)
            
            ,turn_10_accel_speed_median = median(accel_speed)
            
            ,turn_10_accel_speed_mode = Mode(accel_speed)
            
            ,turn_10_accel_speed_max = max(accel_speed)
            
            ,turn_10_accel_speed_sd = sd(accel_speed)
            
            ,turn_10_accel_speed_kurtosis = kurtosis(accel_speed)
            
            ,turn_10_accel_speed_skewness = skewness(accel_speed)
            
            ,turn_10_accel_speed_P95 = quantile(accel_speed,0.95)
            
            ,turn_10_accel_speed_P5 = quantile(accel_speed,0.05)
            
            
            
            ,turn_10_speed_lat_avg = mean(speed_lat)
            
            ,turn_10_speed_lat_median = median(speed_lat)
            
            ,turn_10_speed_lat_mode = Mode(speed_lat)
            
            ,turn_10_speed_lat_max = max(speed_lat)
            
            ,turn_10_speed_lat_sd = sd(speed_lat)
            
            ,turn_10_speed_lat_kurtosis = kurtosis(speed_lat)
            
            ,turn_10_speed_lat_skewness = skewness(speed_lat)
            
            ,turn_10_speed_lat_P95 = quantile(speed_lat,0.95)
            
            ,turn_10_speed_lat_P5 = quantile(speed_lat,0.05)
            
            
            
            ,turn_10_speed_lon_avg = mean(speed_lon)
            
            ,turn_10_speed_lon_median = median(speed_lon)
            
            ,turn_10_speed_lon_mode = Mode(speed_lon)
            
            ,turn_10_speed_lon_max = max(speed_lon)
            
            ,turn_10_speed_lon_sd = sd(speed_lon)
            
            ,turn_10_speed_lon_kurtosis = kurtosis(speed_lon)
            
            ,turn_10_speed_lon_skewness = skewness(speed_lon)
            
            ,turn_10_speed_lon_P95 = quantile(speed_lon,0.95)
            
            ,turn_10_speed_lon_P5 = quantile(speed_lon,0.05)
            
            
            
            ,turn_10_accel_lon_avg = mean(accel_lon)
            
            ,turn_10_accel_lon_median = median(accel_lon)
            
            ,turn_10_accel_lon_mode = Mode(accel_lon)
            
            ,turn_10_accel_lon_max = max(accel_lon)
            
            ,turn_10_accel_lon_sd = sd(accel_lon)
            
            ,turn_10_accel_lon_kurtosis = kurtosis(accel_lon)
            
            ,turn_10_accel_lon_skewness = skewness(accel_lon)
            
            ,turn_10_accel_lon_P95 = quantile(accel_lon,0.95)
            
            ,turn_10_accel_lon_P5 = quantile(accel_lon,0.05)
            
            
            
            ,turn_10_accel_lat_avg = mean(accel_lat)
            
            ,turn_10_accel_lat_median = median(accel_lat)
            
            ,turn_10_accel_lat_mode = Mode(accel_lat)
            
            ,turn_10_accel_lat_max = max(accel_lat)
            
            ,turn_10_accel_lat_sd = sd(accel_lat)
            
            ,turn_10_accel_lat_kurtosis = kurtosis(accel_lat)
            
            ,turn_10_accel_lat_skewness = skewness(accel_lat)
            
            ,turn_10_accel_lat_P95 = quantile(accel_lat,0.95)
            
            ,turn_10_accel_lat_P5 = quantile(accel_lat,0.05)
            
  )



# saveRDS(feature_turns2B,'/data/OverFitBit/files/feature_turns2B.rds')

# saveRDS(feature_turns2B,'/data/OverFitBit/files/Hfeature_turns2B.rds')



feature_turns2C = feature_turns %>% filter(turn_20==1) %>% group_by(trip_id) %>%
  
  summarize(turn_20_speed_avg = mean(speed_num)
            
            ,turn_20_speed_median = median(speed_num)
            
            ,turn_20_speed_mode = Mode(speed_num)
            
            ,turn_20_speed_max = max(speed_num)
            
            ,turn_20_speed_sd = sd(speed_num)
            
            ,turn_20_speed_kurtosis = kurtosis(speed_num)
            
            ,turn_20_speed_skewness = skewness(speed_num)
            
            ,turn_20_speed_P95 = quantile(speed_num,0.95)
            
            ,turn_20_speed_P5 = quantile(speed_num,0.05)
            
            ,turn_20 = 1
            
            ,turn_20_count = n()
            
            
            
            ,turn_20_accel_speed_avg = mean(accel_speed)
            
            ,turn_20_accel_speed_median = median(accel_speed)
            
            ,turn_20_accel_speed_mode = Mode(accel_speed)
            
            ,turn_20_accel_speed_max = max(accel_speed)
            
            ,turn_20_accel_speed_sd = sd(accel_speed)
            
            ,turn_20_accel_speed_kurtosis = kurtosis(accel_speed)
            
            ,turn_20_accel_speed_skewness = skewness(accel_speed)
            
            ,turn_20_accel_speed_P95 = quantile(accel_speed,0.95)
            
            ,turn_20_accel_speed_P5 = quantile(accel_speed,0.05)
            
            
            
            ,turn_20_speed_lat_avg = mean(speed_lat)
            
            ,turn_20_speed_lat_median = median(speed_lat)
            
            ,turn_20_speed_lat_mode = Mode(speed_lat)
            
            ,turn_20_speed_lat_max = max(speed_lat)
            
            ,turn_20_speed_lat_sd = sd(speed_lat)
            
            ,turn_20_speed_lat_kurtosis = kurtosis(speed_lat)
            
            ,turn_20_speed_lat_skewness = skewness(speed_lat)
            
            ,turn_20_speed_lat_P95 = quantile(speed_lat,0.95)
            
            ,turn_20_speed_lat_P5 = quantile(speed_lat,0.05)
            
            
            
            ,turn_20_speed_lon_avg = mean(speed_lon)
            
            ,turn_20_speed_lon_median = median(speed_lon)
            
            ,turn_20_speed_lon_mode = Mode(speed_lon)
            
            ,turn_20_speed_lon_max = max(speed_lon)
            
            ,turn_20_speed_lon_sd = sd(speed_lon)
            
            ,turn_20_speed_lon_kurtosis = kurtosis(speed_lon)
            
            ,turn_20_speed_lon_skewness = skewness(speed_lon)
            
            ,turn_20_speed_lon_P95 = quantile(speed_lon,0.95)
            
            ,turn_20_speed_lon_P5 = quantile(speed_lon,0.05)
            
            
            
            ,turn_20_accel_lon_avg = mean(accel_lon)
            
            ,turn_20_accel_lon_median = median(accel_lon)
            
            ,turn_20_accel_lon_mode = Mode(accel_lon)
            
            ,turn_20_accel_lon_max = max(accel_lon)
            
            ,turn_20_accel_lon_sd = sd(accel_lon)
            
            ,turn_20_accel_lon_kurtosis = kurtosis(accel_lon)
            
            ,turn_20_accel_lon_skewness = skewness(accel_lon)
            
            ,turn_20_accel_lon_P95 = quantile(accel_lon,0.95)
            
            ,turn_20_accel_lon_P5 = quantile(accel_lon,0.05)
            
            
            
            ,turn_20_accel_lat_avg = mean(accel_lat)
            
            ,turn_20_accel_lat_median = median(accel_lat)
            
            ,turn_20_accel_lat_mode = Mode(accel_lat)
            
            ,turn_20_accel_lat_max = max(accel_lat)
            
            ,turn_20_accel_lat_sd = sd(accel_lat)
            
            ,turn_20_accel_lat_kurtosis = kurtosis(accel_lat)
            
            ,turn_20_accel_lat_skewness = skewness(accel_lat)
            
            ,turn_20_accel_lat_P95 = quantile(accel_lat,0.95)
            
            ,turn_20_accel_lat_P5 = quantile(accel_lat,0.05)
            
  )



# saveRDS(feature_turns2C,'/data/OverFitBit/files/feature_turns2C.rds')

# saveRDS(feature_turns2C,'/data/OverFitBit/files/Hfeature_turns2C.rds')



feature_turns2D = feature_turns %>% filter(turn_70_4sec==1) %>% group_by(trip_id) %>%
  
  summarize(turn_70_speed_avg = mean(speed_num)
            
            ,turn_70_speed_median = median(speed_num)
            
            ,turn_70_speed_mode = Mode(speed_num)
            
            ,turn_70_speed_max = max(speed_num)
            
            ,turn_70_speed_sd = sd(speed_num)
            
            ,turn_70_speed_kurtosis = kurtosis(speed_num)
            
            ,turn_70_speed_skewness = skewness(speed_num)
            
            ,turn_70_speed_P95 = quantile(speed_num,0.95)
            
            ,turn_70_speed_P5 = quantile(speed_num,0.05)
            
            ,turn_70_4sec = 1
            
            ,turn_70_count = n()
            
            
            
            ,turn_70_accel_speed_avg = mean(accel_speed)
            
            ,turn_70_accel_speed_median = median(accel_speed)
            
            ,turn_70_accel_speed_mode = Mode(accel_speed)
            
            ,turn_70_accel_speed_max = max(accel_speed)
            
            ,turn_70_accel_speed_sd = sd(accel_speed)
            
            ,turn_70_accel_speed_kurtosis = kurtosis(accel_speed)
            
            ,turn_70_accel_speed_skewness = skewness(accel_speed)
            
            ,turn_70_accel_speed_P95 = quantile(accel_speed,0.95)
            
            ,turn_70_accel_speed_P5 = quantile(accel_speed,0.05)
            
            
            
            ,turn_70_speed_lat_avg = mean(speed_lat)
            
            ,turn_70_speed_lat_median = median(speed_lat)
            
            ,turn_70_speed_lat_mode = Mode(speed_lat)
            
            ,turn_70_speed_lat_max = max(speed_lat)
            
            ,turn_70_speed_lat_sd = sd(speed_lat)
            
            ,turn_70_speed_lat_kurtosis = kurtosis(speed_lat)
            
            ,turn_70_speed_lat_skewness = skewness(speed_lat)
            
            ,turn_70_speed_lat_P95 = quantile(speed_lat,0.95)
            
            ,turn_70_speed_lat_P5 = quantile(speed_lat,0.05)
            
            
            
            ,turn_70_speed_lon_avg = mean(speed_lon)
            
            ,turn_70_speed_lon_median = median(speed_lon)
            
            ,turn_70_speed_lon_mode = Mode(speed_lon)
            
            ,turn_70_speed_lon_max = max(speed_lon)
            
            ,turn_70_speed_lon_sd = sd(speed_lon)
            
            ,turn_70_speed_lon_kurtosis = kurtosis(speed_lon)
            
            ,turn_70_speed_lon_skewness = skewness(speed_lon)
            
            ,turn_70_speed_lon_P95 = quantile(speed_lon,0.95)
            
            ,turn_70_speed_lon_P5 = quantile(speed_lon,0.05)
            
            
            
            ,turn_70_accel_lon_avg = mean(accel_lon)
            
            ,turn_70_accel_lon_median = median(accel_lon)
            
            ,turn_70_accel_lon_mode = Mode(accel_lon)
            
            ,turn_70_accel_lon_max = max(accel_lon)
            
            ,turn_70_accel_lon_sd = sd(accel_lon)
            
            ,turn_70_accel_lon_kurtosis = kurtosis(accel_lon)
            
            ,turn_70_accel_lon_skewness = skewness(accel_lon)
            
            ,turn_70_accel_lon_P95 = quantile(accel_lon,0.95)
            
            ,turn_70_accel_lon_P5 = quantile(accel_lon,0.05)
            
            
            
            ,turn_70_accel_lat_avg = mean(accel_lat)
            
            ,turn_70_accel_lat_median = median(accel_lat)
            
            ,turn_70_accel_lat_mode = Mode(accel_lat)
            
            ,turn_70_accel_lat_max = max(accel_lat)
            
            ,turn_70_accel_lat_sd = sd(accel_lat)
            
            ,turn_70_accel_lat_kurtosis = kurtosis(accel_lat)
            
            ,turn_70_accel_lat_skewness = skewness(accel_lat)
            
            ,turn_70_accel_lat_P95 = quantile(accel_lat,0.95)
            
            ,turn_70_accel_lat_P5 = quantile(accel_lat,0.05)
            
            
            
  )



# saveRDS(feature_turns2D,'/data/OverFitBit/files/feature_turns2D.rds')

# saveRDS(feature_turns2D,'/data/OverFitBit/files/Hfeature_turns2D.rds')



feature_turns2E = feature_turns %>% filter(turn_150_6sec==1) %>% group_by(trip_id) %>%
  
  summarize(turn_150_speed_avg = mean(speed_num)
            
            ,turn_150_speed_median = median(speed_num)
            
            ,turn_150_speed_mode = Mode(speed_num)
            
            ,turn_150_speed_max = max(speed_num)
            
            ,turn_150_speed_sd = sd(speed_num)
            
            ,turn_150_speed_kurtosis = kurtosis(speed_num)
            
            ,turn_150_speed_skewness = skewness(speed_num)
            
            ,turn_150_speed_P95 = quantile(speed_num,0.95)
            
            ,turn_150_speed_P5 = quantile(speed_num,0.05)
            
            ,turn_150_6sec = 1
            
            ,turn_150_count = n()
            
            
            
            ,turn_150_accel_speed_avg = mean(accel_speed)
            
            ,turn_150_accel_speed_median = median(accel_speed)
            
            ,turn_150_accel_speed_mode = Mode(accel_speed)
            
            ,turn_150_accel_speed_max = max(accel_speed)
            
            ,turn_150_accel_speed_sd = sd(accel_speed)
            
            ,turn_150_accel_speed_kurtosis = kurtosis(accel_speed)
            
            ,turn_150_accel_speed_skewness = skewness(accel_speed)
            
            ,turn_150_accel_speed_P95 = quantile(accel_speed,0.95)
            
            ,turn_150_accel_speed_P5 = quantile(accel_speed,0.05)
            
            
            
            ,turn_150_speed_lat_avg = mean(speed_lat)
            
            ,turn_150_speed_lat_median = median(speed_lat)
            
            ,turn_150_speed_lat_mode = Mode(speed_lat)
            
            ,turn_150_speed_lat_max = max(speed_lat)
            
            ,turn_150_speed_lat_sd = sd(speed_lat)
            
            ,turn_150_speed_lat_kurtosis = kurtosis(speed_lat)
            
            ,turn_150_speed_lat_skewness = skewness(speed_lat)
            
            ,turn_150_speed_lat_P95 = quantile(speed_lat,0.95)
            
            ,turn_150_speed_lat_P5 = quantile(speed_lat,0.05)
            
            
            
            ,turn_150_speed_lon_avg = mean(speed_lon)
            
            ,turn_150_speed_lon_median = median(speed_lon)
            
            ,turn_150_speed_lon_mode = Mode(speed_lon)
            
            ,turn_150_speed_lon_max = max(speed_lon)
            
            ,turn_150_speed_lon_sd = sd(speed_lon)
            
            ,turn_150_speed_lon_kurtosis = kurtosis(speed_lon)
            
            ,turn_150_speed_lon_skewness = skewness(speed_lon)
            
            ,turn_150_speed_lon_P95 = quantile(speed_lon,0.95)
            
            ,turn_150_speed_lon_P5 = quantile(speed_lon,0.05)
            
            
            
            ,turn_150_accel_lon_avg = mean(accel_lon)
            
            ,turn_150