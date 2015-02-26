use <- c('user','trip','entries',"dist_total_trip",
         "dist_total_trip" ,"zero_count","jerk_count" ,"angle_per_unit_speed_fraction",
         "speed_0.5","speed_0.95" ,
         "accel_0.5","accel_0.95" )
data_aggregate <- data_aggregates[,use]
colnames(data_aggregate)[1] <- 'driver'
data_aggregate$jerk_fraction = data_aggregate$jerk_count/data_aggregate$entries
data_aggregate$jerk_count<-NULL
data_aggregate$entries<-NULL
data_aggregate$dist_total_trip.1<-NULL
