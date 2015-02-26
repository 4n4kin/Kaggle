library(data.table)
library(stringr)
combined_aggregates <- fread("~/AXA Kaggle Challenge/combined_aggregates.txt")
combined_aggregates[,driver:=str_replace(string=combined_aggregates$user,pattern='./drivers/drivers//',replacement='')]
combined_aggregates[,angle_per_unit_speed_fraction:=angle_per_unit_speed_count/entries]

combined_aggregates[,user:=NULL]
combined_aggregates[,jerk_count:=NULL]
combined_aggregates[,angle_60:=NULL]
combined_aggregates[,entries:=NULL]
combined_aggregates[,dist_total_trip:=NULL]
combined_aggregates[,angle_per_unit_speed:=NULL]
combined_aggregates[,angle_per_unit_speed_count:=NULL]





View(head(combined_aggregates))
