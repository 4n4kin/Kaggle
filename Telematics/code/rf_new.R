library(foreach)  
library(doParallel)  
library(parallel)
library(data.table)
library(plyr)

data_aggregate_new$jerk_fraction_positive <-data_aggregate_new$jerk_count_positive/data_aggregate_new$entries
data_aggregate_new$jerk_fraction_negative <- data_aggregate_new$jerk_count_negative/data_aggregate_new$entries
vars_use <- c('driver',
              'trip',
              "zero_count",
              'dist_total_trip',
              'speed_avg',
              'speed_0.1',
              "speed_0.5",
              "speed_0.95",
              "accel_0.1",
              'accel_0.5',
              "accel_0.95",
              'dist_first5',
              'dist_first10',
              "angle_per_unit_speed_fraction_5",
              "jerk_fraction_positive",
              "jerk_fraction_negative")

data_aggregate<-data_aggregate_new[,vars_use]
#cor_mat<-cor(data_aggregate[,3:ncol(data_aggregate)])
#corrplot(cor_mat,method='number')

for(i in 4:13){
  print(colnames(data_aggregate)[i])
  val <- quantile(data_aggregate[,i],0.9999)
  data_aggregate[,i] <- ifelse(data_aggregate[,i]>=val,val,data_aggregate[,i])
}

modelling<-function(data_aggregate){
  data_aggregate$prob <- 0
  data_aggregate$driver<-as.character(data_aggregate$driver)
  data_aggregate$trip<-as.character(data_aggregate$trip)
  driver_id <- unique(data_aggregate$driver)
  data_aggregate<-data.frame(data_aggregate)

  cal<-function(driver,data_aggregate){
    print(driver)
    
    library(randomForest)
    library(data.table)
    library(dismo)
    library(gbm)
    library(VIF)
    
    data_aggregate$model_for <- data_aggregate$driver==driver
    rows_all <-rownames(data_aggregate)
    rows_d <- rows_all[data_aggregate$model_for]
    rows_nd <- sample(rows_all[!data_aggregate$model_for],200)
    use <- c(rows_d,rows_nd)
    data_use <- data_aggregate[use,]
    
    driver <- data_use$driver
    trip <- data_use$trip    
    data_use$model_for <- as.numeric(data_use$model_for)
    data_use <- data_use[,3:ncol(data_use)]

    #Scaling data before creating model
    data_use <- data.frame(scale(x=data_use[,-ncol(data_use)]),
                           model_for=data_use$model_for)
    data_use$driver <- NULL
    data_use$trip <- NULL
    data_use$prob <- NULL
    

    for(i in 1:(ncol(data_use)-1)){
      
        mn = mean(data_use[1:200,i])
        std = sd(data_use[1:200,i])
        data_use[i] <- (data_use[i]-mn) / std
        
      }

    set.seed(42)
    
    fit_rf <- randomForest(x=data_use[,-ncol(data_use)],
                      corr.bias=T,
                      y=factor(data_use$model_for),
                      ntree=5000)
    

#    probs=fit$votes[,2]
#    out <- data.frame(driver_trip=paste(driver,trip,sep='_'),prob=probs)[1:200,]
    return(fit_rf)
gc()
  }
  
  result <- llply(driver_id,cal,data_aggregate,.progress='text')
  return(result)
}

res<-modelling(data_aggregate)
#write.csv(res,'submission_rf_5000trees_limitedvars2_newScaling_10sec_corr.bias_speed_squared.csv',quote=F,row.names=F)
