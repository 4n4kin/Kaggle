modelling<-function(data_aggregate){
  data_aggregate$prob <- 0
  data_aggregate$driver<-as.character(data_aggregate$driver)
  data_aggregate$trip<-as.character(data_aggregate$trip)
  
  driver_id <- unique(data_aggregate$driver)
  
  data_aggregate<-data.frame(data_aggregate)
  
  for(driver in driver_id){
    print(driver)
    
    data_aggregate$model_for <- data_aggregate$driver==driver
    rows_all <-rownames(data_aggregate)
    rows_d <- rows_all[data_aggregate$model_for]
    rows_nd <- sample(rows_all[!data_aggregate$model_for],400)
    use <- c(rows_d,rows_nd)
    data_use <- data_aggregate[use,]
    
    #print(use)
    data_use$model_for <- as.numeric(data_use$model_for)
    #print(colnames(data_use))
    data_use <- data_use[,colnames(data_use)[3:ncol(data_use)]]
    data_use<-data.frame(scale(x=data_use[,3:ncol(data_use)-2]),model_for=data_use$model_for)
    
    vars=colnames(data_use)
    #print(vars)
    data_use<-data_use[,vars]
    
    #print(table(data_use$model_for))
    fit <- glm(data_use$model_for~.,
               data=data.frame(scale(data_use[,1:44])),
               family='binomial')
    #print(fit$fitted.values[data_use$model_for==1])
    
    data_aggregate$prob[data_aggregate$driver==driver] <- fit$fitted.values[data_use$model_for==1]
  }
  #print(data_aggregate$prob)
  results<-data.frame(driver_trip=paste(data_aggregate$driver,data_aggregate$trip,sep='_'),
                      prob=data_aggregate$prob)
  return(results)
}

res<-modelling(combined_aggregates)
write.csv(res,'submission_10.csv',quote=F,row.names=F)
