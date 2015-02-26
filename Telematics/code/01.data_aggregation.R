library(data.table)
library(plyr)
library(dplyr)
library(doParallel)
#nodes <- detectCores()
#cl <- makeCluster(nodes)
#registerDoParallel(cl)

files_combined <- list.files('./combined/')

agg <- function(filepath){ 
  
  data <- read.csv(paste('./combined/',filepath,sep=''))
  data <- data.table(data)
  
  setnames(data,1,'trip')
  setcolorder(x=data,neworder=c('user','trip','x','y'))
  
  data$user <- as.character(data$user)
  data$user <- gsub(x=data$user,pattern='./drivers/',replacement='')
  
  #Create Lag variables
  data[, lag.x:=c(NA, x[-.N]), by=c('user','trip')]
  data[, lag.y:=c(NA, y[-.N]), by=c('user','trip')]
  
  #Distance per segment
  data$dist_segment <- sqrt( (data$y-data$lag.y)^2 +
                               (data$x-data$lag.x)^2 )
  
  #Distance
  
  data[,`:=`(entries=length(dist_segment),
             dist_total_trip=sum(dist_segment,na.rm=T),
             dist_first10=sum(head(dist_segment,10),na.rm=T),
             dist_last10=sum(tail(dist_segment,10),na.rm=T),
             speed_avg=mean(dist_segment,na.rm=T),
             zero_count=sum(dist_segment==0)),
       by=list(user,trip)]
  
  #Speed
  
  
  #Aggregation
  data<-data.frame(data)
  data_agg<-aggregate(data[8:ncol(data)],
                      by=list(user=as.character(data$user),
                              trip=as.character(data$trip)),
                      FUN=mean)
  return(data_agg)
}


data_aggregate<- ldply(.data=files_combined[1:10],
                       .parallel=F,
                       .fun=agg,
                       .progress='text')
