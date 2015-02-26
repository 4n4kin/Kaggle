library(data.table)
library(plyr)
#dirs <- list.dirs('./drivers/')[-1]

data_list<-vector(mode='list',length=200)
names(data_list) <- 1:200

for(trip in 1:200){
  
  print(trip)
  agg <- function(filepath,trip) { 
    
    library(data.table)
    data <- fread(paste(filepath,'/',trip,'.csv',sep=''))
    #data <- data.table(data)
    data$user<-gsub(x=filepath,pattern='./drivers//',replacement='')
    data$trip<-as.character(trip)
    
    setcolorder(x=data,neworder=c('user','trip','x','y'))
    data<-data.table(data)
    #Create Lag variables
    data[, lag.x:=c(0, x[-.N]), by=c('user','trip')]
    data[, lag.y:=c(0, y[-.N]), by=c('user','trip')]
    data[,lead.x:=c(x[-1],x[.N]),by=c('user','trip')]
    data[,lead.y:=c(y[-1],y[.N]),by=c('user','trip')]
    
    
    #Angle
    data[,dot:=( ( (lag.x-x)*(lead.x-x) + (lag.y - y)*(lead.y-y) ) /
                   ( (sqrt((lag.x - x)^2+((lag.y -y)^2 ))) * (sqrt((lead.x-x)^2+(lead.y-y)^2)) ) ),
         by=c('user','trip')]
    con = data$dot==1
    data[,angle:=ifelse(test=con,yes=pi,no=acos(dot)),
         by=c('user','trip')]
    
    data[,dot:=NULL]
    #Distance per segment, speed,acceleration,jerk
    
    data[,dist_segment := sqrt( (y-lag.y)^2 +
                                  (x-lag.x)^2 )]
    
    data[, lag.speed:=c(0, dist_segment[-.N]), by=c('user','trip')]
    data[,accel:= c(0,0,(dist_segment - lag.speed)[3:.N])]
    
    data[, lag.accel:=c(0, accel[-.N]), by=c('user','trip')]
    data[, jerk:= c(0,0,0,(accel - lag.accel)[4:.N])]
    
    #Distance
    data[,`:=`(entries = length(dist_segment),
               dist_total_trip = sum(dist_segment[-c((.N-50):(.N))],na.rm=T),
               dist_first5 = sum(dist_segment[1:5],na.rm=T),
               dist_last5 = sum(dist_segment[(.N-5):(.N)],na.rm=T),
               zero_count = sum(tail(dist_segment==0,100),na.rm=T),
               jerk_count = sum(abs(jerk[-c((.N-50):(.N))])>=3),
               angle_per_unit_speed = angle/dist_segment,
               angle_60 = sum(angle[-c((.N-50):(.N))]<pi/3,na.rm=T)),
         by=list(user,trip)]
    
    data$angle_per_unit_speed[is.na(data$angle_per_unit_speed)] <- 1
    
    data[,angle_per_unit_speed_count:=sum(angle_per_unit_speed<=0.1),
         by=list(user,trip)]
    
    data[,angle_per_unit_speed_fraction:=angle_per_unit_speed_count/entries,
         by=list(user,trip)]
    
    #Speed Quantiles
    speed_quantiles<-function(data){
      data[,`:=`(speed_0.05=quantile(dist_segment[-c((.N-50):(.N))],probs=0.05,na.rm=T),
                 speed_0.1=quantile(dist_segment[-c((.N-50):(.N))],probs=0.1,na.rm=T),
                 speed_0.15=quantile(dist_segment[-c((.N-50):(.N))],probs=0.15,na.rm=T),
                 speed_0.2=quantile(dist_segment[-c((.N-50):(.N))],probs=0.20,na.rm=T),
                 speed_0.25=quantile(dist_segment[-c((.N-50):(.N))],probs=0.25,na.rm=T),
                 speed_0.3=quantile(dist_segment[-c((.N-50):(.N))],probs=0.3,na.rm=T),
                 speed_0.35=quantile(dist_segment[-c((.N-50):(.N))],probs=0.35,na.rm=T),
                 speed_0.4=quantile(dist_segment[-c((.N-50):(.N))],probs=0.4,na.rm=T),
                 speed_0.45=quantile(dist_segment[-c((.N-50):(.N))],probs=0.45,na.rm=T),
                 speed_0.5=quantile(dist_segment[-c((.N-50):(.N))],probs=0.5,na.rm=T),
                 speed_0.55=quantile(dist_segment[-c((.N-50):(.N))],probs=0.55,na.rm=T),
                 speed_0.6=quantile(dist_segment[-c((.N-50):(.N))],probs=0.6,na.rm=T),
                 speed_0.65=quantile(dist_segment[-c((.N-50):(.N))],probs=0.65,na.rm=T),
                 speed_0.7=quantile(dist_segment[-c((.N-50):(.N))],probs=0.7,na.rm=T),
                 speed_0.75=quantile(dist_segment[-c((.N-50):(.N))],probs=0.75,na.rm=T),
                 speed_0.8=quantile(dist_segment[-c((.N-50):(.N))],probs=0.8,na.rm=T),
                 speed_0.85=quantile(dist_segment[-c((.N-50):(.N))],probs=0.85,na.rm=T),
                 speed_0.9=quantile(dist_segment[-c((.N-50):(.N))],probs=0.9,na.rm=T),
                 speed_0.95=quantile(dist_segment[-c((.N-50):(.N))],probs=0.95,na.rm=T),
                 speed_1.00=quantile(dist_segment[-c((.N-50):(.N))],probs=1,na.rm=T)),
           by=list(user,trip)]
    }
    data<-speed_quantiles(data)
    
    #Acceleration Quantiles
    acceleration_quantiles<-function(data){
      data[,`:=`(accel_0.05=quantile(accel[-c((.N-50):(.N))],probs=0.05,na.rm=T),
                 accel_0.1=quantile(accel[-c((.N-50):(.N))],probs=0.1,na.rm=T),
                 accel_0.15=quantile(accel[-c((.N-50):(.N))],probs=0.15,na.rm=T),
                 accel_0.2=quantile(accel[-c((.N-50):(.N))],probs=0.20,na.rm=T),
                 accel_0.25=quantile(accel[-c((.N-50):(.N))],probs=0.25,na.rm=T),
                 accel_0.3=quantile(accel[-c((.N-50):(.N))],probs=0.3,na.rm=T),
                 accel_0.35=quantile(accel[-c((.N-50):(.N))],probs=0.35,na.rm=T),
                 accel_0.4=quantile(accel[-c((.N-50):(.N))],probs=0.4,na.rm=T),
                 accel_0.45=quantile(accel[-c((.N-50):(.N))],probs=0.45,na.rm=T),
                 accel_0.5=quantile(accel[-c((.N-50):(.N))],probs=0.5,na.rm=T),
                 accel_0.55=quantile(accel[-c((.N-50):(.N))],probs=0.55,na.rm=T),
                 accel_0.6=quantile(accel[-c((.N-50):(.N))],probs=0.6,na.rm=T),
                 accel_0.65=quantile(accel[-c((.N-50):(.N))],probs=0.65,na.rm=T),
                 accel_0.7=quantile(accel[-c((.N-50):(.N))],probs=0.7,na.rm=T),
                 accel_0.75=quantile(accel[-c((.N-50):(.N))],probs=0.75,na.rm=T),
                 accel_0.8=quantile(accel[-c((.N-50):(.N))],probs=0.8,na.rm=T),
                 accel_0.85=quantile(accel[-c((.N-50):(.N))],probs=0.85,na.rm=T),
                 accel_0.9=quantile(accel[-c((.N-50):(.N))],probs=0.9,na.rm=T),
                 accel_0.95=quantile(accel[-c((.N-50):(.N))],probs=0.95,na.rm=T),
                 accel_1.00=quantile(accel[-c((.N-50):(.N))],probs=1,na.rm=T)),
           by=list(user,trip)]
      
    } 
    data <- acceleration_quantiles(data)
    
    #Angle/Speed Quantile
    
    angle_speed_quantiles <- function(data){
      data[,`:=`(angle_per_unit_speed_0.05 = quantile(angle_per_unit_speed[-c((.N-50):(.N))],probs=0.05,na.rm=T),
                 angle_per_unit_speed_0.1 = quantile(angle_per_unit_speed[-c((.N-50):(.N))],probs=0.1,na.rm=T),
                 angle_per_unit_speed_0.15 = quantile(angle_per_unit_speed[-c((.N-50):(.N))],probs=0.15,na.rm=T),
                 angle_per_unit_speed_0.2 = quantile(angle_per_unit_speed[-c((.N-50):(.N))],probs=0.20,na.rm=T),
                 angle_per_unit_speed_0.25 = quantile(angle_per_unit_speed[-c((.N-50):(.N))],probs=0.25,na.rm=T),
                 angle_per_unit_speed_0.3 = quantile(angle_per_unit_speed[-c((.N-50):(.N))],probs=0.3,na.rm=T),
                 angle_per_unit_speed_0.35 = quantile(angle_per_unit_speed[-c((.N-50):(.N))],probs=0.35,na.rm=T),
                 angle_per_unit_speed_0.4 = quantile(angle_per_unit_speed[-c((.N-50):(.N))],probs=0.4,na.rm=T),
                 angle_per_unit_speed_0.45 = quantile(angle_per_unit_speed[-c((.N-50):(.N))],probs=0.45,na.rm=T),
                 angle_per_unit_speed_0.5 = quantile(angle_per_unit_speed[-c((.N-50):(.N))],probs=0.5,na.rm=T),
                 angle_per_unit_speed_0.55 = quantile(angle_per_unit_speed[-c((.N-50):(.N))],probs=0.55,na.rm=T),
                 angle_per_unit_speed_0.6 = quantile(angle_per_unit_speed[-c((.N-50):(.N))],probs=0.6,na.rm=T),
                 angle_per_unit_speed_0.65 = quantile(angle_per_unit_speed[-c((.N-50):(.N))],probs=0.65,na.rm=T),
                 angle_per_unit_speed_0.7 = quantile(angle_per_unit_speed[-c((.N-50):(.N))],probs=0.7,na.rm=T),
                 angle_per_unit_speed_0.75 = quantile(angle_per_unit_speed[-c((.N-50):(.N))],probs=0.75,na.rm=T),
                 angle_per_unit_speed_0.8 = quantile(angle_per_unit_speed[-c((.N-50):(.N))],probs=0.8,na.rm=T),
                 angle_per_unit_speed_0.85 = quantile(angle_per_unit_speed[-c((.N-50):(.N))],probs=0.85,na.rm=T),
                 angle_per_unit_speed_0.9 = quantile(angle_per_unit_speed[-c((.N-50):(.N))],probs=0.9,na.rm=T),
                 angle_per_unit_speed_0.95 = quantile(angle_per_unit_speed[-c((.N-50):(.N))],probs=0.95,na.rm=T),
                 angle_per_unit_speed_1.00 = quantile(angle_per_unit_speed[-c((.N-50):(.N))],probs=1,na.rm=T)),
           by=list(user,trip)]
    }
    #data<-angle_speed_quantiles(data)
    
    
    data[,lag.x:=NULL]
    data[,lag.y:=NULL]
    data[,lead.x:=NULL]
    data[,lead.y:=NULL]
    data[,x:=NULL]
    data[,y:=NULL]
    data[,lag.speed:=NULL]
    data[,accel:=NULL]
    data[,jerk:=NULL]
    data[,lag.accel:=NULL]
    data[,dist_segment:=NULL]
    data[,angle:=NULL]
    data[,angle_per_unit_speed:=NULL]
    data[,angle_per_unit_speed_count:=NULL]
    
    #Aggregation
    data_agg<-data[,lapply(.SD,mean,na.rm=T),by=c('user','trip')]
    return(data_agg)
  } 
  
  data_list[[as.character(trip)]]<- ldply(.data=dirs,
                                       .parallel=F,
                                       .fun=agg,trip)
  
}
data_aggregate <- ldply(.data = data_list,.fun = data.frame)