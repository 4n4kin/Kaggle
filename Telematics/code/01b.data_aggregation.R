library(plyr)
library(data.table)
library(stringr)

dirs <- list.dirs('./drivers/drivers/')[-1]

data_list<-vector(mode='list',length=200)
print('File list created')

names(data_list) <- 1:200

for(trip in 1:200){ 
  print(trip)
  agg <- function(filepath,trip){
    library(data.table)
    data <- fread(paste(filepath,'/',trip,'.csv',sep=''))
    data$user<-gsub(x=filepath,pattern='./drivers/drivers//',replacement='')
    data$trip<-as.character(trip)
    
    setcolorder(x=data,neworder=c('user','trip','x','y'))
    
    #Create Lag variables : 3
    data[, lag.x:=c(0, x[-.N]), by=c('user','trip')]
    data[, lag.y:=c(0, y[-.N]), by=c('user','trip')]
    data[,lead.x:=c(x[-1],x[.N]),by=c('user','trip')]
    data[,lead.y:=c(y[-1],y[.N]),by=c('user','trip')]
    
    #Distance per segment, speed,acceleration,jerk
    
    data[,speed := sqrt( (y-lag.y)^2 + (x-lag.x)^2 ),
         by=c('user','trip')]
    
    data[,lag.speed:=c(speed[1], speed[-.N]),
         by=c('user','trip')]
    data[,accel:= c(0,0,(speed - lag.speed)[3:.N]),
         by=c('user','trip')]
    data[, lag.accel:=c(accel[1], accel[-.N]),
         by=c('user','trip')]
    data[, jerk:= c(0,0,0,(accel - lag.accel)[4:.N]),
         by=c('user','trip')]
    data[,power:=abs(accel * speed),
         by=c('user','trip')]
    #Angle
    data[,dot_3:=( ( (lag.x-x)*(lead.x-x) + (lag.y - y)*(lead.y-y) ) /
                     ( (sqrt((lag.x - x)^2+ ((lag.y -y)^2 ))) * (sqrt((lead.x-x)^2+(lead.y-y)^2)) ) ),
         by=c('user','trip')]
    
    con = data$dot==1
    data[,angle_3:=ifelse(test=con,yes=pi,no=acos(dot_3)),
         by=c('user','trip')]
    
    data[,lag.x:=NULL]
    data[,lag.y:=NULL]
    data[,lead.x:=NULL]
    data[,lead.y:=NULL]
    
    
    #Create Lag variables : 5
    data[, lag.x:=c(0,0, x[1:(.N-2)]), by=c('user','trip')]
    data[, lag.y:=c(0,0, y[1:(.N-2)]), by=c('user','trip')]
    data[, lead.x:=c(x[3:.N],x[(.N-1):.N]), by=c('user','trip')]
    data[, lead.y:=c(y[3:.N],y[(.N-1):.N]), by=c('user','trip')]
    
    #Angle
    data[,dot_5:=( ( (lag.x-x)*(lead.x-x) + (lag.y - y)*(lead.y-y) ) /
                     ( (sqrt((lag.x - x)^2+ ((lag.y -y)^2 ))) * (sqrt((lead.x-x)^2+(lead.y-y)^2)) ) ),
         by=c('user','trip')]
    
    con = data$dot_5==1
    data[,angle_5:=ifelse(test=con,yes=pi,no=acos(dot_5)),
         by=c('user','trip')]
    
    data[,lag.x:=NULL]
    data[,lag.y:=NULL]
    data[,lead.x:=NULL]
    data[,lead.y:=NULL]
    
    
    #Create Lag variables : 7
    data[, lag.x:=c(0,0,0, x[1:(.N-3)]), by=c('user','trip')]
    data[, lag.y:=c(0,0,0, y[1:(.N-3)]), by=c('user','trip')]
    data[,lead.x:=c(x[4:.N],x[(.N-2):.N]),by=c('user','trip')]
    data[,lead.y:=c(y[4:.N],y[(.N-2):.N]),by=c('user','trip')]
    
    
    #Angle
    data[,dot_7:=( ( (lag.x-x)*(lead.x-x) + (lag.y - y)*(lead.y-y) ) /
                     ( (sqrt((lag.x - x)^2+ ((lag.y -y)^2 ))) * (sqrt((lead.x-x)^2+(lead.y-y)^2)) ) ),
         by=c('user','trip')]
    
    con = data$dot_7==1
    data[,angle_7:=ifelse(test=con,yes=pi,no=acos(dot_7)),
         by=c('user','trip')]
    
    data[,lag.x:=NULL]
    data[,lag.y:=NULL]
    data[,lead.x:=NULL]
    data[,lead.y:=NULL]
    #Distance
    data[,`:=`(entries = length(speed),
               dist_total_trip = sum(speed,na.rm=T),
               dist_first10 = sum(speed[1:10],na.rm=T),
               dist_first5 = sum(speed[1:5],na.rm=T),
               speed_avg = mean(speed,na.rm=T),
               zero_count = sum(tail(speed==0,100),na.rm=T),
               jerk_count_positive = sum(jerk>=3),
               jerk_count_negative = sum(jerk<=(-3)),
               angle_per_unit_speed_3 = angle_3/speed,
               angle_per_unit_speed_5 = angle_5/speed,
               angle_per_unit_speed_7 = angle_7/speed),
         by=c('user','trip')]
    
    
    #Power Quantiles
    power_quantiles<-function(data){
      data[,`:=`(power_0.05=quantile(power,probs=0.05,na.rm=T),
                 power_0.1=quantile(power,probs=0.1,na.rm=T),
                 power_0.15=quantile(power,probs=0.15,na.rm=T),
                 power_0.2=quantile(power,probs=0.20,na.rm=T),
                 power_0.25=quantile(power,probs=0.25,na.rm=T),
                 power_0.3=quantile(power,probs=0.3,na.rm=T),
                 power_0.35=quantile(power,probs=0.35,na.rm=T),
                 power_0.4=quantile(power,probs=0.4,na.rm=T),
                 power_0.45=quantile(power,probs=0.45,na.rm=T),
                 power_0.5=quantile(power,probs=0.5,na.rm=T),
                 power_0.55=quantile(power,probs=0.55,na.rm=T),
                 power_0.6=quantile(power,probs=0.6,na.rm=T),
                 power_0.65=quantile(power,probs=0.65,na.rm=T),
                 power_0.7=quantile(power,probs=0.7,na.rm=T),
                 power_0.75=quantile(power,probs=0.75,na.rm=T),
                 power_0.8=quantile(power,probs=0.8,na.rm=T),
                 power_0.85=quantile(power,probs=0.85,na.rm=T),
                 power_0.9=quantile(power,probs=0.9,na.rm=T),
                 power_0.95=quantile(power,probs=0.95,na.rm=T),
                 power_1.00=quantile(power,probs=1,na.rm=T)),
           by=list(user,trip)]
    }
    data<-power_quantiles(data)
    
    
    #Calculating Angle/Speed
    anglebyspeed <- function(data){
      #Angle/Speed : 3 Secs
      data$angle_per_unit_speed_3[is.na(data$angle_per_unit_speed_3)] <- 1
      
      data[,angle_per_unit_speed_count_3:=sum(angle_per_unit_speed_3<=0.1),
           by=list(user,trip)]
      data[,angle_per_unit_speed_fraction_3:=angle_per_unit_speed_count_3/entries,
           by=list(user,trip)]
      
      #Angle /Speed : 5 Secs
      data$angle_per_unit_speed_5[is.na(data$angle_per_unit_speed_5)] <- 1
      
      data[,angle_per_unit_speed_count_5:=sum(angle_per_unit_speed_5<=0.1),
           by=list(user,trip)]
      data[,angle_per_unit_speed_fraction_5:=angle_per_unit_speed_count_5/entries,
           by=list(user,trip)]
      
      #Angle/Speed : 7 Secs
      data$angle_per_unit_speed_7[is.na(data$angle_per_unit_speed_7)] <- 1
      
      data[,angle_per_unit_speed_count_7:=sum(angle_per_unit_speed_7<=0.1),
           by=list(user,trip)]
      data[,angle_per_unit_speed_fraction_7:=angle_per_unit_speed_count_7/entries,
           by=list(user,trip)]
      return(data)
    }
    data <- anglebyspeed(data)
    
    #Speed Quantiles
    speed_quantiles<-function(data){
      data[,speed:=speed*speed]
      data[,`:=`(speed_0.05=quantile(speed,probs=0.05,na.rm=T),
                 speed_0.1=quantile(speed,probs=0.1,na.rm=T),
                 speed_0.15=quantile(speed,probs=0.15,na.rm=T),
                 speed_0.2=quantile(speed,probs=0.20,na.rm=T),
                 speed_0.25=quantile(speed,probs=0.25,na.rm=T),
                 speed_0.3=quantile(speed,probs=0.3,na.rm=T),
                 speed_0.35=quantile(speed,probs=0.35,na.rm=T),
                 speed_0.4=quantile(speed,probs=0.4,na.rm=T),
                 speed_0.45=quantile(speed,probs=0.45,na.rm=T),
                 speed_0.5=quantile(speed,probs=0.5,na.rm=T),
                 speed_0.55=quantile(speed,probs=0.55,na.rm=T),
                 speed_0.6=quantile(speed,probs=0.6,na.rm=T),
                 speed_0.65=quantile(speed,probs=0.65,na.rm=T),
                 speed_0.7=quantile(speed,probs=0.7,na.rm=T),
                 speed_0.75=quantile(speed,probs=0.75,na.rm=T),
                 speed_0.8=quantile(speed,probs=0.8,na.rm=T),
                 speed_0.85=quantile(speed,probs=0.85,na.rm=T),
                 speed_0.9=quantile(speed,probs=0.9,na.rm=T),
                 speed_0.95=quantile(speed,probs=0.95,na.rm=T),
                 speed_1.00=quantile(speed,probs=1,na.rm=T)),
           by=list(user,trip)]
    }
    data<-speed_quantiles(data)
    
    #Acceleration Quantiles
    acceleration_quantiles<-function(data){
      data[,`:=`(accel_0.05=quantile(accel,probs=0.05,na.rm=T),
                 accel_0.1=quantile(accel,probs=0.1,na.rm=T),
                 accel_0.15=quantile(accel,probs=0.15,na.rm=T),
                 accel_0.2=quantile(accel,probs=0.20,na.rm=T),
                 accel_0.25=quantile(accel,probs=0.25,na.rm=T),
                 accel_0.3=quantile(accel,probs=0.3,na.rm=T),
                 accel_0.35=quantile(accel,probs=0.35,na.rm=T),
                 accel_0.4=quantile(accel,probs=0.4,na.rm=T),
                 accel_0.45=quantile(accel,probs=0.45,na.rm=T),
                 accel_0.5=quantile(accel,probs=0.5,na.rm=T),
                 accel_0.55=quantile(accel,probs=0.55,na.rm=T),
                 accel_0.6=quantile(accel,probs=0.6,na.rm=T),
                 accel_0.65=quantile(accel,probs=0.65,na.rm=T),
                 accel_0.7=quantile(accel,probs=0.7,na.rm=T),
                 accel_0.75=quantile(accel,probs=0.75,na.rm=T),
                 accel_0.8=quantile(accel,probs=0.8,na.rm=T),
                 accel_0.85=quantile(accel,probs=0.85,na.rm=T),
                 accel_0.9=quantile(accel,probs=0.9,na.rm=T),
                 accel_0.95=quantile(accel,probs=0.95,na.rm=T),
                 accel_1.00=quantile(accel,probs=1,na.rm=T)),
           by=list(user,trip)]
    } 
    data <- acceleration_quantiles(data)
    
    #Angle/Speed Quantile  
    angle_speed_quantiles <- function(data){
      data[,`:=`(angle_per_unit_speed_0.05 = quantile(angle_per_unit_speed,probs=0.05,na.rm=T),
                 angle_per_unit_speed_0.1 = quantile(angle_per_unit_speed,probs=0.1,na.rm=T),
                 angle_per_unit_speed_0.15 = quantile(angle_per_unit_speed,probs=0.15,na.rm=T),
                 angle_per_unit_speed_0.2 = quantile(angle_per_unit_speed,probs=0.20,na.rm=T),
                 angle_per_unit_speed_0.25 = quantile(angle_per_unit_speed,probs=0.25,na.rm=T),
                 angle_per_unit_speed_0.3 = quantile(angle_per_unit_speed,probs=0.3,na.rm=T),
                 angle_per_unit_speed_0.35 = quantile(angle_per_unit_speed,probs=0.35,na.rm=T),
                 angle_per_unit_speed_0.4 = quantile(angle_per_unit_speed,probs=0.4,na.rm=T),
                 angle_per_unit_speed_0.45 = quantile(angle_per_unit_speed,probs=0.45,na.rm=T),
                 angle_per_unit_speed_0.5 = quantile(angle_per_unit_speed,probs=0.5,na.rm=T),
                 angle_per_unit_speed_0.55 = quantile(angle_per_unit_speed,probs=0.55,na.rm=T),
                 angle_per_unit_speed_0.6 = quantile(angle_per_unit_speed,probs=0.6,na.rm=T),
                 angle_per_unit_speed_0.65 = quantile(angle_per_unit_speed,probs=0.65,na.rm=T),
                 angle_per_unit_speed_0.7 = quantile(angle_per_unit_speed,probs=0.7,na.rm=T),
                 angle_per_unit_speed_0.75 = quantile(angle_per_unit_speed,probs=0.75,na.rm=T),
                 angle_per_unit_speed_0.8 = quantile(angle_per_unit_speed,probs=0.8,na.rm=T),
                 angle_per_unit_speed_0.85 = quantile(angle_per_unit_speed,probs=0.85,na.rm=T),
                 angle_per_unit_speed_0.9 = quantile(angle_per_unit_speed,probs=0.9,na.rm=T),
                 angle_per_unit_speed_0.95 = quantile(angle_per_unit_speed,probs=0.95,na.rm=T),
                 angle_per_unit_speed_1.00 = quantile(angle_per_unit_speed,probs=1,na.rm=T)),
           by=list(user,trip)]
    }
    #data<-angle_speed_quantiles(data)
    
    
    
    data[,dot_3:=NULL]
    data[,dot_5:=NULL]
    data[,dot_7:=NULL]
    data[,x:=NULL]
    data[,y:=NULL]
    data[,speed:=NULL]
    data[,lag.speed:=NULL]
    data[,accel:=NULL]
    data[,lag.accel:=NULL]
    data[,jerk:=NULL]
    data[,angle_3:=NULL]
    data[,angle_5:=NULL]
    data[,angle_7:=NULL]
    data[,angle_per_unit_speed_3:=NULL]
    data[,angle_per_unit_speed_5:=NULL]
    data[,angle_per_unit_speed_7:=NULL]
    data[,angle_per_unit_speed_count_3:=NULL]
    data[,angle_per_unit_speed_count_5:=NULL]
    data[,angle_per_unit_speed_count_7:=NULL]
    
    #Aggregation
    data_agg<-data[,lapply(.SD,mean,na.rm=T),by=c('user','trip')]
    return(data_agg)
  }
  
  data_list[[as.character(trip)]]<- ldply(.data=dirs,.progress='text',
                                          .parallel=F,
                                          .fun=agg,trip)
  gc()
  
}

data_aggregate_new<-ldply(data_list,data.frame)
data_aggregate_new$.id<-NULL
colnames(data_aggregate_new)[1] <- 'driver'
