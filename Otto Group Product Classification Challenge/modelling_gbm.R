library(ggplot2)
library(ggvis)
library(randomForest)
library(reshape2)
library(dismo)

train$target <- as.numeric(factor(train$target))


m1 <- gbm.step(gbm.x =2:94 ,gbm.y =95,learning.rate = 0.03,data = train,family='poisson')

result <- predict(object = m1,newdata=test)
results <- predict(object = m1,test)


results<- as.character(results)
results<-data.frame(id=test$id,class=results )
submission<-dcast(data = results,id~class,length)
write.csv(submission,file = 'submissions/submission02_gbm.csv',quote = F,row.names = F)
