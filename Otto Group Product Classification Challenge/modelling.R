library(ggplot2)
library(ggvis)
library(randomForest)
library(reshape2)
library(plyr)
library(dplyr)

train$target <- factor(train$target)

set.seed(42)
m1 <- randomForest(target~.,data=train[,-1],ntree=200)
imp <- data.frame(vars=rownames(m1$importance),imp=m1$importance)
imp <- imp[order(-imp$MeanDecreaseGini),]
use <- as.character(imp$vars[1:15])



m1 <- randomForest(target~.,data=train[,c(use,'target')],ntree=500)
result <- predict(object = m1,newdata=test)


results<- as.character(result)
results<-data.frame(id=test$id,class=results )
submission<-dcast(data = results,id~class,length)
colnames(submission) <- c('id',paste('Class',1:9,sep = '_'))
write.csv(submission,file = 'submissions/submission04.csv',quote = F,row.names = F)
