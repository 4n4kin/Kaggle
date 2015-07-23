# We'll convert all the characters to factors so we can train a randomForest model on them
extractFeatures <- function(data) {
  character_cols <- names(Filter(function(x) x=="character", sapply(data, class)))
  for (col in character_cols) {
    data[,col] <- as.factor(data[,col])
  }
  return(data)
}

trainFea <- extractFeatures(train)
testFea  <- extractFeatures(test)



trainFea <- extractFeatures(train)
testFea  <- extractFeatures(test)

library(randomForest)

set.seed(42)
model00_rf <- randomForest(trainFea[,3:34], trainFea$Hazard,
                           ntree=100, imp=TRUE,corr.bias = T)

cat("Making predictions\n")
submission <- data.frame(Id=test$Id)
submission$Hazard <- predict(rf, extractFeatures(testFea[,2:33]))
write_csv(submission, "1_random_forest_benchmark.csv")
