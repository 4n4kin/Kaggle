#dirs <- list.dirs(path='./drivers')[-1]

library(doParallel)
library(plyr)

nodes <- detectCores()
cl <- makeCluster(nodes)
registerDoParallel(cl)

files <- lapply(dirs,FUN=function(y){
  library(plyr)
  print(y)
  file_list <- list.files(path=y,pattern='.csv')
  
  dataset<- adply(paste(y,file_list,sep='/'),
                  .parallel=T,
                  1,
                read.csv)
  dataset$user <- y
  write.table(dataset,
              file=paste(y,'_combined.txt',sep=''),
              row.names=F,
              quote=F,
              sep=',')
  rm(dataset)
}
)


stopCluster(cl)