dirs <- list.dirs(path='./drivers')[-1]

files <- lapply(dirs,FUN=function(y){
  library(plyr)
  print(y)
  file_list <- list.files(path=y,pattern='.csv')
  
  for(file in file_list){
    
    use <- paste(y,file,sep='/')
    
    if(exists('dataset')){
      
      temp_dataset <- read.table(use,
                                 header=T,
                                 sep=',')
      temp_dataset$ride <- file
      dataset <- rbind(dataset,temp_dataset)
      rm(temp_dataset)
    }
    
    else{
      
      dataset <- read.table(use,
                            header=T,
                            sep=',')
      dataset$ride <- file
      
    }
  }
  
  dataset$user <- y
  write.table(dataset,
              file=paste(y,'_combined.txt'),
              row.names=F,
              quote=F,
              sep='\t')
  rm(dataset) 
}
)