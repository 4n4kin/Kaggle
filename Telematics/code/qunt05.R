l <-list.files("./drivers")


substrEnd <- function(x, n){
  substr(x,start = 1,stop = nchar(x)-n)
}
loopTo <- substrEnd(l,4)
df <- data.frame()
for (i in loopTo)  
{
  print(i)
  data <- read.csv(paste0(i,".csv"))
  t <- data.frame(table(data$drive))
  q05 <-quantile(t$Freq,c(0.05))
  #print(q05)
  for (j in unique(data$drive))
  {
    if (nrow(data[data$drive == j,])< q05){
      vec <- cbind(paste0(i,"_",j),0)
      df <- rbind(df,vec)
    }
    else
    {
      vec <- cbind(paste0(i,"_",j),1)
      df <- rbind(df,vec)      
    }
  }
  
  
}

