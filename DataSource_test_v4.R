
statistics_1 <- matrix(ncol=3)

for (i in 2:length(statistics[,1]))
{
  
  BF<-DataResolve(statistics[i,1],"3Y")
  KDV <- KD()
  
  if (KDV[length(KDV[,1]),2] < 30 &  KDV[length(KDV[,1]),3] < 30)
  {
    
    statistics_1 <- rbind(statistics_1,c(statistics[i,1],KDV[length(KDV[,1]),2],KDV[length(KDV[,1]),3]))
    
  }
  
}
