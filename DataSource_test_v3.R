library(RCurl)
library(stringr)


statistics <- matrix(ncol=7)

x=read.table("C:\\Users/Administrator/Desktop/R_test/1050910.txt")

for (j in 1:length(x[,1]))
{
  
  BF<-DataResolve(x[j,1],"4Y")
  KDV <- KD()
  summary <- Formula()
  
  P = 0
  L = 0
  P_sum = 0
  L_sum = 0
  
  if (length(summary) > 9 & length(KDV) > 5 & length(BF) > 9)
  {
    
    
    for (i in 1:length(summary[,5]))
    {
      
      if (as.numeric(summary[i,5]) >= 0)
      {
        
        P = P + 1
        P_sum = P_sum + as.numeric(summary[i,5])
        
      }
      else
      {
        
        L = L + 1
        L_sum = L_sum + as.numeric(summary[i,5])
        
      }
      
    }
    
    WP = P / length(summary[,5]) * 100
    
    
    if (WP > 50 || P_sum + L_sum > 0)
    {
      
      statistics <- rbind(statistics,c(x[j,1],WP,P_sum,L_sum,round(P_sum + L_sum,3), P, L))
      
    }
  
  }
    
}

