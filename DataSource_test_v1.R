library(RCurl)
library(stringr)
library(rjson)
library(lubridate)

n = 1

code = "0050"
DBuffer = 12*14+2
DBuffer = gsub("-","",rev(Sys.Date() %m-% months(c(0:DBuffer))))

for (j in 1:length(DBuffer))
{
  
  x <- paste("http://www.twse.com.tw/exchangeReport/STOCK_DAY?response=json&date=",DBuffer[j],"&stockNo=",code,sep="")
  x <- getURL(x)
  x <- fromJSON(x)
  
  
  for (i in 1:length(x$data))
  {
    
    x$data[[i]][2] <- as.numeric(gsub(",","",x$data[[i]][2]))
    x$data[[i]][3] <- as.numeric(gsub(",","",x$data[[i]][3]))
    
    
    if (x$data[[i]][8] == "X0.00")
    {
      
      x$data[[i]][8] = 0.00
      
    }
    
    if (n>1)
    {
      
      
      x$data[[i]][8] <- paste(as.numeric(x$data[[i]][8]) / as.numeric(gsub(",","",Buffer[n-1,3])),"",sep="")
      Buffer[n-1,9] <- x$data[[i]][5]
      Buffer <- rbind(Buffer,c(x$data[[i]][1],x$data[[i]][4],x$data[[i]][7],x$data[[i]][6],x$data[[i]][5],x$data[[i]][2],x$data[[i]][3],x$data[[i]][8],x$data[[i]][9]))
      
    }
    else
    {
      
      Buffer <- matrix(c(x$data[[i]][1],x$data[[i]][4],x$data[[i]][7],x$data[[i]][6],x$data[[i]][5],x$data[[i]][2],x$data[[i]][3],x$data[[i]][8],x$data[[i]][9]), ncol = 9)
      
    }  
    
    n = n + 1
    
  }

  Sys.sleep(1)
  
}

KDV <- KD()

write.csv(Buffer, file="C:\\users\\Administrator\\Desktop\\tt.csv")
#write.csv(KDV, file="C:\\users\\Administrator\\Desktop\\test_2.csv")
