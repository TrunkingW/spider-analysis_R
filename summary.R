
Formula <- function()  
{
  
  Status = FALSE
  IN_T = ""
  IN_H = 0
  OUT_T = ""
  OUT_L = 0
  PAL = 0
  summary <- matrix(ncol=5,dimnames=list(NULL,c("IN_T","OUT_T","IN_H","OUT_L","PAL")))
  
  for (i in 20:length(KDV[,1]))
  {
    
    if (length(KDV) < 6)
    {
      break 
    }
    
    if (Status)
    {
      
      if (KDV[i,2] < KDV[i,3])
      {
        
        Status = FALSE
        OUT_T = KDV[i,1]
        OUT_L = BF[i,4]
        PAL = as.numeric(OUT_L) - as.numeric(IN_H)
        summary <- rbind(summary,c(IN_T,OUT_T,IN_H,OUT_L,PAL))
        
      }
      
    }
    else
    {
      
      if (KDV[i,2] < 20 & KDV[i,3] < 20 & KDV[i,2] > KDV[i,3])
      {
        
        Status = TRUE
        IN_T = KDV[i,1]
        IN_H = as.numeric(BF[i,3])
        
      }
      
    }
    
  }
  
  summary <- summary[-1,]
  
  return(summary)
}

MA <- function(tag_MA)
{
  
  MA = 0
  
  for (i in 2:length(Buffer[,3]))
  {
    
    if (i >= tag_MA)
    {
      
      MA = c(MA, sum(as.numeric(Buffer[(i-(tag_MA-1)):i,3])) / tag_MA)
      
    }
    else
    {
      
      MA = c(MA, 0)
      
    }
    
  }
  
  return(MA)
  
}

BIAS <- function(tag_BIAS)
{
  
  BIAS = 0
  data_MA = MA(tag_BIAS)
  
  for (i in 2:length(Buffer[,3]))
  {
    
    if (i >= tag_BIAS)
    {
      
      BIAS = c(BIAS, (as.numeric(Buffer[i,3])-data_MA[i])/data_MA[i]*100)
      
    }
    else
    {
      
      BIAS = c(BIAS, 0)
      
    }
    
  }
  
  return(BIAS)
  
}

RSI <- function(tag_short)
{
  
  short = 0
  short_x = 0
  short_y = 0
  Short_Up = 0
  short_Down = 0
  
  
  for (i in 2:length(Buffer[,3]))
  {
    
    if (i-1 > tag_short)
    {
      
      if (Buffer[i,3] > Buffer[i-1,3])
      {
        
        short_x = as.numeric(Buffer[i,3]) - as.numeric(Buffer[i-1,3])
        short_y = 0
        
      }
      else
      {
        
        short_x = 0
        short_y = as.numeric(Buffer[i-1,3]) - as.numeric(Buffer[i,3])
        
      }
      
      Short_Up = ((Short_Up * (tag_short - 1)) + short_x) / tag_short
      short_Down = ((short_Down * (tag_short -1)) + short_y) / tag_short
      short_RSI = c(short_RSI, Short_Up / (Short_Up + short_Down) * 100)
      
    }
    else if(i-1 == tag_short)
    {
      
      if (Buffer[i,3] > Buffer[i-1,3])
      {
        
        short_x = short_x + as.numeric(Buffer[i,3]) - as.numeric(Buffer[i-1,3])
        
      }
      else
      {
        
        short_y = short_y + as.numeric(Buffer[i-1,3]) - as.numeric(Buffer[i,3])
        
      }
      
      Short_Up = short_x / tag_short
      short_Down = short_y / tag_short
      short_RSI = Short_Up / (Short_Up + short_Down) * 100
      
    }
    else
    {
      
      if (Buffer[i,3] > Buffer[i-1,3])
      {
        
        short_x = short_x + as.numeric(Buffer[i,3]) - as.numeric(Buffer[i-1,3])
        
      }
      else
      {
        
        short_y = short_y + as.numeric(Buffer[i-1,3]) - as.numeric(Buffer[i,3])
        
      }
      
    }
    
    
  }
  
   
  
  
}

KD <- function()
{
  
  K = ""
  D = ""
  KDV = ""
  
  for (i in 1:length(Buffer[,1]))
  {
    
    if (i <= 1)
    {
      
      R = RSV(i)
      K = 2/3 * 50 + 1/3 * R
      D = 2/3 * 50 + 1/3 * K
      KDV = matrix(round(c(K,D,R),3), ncol=3)
      
    }
    else
    {
      
      R = RSV(i)
      K = 2/3 * KDV[(i-1),1] + 1/3 * R
      D = 2/3 * KDV[(i-1),2] + 1/3 * K
      KDV = rbind(KDV,round(c(K,D,R),3))
      
    }
    
  }
  
  KDV <- cbind(Buffer[,1],KDV)
  
  return(KDV)
}

RSV <- function(tag)
{
  
  if (tag <= 9)
  {
    
    CN = as.numeric(Buffer[tag,3])
    LN = as.numeric(min(Buffer[1:tag,4]))
    HN = as.numeric(max(Buffer[1:tag,5]))
    
  }
  else
  {
    
    CN = as.numeric(Buffer[tag,3])
    LN = as.numeric(min(Buffer[(tag-8):tag,4]))
    HN = as.numeric(max(Buffer[(tag-8):tag,5]))
    
  }
  
  x=(CN-LN)/(HN-LN)*100
  
  return (x)
}


DataResolve <- function(code,DayT)
{
  
  TB <- ""
  n <- 9 
  
  x = paste("httP://www.google.com./finance/getprices?q=",code,"&x=TPE&i=86400&p=",DayT,"&f=d,o,h,l,c",sep="") 
  DRBuffer <- getURL(x)
  DRBuffer <- unlist(strsplit(DRBuffer,"\n"))
  x <- unlist(str_split(DRBuffer[8]))
  TB <- as.numeric(str_sub(x[1],2,nchar(x[1])))
  x[1] <- as.character(as.Date(as.POSIXct(TB, origin="1970-01-01")))
  DRBuffer_1 <- matrix(c(x[1:5]), ncol = 5)
  
  while (n <= length(DRBuffer))
  {
    
    
    x <- unlist(str_split(DRBuffer[n]))
    
    if (str_sub(x[1],1,1) == "a") 
    {
      
      TB <- as.numeric(str_sub(x[1],2,nchar(x[1])))
      x[1] <- as.character(as.Date(as.POSIXct(as.numeric(str_sub(x[1],2,nchar(x[1]))), origin="1970-01-01")))
      
    }
    else
    {
      
      x[1] <- as.character(as.Date(as.POSIXct(as.numeric(TB) + as.numeric(x[1]) * 86400, origin="1970-01-01")))
      
    }
    
    DRBuffer_1 <- rbind(DRBuffer_1,c(x[1:5]))
    n <- n  + 1
    
  }
  
  return(DRBuffer_1)
  
}

GetStockData <- function(code,DayT)
{
  
  n = 1
  
  DBuffer = 12*DayT
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
    
    Sys.sleep(10)
    
  }
  
  return(Buffer)
  
}

str_split <- function(DRstr)
{
  
  return(unlist(strsplit(DRstr,",")))
  
}
