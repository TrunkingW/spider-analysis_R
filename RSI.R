tag_short = 6
tag_long = 12

short = 0
short_x = 0
short_y = 0
Short_Up = 0
short_Down = 0

long = 0
long_x = 0
long_y = 0
long_Up = 0
long_Down = 0
long_RSI = 0


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
  
  if (i-1 > tag_long)
  {
    
    if (Buffer[i,3] > Buffer[i-1,3])
    {
      
      long_x = as.numeric(Buffer[i,3]) - as.numeric(Buffer[i-1,3])
      long_y = 0
      
    }
    else
    {
      
      long_x = 0
      long_y = as.numeric(Buffer[i-1,3]) - as.numeric(Buffer[i,3])
      
    }
    
    long_Up = ((long_Up * (tag_long - 1)) + long_x) / tag_long
    long_Down = ((long_Down * (tag_long -1)) + long_y) / tag_long
    long_RSI = c(long_RSI, long_Up / (long_Up + long_Down) * 100)
    
  }
  else if (i-1 == tag_long)
  {
    
    if (Buffer[i,3] > Buffer[i-1,3])
    {
      
      long_x = long_x + as.numeric(Buffer[i,3]) - as.numeric(Buffer[i-1,3])
      
    }
    else
    {
      
      long_y = long_y + as.numeric(Buffer[i-1,3]) - as.numeric(Buffer[i,3])
      
    }
    
    long_Up = long_x / tag_long
    long_Down = long_y / tag_long
    long_RSI = long_Up / (long_Up + long_Down) * 100
    
  }
  else
  {
    
    if (Buffer[i,3] > Buffer[i-1,3])
    {
      
      long_x = long_x + as.numeric(Buffer[i,3]) - as.numeric(Buffer[i-1,3])
      
    }
    else
    {
      
      long_y = long_y + as.numeric(Buffer[i-1,3]) - as.numeric(Buffer[i,3])
      
    }
    
  }
  
}

write.csv(short_RSI, file="C:\\users\\Administrator\\Desktop\\short_RSI.csv")
write.csv(long_RSI, file="C:\\users\\Administrator\\Desktop\\long_RSI.csv")
