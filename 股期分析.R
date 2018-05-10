library(RCurl)
library(stringr)
library(rjson)
library(lubridate)

h_all_t = 0
h_me_t = 0
l_all_t = 0
l_me_t = 0
s_all_t = 0 #all_meet
s_me_t = 0 #all_meet

analysis <- matrix(ncol=7)

for (j in subject[,1])
{
  
  #Buffer <- GetStockData(j, 2)
  Buffer <- GetStockData("2548", 2)
  
  
  for (i in 2:length(Buffer[,1]))
  {
    
    ycl = as.numeric(Buffer[i-1,3])
    op = as.numeric(Buffer[i,2])
    cl = as.numeric(Buffer[i,3])
    lo = as.numeric(Buffer[i,4])
    hi = as.numeric(Buffer[i,5])
    
    mix_all_buff = 0 #bool
    mix_me_buff = 0 #bool
    
    if (hi/ycl - 1 > 0.05)
    {
      
      h_all_t = h_all_t + 1 
      mix_all_buff = mix_all_buff + 1
      
      if (op >= cl)
      {
        
        if ((hi/ycl - 1) - (op/ycl - 1) > 0.03)
        {
          
          h_me_t = h_me_t + 1
          mix_me_buff = mix_me_buff+1
          
        }
        
      }
      else
      {
        
        if ((hi/ycl - 1) - (cl/ycl - 1) > 0.03)
        {
          
          h_me_t = h_me_t + 1
          mix_me_buff = mix_me_buff+1
          
        }
      
      }
      
    }  
    
    if (lo/ycl -1 < -0.05)
    {
      
      l_all_t = l_all_t +1
      mix_all_buff = mix_all_buff +1
      
      if(op >= cl)
      {
        
        if((lo/ycl - 1) - (cl/ycl - 1) < -0.03)
        {
          
          l_me_t = l_me_t + 1
          mix_me_buff = mix_me_buff+1
          
        }
        
      }
      else
      {
        
        if((lo/ycl - 1) - (op/ycl - 1) < -0.03)
        {
          
          l_me_t = l_me_t + 1
          mix_me_buff = mix_me_buff + 1
          
        }
        
      }
      
    }
    
    if (mix_all_buff > 1)
    {
      
      s_all_t = s_all_t + 1
      
    }
    
    if (mix_me_buff > 1)
    {
      
      s_me_t = s_me_t + 1
      
    }
    
  }

  analysis <- rbind(analysis, length(subject[,1]), h_all_t, h_me_t, l_all_t, l_me_t, s_all_t, s_me_t)
  
}