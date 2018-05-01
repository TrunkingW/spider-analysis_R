library(stringr)

data_mix = ""
data_mix <- matrix(ncol=4)
tag_j = 1
tag_k = 1

for(i in 1:length(Buffer[,1]))
{
  
  Buffer_str = str_replace_all(Buffer[i]," ","")
  status = TRUE
  
  for (j in tag_j:length(data_information[,1]))
  {
    
    if(Buffer_str == paste(as.numeric(substr(data_information[j],1,4))-1911 , "/", substr(data_information[j], 6, 7), "/", substr(data_information[j], 9, 10),sep=""))
    {
      
      data_mix <- rbind(data_mix, c(Buffer_str, data_information[j,4], data_information[j,7], data_information[j,8]))
      status = FALSE
      tag_j = j
      break
      
    }
    
  }
  
  if (status)
  {
    
    for (k in tag_k:length(data_information_2[,1]))
    {
      
      if(Buffer_str == data_information_2[k])
      {
        
        data_mix <- rbind(data_mix, c(Buffer_str, data_information_2[k,2], data_information_2[k,3], data_information_2[k,4]))
        tag_k = k
        break
        
      }
      
    }
    
  }
  
}

data_mix <- data_mix[-1,]
write.csv(data_mix, file="C:\\users\\Administrator\\Desktop\\test_3.csv")
