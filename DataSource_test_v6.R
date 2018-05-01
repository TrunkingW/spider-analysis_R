library(xml2)
library(rvest)
library(stringr)
library(lubridate)

data_information=""
data_r =""
data_r_n = 1
code = "0050"

data_information <- matrix(ncol=8)

get_date_fun <- function(date_rang)
{
  
  date_rang=date_rang-1
  x = paste(as.numeric(format(Sys.Date(), '%Y')) - date_rang, "", sep="")
  date_start <- seq.Date(from = as.Date(paste(x, "/01/01", sep=""),format = "%Y/%m/%d"), by = "year", length.out = date_rang+1)
  date_stop <- seq.Date(from = as.Date(paste(x, "/12/31", sep=""),format = "%Y/%m/%d"), by = "year", length.out = date_rang+1)
  
  for (i in 1:length(date_start))
  {
    
    date_rang = c(date_rang, paste(date_start[i], "&d=",date_stop[i], sep=""))
    
  }
  
  date_rang = date_rang[-1]
  
  return(date_rang)
}

data_r = get_date_fun(data_r_n)

#for (i in 1:data_r_n)
#{
  
  #web <- read_html(paste("http://easyfun.concords.com.tw/z/zc/zcn/zcn.djhtm?a=", code, "&c=", data_r[i], sep=""))
  web <- read_html("http://easyfun.concords.com.tw/z/zc/zcn/zcn.djhtm?a=0050&c=2017-08-01&d=2017-12-31")
  stock_name1<-web%>%html_nodes(".t3n1, .t3r1, .t3n0")%>%html_text()
  stock_name1<-str_replace_all(stock_name1,",","")
  stock_name1<-str_replace_all(stock_name1,"%","")
  x<-length(stock_name1)
  stock_name1 <- stock_name1[-c(1,x,x-1)]
  x<-length(stock_name1)
  
  while (x > 0)
  {
    
    data_information <- rbind(data_information, c(stock_name1[x-14], stock_name1[x-13], stock_name1[x-12], stock_name1[x-9], stock_name1[x-6], stock_name1[x-5], stock_name1[x-2], as.numeric(stock_name1[x-1])*0.01))
    x<-x-15
    
  }
   
  data_information <- data_information[-1,]
  
#}