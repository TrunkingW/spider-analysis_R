library(xml2)
library(rvest)
library(stringr)
library(lubridate)

data_inf=""
data_r =""
data_r_n = 1
code = "0050"

data_inf <- matrix(ncol=5)

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

for (i in 1:data_r_n)
{
  
  web <- read_html(paste("http://easyfun.concords.com.tw/z/zc/zcl/zcl.djhtm?a=", code, "&c=", data_r[i], sep=""))
  stock_name1<-web%>%html_nodes(".t3n1, .t3r1, .t3n0")%>%html_text()
  stock_name1<-str_replace_all(stock_name1,",","")
  x<-length(stock_name1)
  stock_name1 <- stock_name1[-c(1,x,x-1,x-2,x-3,x-4,x-5)]
  x<-length(stock_name1)
  
  #data_inf <- matrix(c(stock_name1[x-10], stock_name1[x-9], stock_name1[x-8], stock_name1[x-7], stock_name1[x-6]), ncol = 5)
  #x<-x-11
  
  while (x > 0)
  {
    
    data_inf <- rbind(data_inf, c(stock_name1[x-10], stock_name1[x-9], stock_name1[x-8], stock_name1[x-7], stock_name1[x-6]))
    x<-x-11
    
  }
  
}

data_inf <- data_inf[-1,]

write.csv(data_inf, file="C:\\users\\Administrator\\Desktop\\test_1.csv")
