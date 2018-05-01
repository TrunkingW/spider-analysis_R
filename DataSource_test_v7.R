library(xml2)
library(rvest)
library(stringr)
library(lubridate)

data_information=""
data_r =""
data_r_n = 15
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
    
    date_rang = c(date_rang, paste(date_start[i], "&todate=",date_stop[i], sep=""))
    
  }
  
  date_rang = date_rang[-1]
  
  return(date_rang)
}

data_r = get_date_fun(data_r_n)


for (i in 1:data_r_n)
{

  web <- read_html(paste("http://estock.marbo.com.tw/asp/v_loa2.asp?T=&id=", code, "&frdate=", data_r[i], sep=""))
  #web <- read_html("http://estock.marbo.com.tw/asp/v_loa2.asp?T=&id=0050&frdate=2017/09/01&todate=2017/010/30")
  stock_name1<-web%>%html_nodes("table")%>%html_nodes("tr")%>%html_nodes("td")%>%html_text()
  stock_name1<-str_replace_all(stock_name1,",","")
  x<-length(stock_name1)
  stock_name1 <- stock_name1[-c(1:19,x,x-1)]
  x<-length(stock_name1)
 
  while (x > 0)
  {
    
    data_information <- rbind(data_information, c(stock_name1[x-19], stock_name1[x-18], stock_name1[x-16], stock_name1[x-12], stock_name1[x-9], stock_name1[x-7], stock_name1[x-3], as.numeric(stock_name1[x])*0.01))
    x<-x-20
    
  }
  
   
}

data_information <- data_information[-1,]
write.csv(data_information, file="C:\\users\\Administrator\\Desktop\\test_3.csv")
