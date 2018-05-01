library(xml2)
library(rvest)
library(dplyr)
library(stringr)

data_information_2=""
code = "0050"
data_information_2 <- matrix(ncol=4)

Y = c(92:106)
M = c(paste("0", 1:9, sep=""), 10:12)


for (i in 1:length(Y))
{
  
  for (j in 1:length(M))
  {
    
    #web <- read_html("https://stock.wearn.com/netbuy.asp?Year=106&month=08&kind=0050")
    web <- read_html(paste("http://stock.wearn.com/acredit.asp?Year=", Y[i],"&month=", M[j],"&kind=", code, sep=""))
    stock_name1<-web%>%html_nodes(".stockalllistbg1 td, .stockalllistbg2 td")%>%html_text()
    stock_name1<-str_replace_all(stock_name1,"\r\n\t","")
    stock_name1<-str_replace_all(stock_name1,",","")
    stock_name1<-str_replace_all(stock_name1,"%","")
    #stock_name1<-str_replace_all(stock_name1,"\\s","") #¥h±¼html¸Ì"<U+00A0>"¶Ã½X
    x <- length(stock_name1)

    while (x > 0)
    {
      info_1 = stock_name1[x-7]
      info_2 = unlist(strsplit(stock_name1[x-5], "\\s"))
      info_3 = unlist(strsplit(stock_name1[x-3], "\\s"))
      info_4 = as.numeric(str_replace_all(stock_name1[x-1], "\\s", ""))*0.01
      data_information_2 <- rbind(data_information_2, c(info_1, info_2[1], info_3[1], info_4))
      x<-x-8

    }
    
  }
  
}

data_information_2 <- data_information_2[-1,]
#write.csv(data_information, file="C:\\users\\Administrator\\Desktop\\test_3.csv")
