a <-matrix(c(1,2),ncol = 2)
a <- rbind(a,1:2)
write.csv(b, file="C:\\users\\Administrator\\Desktop\\test.csv")
c <-read.csv(file="C:\\users\\Administrator\\Desktop\\test.csv")
c <- c[-1,]

library(RCurl)
x=getURL("http://www.google.com/finance/getprices?q=1101&x=TPE&i=86400&p=100d&f=d,c,h,l,o,v")
x<-unlist(strsplit(x,"\n"))
a<-rbind(a,x[1])


BF <- DataResolve("1101","2Y")



as.Date(as.POSIXct(1414040400, origin="1970-01-01")) 
