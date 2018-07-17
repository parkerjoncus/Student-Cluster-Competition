library(ggplot2)
library(readr)
mydata <- read_csv("~/mydata.csv")
View(mydata)
mydata<-mydata[,-1]
mydata<-mydata[,-2]
mydata<-mydata[,-3]
mydata[,'time']<-0
Time=13.37 #change the amount of time depending on what the output of the hpl is
x=Time/length(mydata$Column)
mydata$time<-seq(0,Time,x)[-1]
ggplot(mydata, aes(x=Fraction, y=Gflops))+geom_point()+geom_line()
ggplot(mydata, aes(x=time, y=Gflops))+geom_point()+geom_line()