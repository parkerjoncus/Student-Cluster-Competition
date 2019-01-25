library(readr)
library(sqldf)
library(ggplot2)
library(plyr)
library(plotly)
times1 <- read_csv("~/Documents/SCCgit/Assignment1/SkylakeICC/times.csv", col_names = FALSE)
hpl1 <- read_csv("~/Documents/SCCgit/Assignment1/SkylakeICC/data.csv")
power1 <- read_csv("~/Documents/SCCgit/Assignment1/SkylakeICC/powerdata.csv")
#clean power data
power1<-power1[complete.cases(power1),]
View(times1)
View(hpl1)
View(power1)
#change format of the times data frame so that start and end are separeate
times1$start<-NA
times1$end<-NA
x=1
y=1
for (cell in times1$X1){
  #check if it is a start time or end time
  if ((strsplit(cell, ':')[[1]][1])=='start'){
    times1$start[x]<-as.character(as.POSIXct(strsplit((strsplit(cell,'start: ')[[1]][2]), ' UTC')[[1]][1], tz= 'UTC', format = '%a %b %d %R:%S'))
    x=x+1
  }
  #put the time into correct column
  else{
    times1$end[y]<-as.character(as.POSIXct(strsplit((strsplit(cell,'end: ')[[1]][2]), ' UTC')[[1]][1], tz= 'UTC', format = '%a %b %d %R:%S'))
    y=y+1
  }
}
times1$X1<-NULL
times1<-times1[complete.cases(times1),]

#hpl data
power1$Time<-paste('2018-08-30',power1$Time, sep = ' ')
power1$Time<-as.POSIXct(power1$Time, tz = 'UTC' , format = '%Y-%M-%d %R:%S')
power1$Time[0:366]<-power1$Time[0:366]-24*60*60
times1$startnum<-as.numeric(as.POSIXct(times1$start))
times1$endnum<-as.numeric(as.POSIXct(times1$end))
power1$Timenum<-as.numeric(as.POSIXct(power1$Time))
temp<-sqldf('SELECT startnum,endnum,AVG(Watts) FROM times1, power1 WHERE Timenum>startnum AND Timenum<endnum GROUP BY startnum' )
times1<-join(times1,temp, by = c('startnum','endnum'))
hpl1$Watts<-NA
for (row in 1:dim(hpl1)[1]){
  hpl1$Watts[row]<-times1$`AVG(Watts)`[row]
}
#calculate gflops/watt
hpl1$`GflopsWatt`<-hpl1$Gflops/hpl1$Watts

ggplot(hpl1, aes(x=NB, y=Gflops)) + geom_point(aes(colour = factor(P))) + geom_line(aes(colour = factor(P))) + ggtitle('Gflops as NB increases and N=132000.')
ggplot(hpl1, aes(x=NB, y=Watts)) + geom_point(aes(colour = factor(P))) + geom_line(aes(colour = factor(P))) +ggtitle('Watts as NB increases and N=132000')
ggplot(hpl1, aes(x=NB, y=GflopsWatt)) + geom_point(aes(colour = factor(P))) + geom_line(aes(colour = factor(P))) +ggtitle('Gflops/Watt as NB increases and N=132000')
ggplot(hpl1, aes(x=NB, y=Gflops/1900)) + geom_point(aes(colour = factor(P))) + geom_line(aes(colour = factor(P))) +ggtitle('Percentage of Performance as NB increases and N=132000')

plot_ly(hpl1[complete.cases(hpl1),], x = ~P, y = ~Q, z = ~GflopsWatt, marker = list(color = ~GflopsWatt, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE)) %>%
  add_markers() %>% layout(title='GCC N=20,000', scene = list(xaxis = list(title = 'P'),
                                        yaxis = list(title = 'Q'),
                                        zaxis = list(title = 'Gflops/Watt')),
                           annotations = list(
                             x = 1.13,
                             y = 1.05,
                             text = 'Gflops/Watt',
                             xref = 'paper',
                             yref = 'paper',
                             showarrow = FALSE
                           ))

plot_ly(hpl1[complete.cases(hpl1),], x = ~P, y = ~Processes, z = ~GflopsWatt, marker = list(color = ~GflopsWatt, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE)) %>%
  add_markers() %>% layout(title='GCC N=20,000', scene = list(xaxis = list(title = 'P'),
                                        yaxis = list(title = 'Processes'),
                                        zaxis = list(title = 'Gflops/Watt')),
                           annotations = list(
                             x = 1.13,
                             y = 1.05,
                             text = 'Gflops/Watt',
                             xref = 'paper',
                             yref = 'paper',
                             showarrow = FALSE
                           ))


