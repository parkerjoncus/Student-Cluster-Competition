library(ggplot2)
library(plyr)
library(sqldf)
library(plotly)
hpldata4$Processes<-as.numeric(hpldata4$P) * as.numeric(hpldata4$Q)
ggplot(hpldata4, aes(x=Processes, y=Gflops)) + geom_point() + geom_line()
hpldata4$difference<-abs(hpldata4$P-hpldata4$Q)
ggplot(hpldata4, aes(x=Processes, y=Gflops)) + geom_point(aes(colour = difference)) + geom_line(aes(colour = difference))
ggplot(hpldata4, aes(x=Processes, y=Gflops)) + geom_point(aes(colour = factor(difference))) + geom_line(aes(colour = factor(difference)))
head(arrange(hpldata4,desc(Gflops)), n = 20)
temp1<-sqldf('SELECT Processes, AVG(Gflops) FROM hpldata4 GROUP BY Processes')
temp2<-sqldf('SELECT Processes, MAX(Gflops) FROM hpldata4 GROUP BY Processes')
temp3<-sqldf('SELECT Processes, MIN(Gflops) FROM hpldata4 GROUP BY Processes')
ggplot(temp1, aes(x=Processes, y= `AVG(Gflops)`)) + geom_point() + geom_line() + ggtitle('avg gflops for each # of processes')
ggplot(temp2, aes(x=Processes, y= `MAX(Gflops)`)) + geom_point() + geom_line() + ggtitle('max gflops for each # of processes')
ggplot(temp3, aes(x=Processes, y= `MIN(Gflops)`)) + geom_point() + geom_line() + ggtitle('min gflops for each # of processes')

plot_ly(hpldata4, x = ~P, y = ~Q, z = ~Gflops, marker = list(color = ~Gflops, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE)) %>%
  add_markers() %>% layout(scene = list(xaxis = list(title = 'P'),
                                        yaxis = list(title = 'Q'),
                                        zaxis = list(title = 'Gflops')),
                           annotations = list(
                             x = 1.13,
                             y = 1.05,
                             text = 'Gflops',
                             xref = 'paper',
                             yref = 'paper',
                             showarrow = FALSE
                           ))

plot_ly(hpldata4, x = ~P, y = ~Q, z = ~Gflops, marker = list(color = ~Processes, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE)) %>%
  add_markers() %>% layout(scene = list(xaxis = list(title = 'P'),
                                        yaxis = list(title = 'Q'),
                                        zaxis = list(title = 'Gflops')),
                           annotations = list(
                             x = 1.13,
                             y = 1.05,
                             text = '# Processes',
                             xref = 'paper',
                             yref = 'paper',
                             showarrow = FALSE
                           ))

