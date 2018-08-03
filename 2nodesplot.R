library(readr)
hpldata5 <- read_csv("~/Downloads/hpldata5.csv")
View(hpldata5)
hpldata5$node2<-48-hpldata5$node1
ggplot(hpldata5, aes(x=node2, y=Gflops)) + geom_point() + geom_line()
ggplot(hpldata5, aes(x=node2, y=Gflops)) + geom_point(aes(colour = factor(P))) + geom_line(aes(colour = factor(P)))