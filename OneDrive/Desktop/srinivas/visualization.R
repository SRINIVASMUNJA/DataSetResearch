library(tidyverse)

pdf("visualization.pdf")
read.csv("data.csv")
d<-read_csv("data.csv")
mean(d$Area,na.rm=TRUE)
d$Area[is.na(d$Area)]<-mean(d$Area,na.rm=TRUE)
production<-d$Production
area<-d$Area
area
plot(area,production, main= "Area vs Production", xlab="Area in Hectares(in lakhs)",ylab="Production(in Kilo Grams)",pch=19, frame=T)
model<-lm(production~area, data=d)
abline(model,col="blue")

#frequency chart for production
h_prod<-hist(production,6,main="Production Frquency", xlab="Production", ylab="Frequency", col="azure",freq = FALSE)
mn<-mean(production,na.rm=TRUE)
stdD<-sd(production, na.rm=TRUE)
x1<-seq(min(production),max(production),1)
#normal distribution of production values
y1<-dnorm(x1,mean=mn,sd=stdD)
lines(x1,y1,col="blue")

#frequency chart for Area
h_area<-hist(area,6,main="Area Frquency", xlab="Area", ylab="Frequency", col="azure",freq = FALSE)
mn<-mean(area,na.rm=TRUE)
stdD<-sd(area, na.rm=TRUE)
x2<-seq(min(area),max(area))
#normal distribution of Area values
y2<-dnorm(x2,mean=mn,sd=stdD)
lines(x2,y2,col="blue")

#quantile-quantile plots for better understanding of normality of data
ggplot(d, aes(sample = Production),ylab = "Production in Kg") + 
  labs(title="Q-Q Plot for Production",y="Production")+
  stat_qq()+
  stat_qq_line()

ggplot(d, aes(sample = Area),ylab = "Area in Hectare") + 
  labs(title="Q-Q Plot for Area",y="Area")+
  stat_qq()+
  stat_qq_line()


dev.off()

