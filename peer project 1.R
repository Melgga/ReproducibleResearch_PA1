#0
act<-read.csv("activity.csv")

#1
dailysteps<-tapply(act$steps,act$date,sum,na.rm=TRUE)
library(ggplot2)
dsf<-data.frame(dailysteps)
mn<-mean(dailysteps)
md<-median(dailysteps)
mm<-as.data.frame(c(mn,md))
names(mm)<-"par"
mm$cat<-factor(c("mean","median")) #for the 2 vertical lines
dsf$Vertical_Lines<-factor(rep("1",61)) #artificial factor needed to make a legend
#
g<-ggplot(dsf,aes(x=dailysteps,linetype=Vertical_Lines))
g+geom_histogram(fill="orange",colour="darkblue",binwidth=3000)+
  labs(title = "Daily Steps")+
  geom_vline(data=mm,aes(xintercept=par,linetype=cat))+
  scale_linetype_discrete(breaks=c("mean","median")) #removes artificial factor from legend
ggsave(filename="p1dsteps.png",width=5,height=4)
dev.off()

#2 average_5 minutes
intervalnumber<-0:(nrow(act)-1)
intervalclass<-factor(intervalnumber%%288)
intmeans<-tapply(act$steps,intervalclass,mean,na.rm=TRUE)
dimnames(intmeans)[[1]]<-NULL  ##to avoid confusions
intmeans2<-as.data.frame(intmeans)
d0<-as.character(act$date[1])
t0<-as.POSIXlt(d0)
l<-dim(intmeans2)[1]
intmeans2$times<-as.POSIXct(seq.POSIXt(from=t0,by="5 min",length.out=l))
#
p2<-ggplot(intmeans2,aes(x=times,y=intmeans))
p2m<-p2+labs(title = "Average steps  observed on 5 minute intervals")+
  geom_point(size=1.2)+
  geom_line(size=.4,col="blue")+
  labs(x="time",y="number of steps")+
  ggsave(filename="p2intmeans.png",width=5,height=4)
#dev.off()

scale_x_date(labels = date_format("%H:%M:%S"))
install.packages("scales")
library(scales)

#max
which.max(intmeans2$intmeans)
max(intmeans2$intmeans)
intmeans2$times[which.max(intmeans2$intmeans)]

#3 NAs
table(is.na(act))
table(is.na(act$steps))
mean(is.na(act$steps))

act2<-act
nal<-which(is.na(act2$steps))
nal2<- nal%%288
act2$steps[nal]<-intmeans2$intmeans[nal2]

dailysteps2<-tapply(act2$steps,act2$date,sum)
dsf2<-data.frame(dailysteps2)
mn2<-mean(dailysteps2)
md2<-median(dailysteps2)
mm2<-as.data.frame(c(mn2,md2))
names(mm2)<-"par"
mm2$cat<-factor(c("mean","median")) #for the 2 vertical lines
dsf2$Vertical_Lines<-factor(rep("1",61)) #artificial factor needed to make a legend

p3<-ggplot(dsf2,aes(x=dailysteps2,linetype=Vertical_Lines))
p3m<-p3+geom_histogram(fill="orange",colour="darkblue",binwidth=3000)+
  labs(title = "Daily Steps")+
  geom_vline(data=mm2,aes(xintercept=par,linetype=cat))+
  scale_linetype_discrete(breaks=c("mean","median")) #removes artificial factor from legend
  ggsave(filename="p3dsteps.png",width=5,height=4)

#4 average_5 minutes no NAs
intervalnumber2<-0:(nrow(act2)-1)
intervalclass2<-factor(intervalnumber2%%288)
library(reshape2)

wd<-weekdays(as.POSIXlt(as.character(act2$date)))
ind<-wd %in% unique(wd)[6:7]
act2$week<-factor("weekday")
levels(act2$week)<-c("weekday","weekend")
act2$week[ind]<-"weekend"
factorslist<-list(intervalclass2,act2$week)
means1<-tapply(act2$steps,factorslist,mean)  ##rownames(means1), colnames(means1)
d0<-as.character(act2$date[1])
t0<-as.POSIXlt(d0)
l<-nrow(means1)
means2<-data.frame(weekday=means1[,1],weekend=means1[,2],time=as.POSIXct(seq.POSIXt(from=t0,
                                          by="5 min",length.out=l)))
means3<-melt(means2,id.vars = "time")

p4<-ggplot(means3,aes(x=time,y=value))
p4m<-p4+labs(title = "Average steps  observed on 5 minute intervals")+
  geom_point(size=1.2)+
  geom_line(size=.4,col="blue")+
  labs(x="time",y="number of steps")+
  facet_wrap(~variable,nrow = 2, ncol = 1)+
  ggsave(filename="p4meanswd.png",width=5,height=4)


#misc:
library(chron)
t1 <- times("00:00:00")
t2 <- times("23:59:55")
tt <- seq(t1, t2, by = times("00:05:00"))
intmeans2$times<-tt 

day0<-as.Date(d0)
tt<-strptime(paste(d0,"1000"),"%Y-%m-%d %M")
d_t<-strptime(paste(cons$Date,cons$Time), "%d/%m/%Y %H:%M:%S")
#ttt<-as.POSIXct(seq.POSIXt(from=t0,by="5 min",length.out=l))
#intmeans2$times<-substr(ttt,12,21)

