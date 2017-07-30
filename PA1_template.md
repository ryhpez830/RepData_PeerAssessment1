---
title: "Assignment1"
output: html_document
---

##Loading and preprocessing the data

1.Load the data
```{r,echo=TRUE}
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",destfile="data.zip")
unzip("data.zip")
read.csv("activity.csv",sep=",")->activity
```

2.Transform the data into a format suitable for analysis
```{r,echo=TRUE}
Sys.setlocale("LC_TIME", "English")
activity$date<-as.Date(activity$date)
head(activity)
```


##What is mean total number of steps taken per day?

1.Calculate the total number of steps taken per day
```{r,echo=TRUE}
library(dplyr)
summarize(group_by(activity,date), sum=sum(steps,na.rm=T))->s
head(s)
```

2.Make a histogram of the total number of steps taken each day.
```{r,echo=TRUE}
plot(as.Date(s$date),s$sum,type="h",xlab="Date",ylab="Total Number of Steps")
```

3.Calculate and report the mean and median of the total number of steps taken per day.
```{r,echo=TRUE}
mean<-mean(s$sum,na.rm=T)
median<-median(s$sum,na.rm=T)
```
The answer is
```{r,echo=TRUE}
c(mean,median)
```


##What is the average daily activity pattern?

1.Make a time series plot of the 5-minute interval and the average number of steps taken, averaged across all days.
```{r,echo=TRUE}
summarize(group_by(activity,interval), mean=mean(steps,na.rm=T))->m

for(i in 1:nrow(m)){
  if(i<=12){
    paste("00",m[i,1],"00",sep=":")->m[i,1]
  }
  else if(i>12 & i<=120){
    paste(substr(m[i,1],1,1),substr(m[i,1],2,3),"00",sep=":")->m[i,1]
  }
  else{
    paste(substr(m[i,1],1,2),substr(m[i,1],3,4),"00",sep=":")->m[i,1]
  }
}

head(m)

strptime(m$interval,"%H:%M")->m$interval
plot(m$interval,m$mean,type="l",xlab="Time",ylab="Average Number of Steps")
```

2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r,echo=TRUE}
which.max(m$mean)->w
unclass(m$interval[w])$hour->hour
unclass(m$interval[w])$min->minute
```
The answer is
```{r,echo=TRUE}
if(hour<12){
  paste(paste(hour,minute,sep=":"),"AM")
}else{
  paste(paste(hour,minute,sep=":"),"PM")
}
```


##Imputing missing values

1.Calculate and report the total number of missing values in the dataset.
```{r,echo=TRUE}
sum(is.na(activity$steps))
```

2.Devise a strategy for filling in all of the missing values in the dataset.

I used the mean for that 5-minute interval.

3.Create a new dataset that is equal to the original dataset but with the missing data filled in.
```{r,echo=TRUE}
summarize(group_by(activity,interval), mean=mean(steps,na.rm=T))->m
activity->activity2

for(i in 1:nrow(activity2)){
  if(is.na(activity2$steps[i])==T){
    which(activity2$interval[i]==m$interval)->n
    activity2$steps[i]<-m$mean[n]
  }
}

head(activity2)
```

4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 
```{r,echo=TRUE}
summarize(group_by(activity2,date), sum=sum(steps))->s2
plot(as.Date(s2$date),s2$sum,type="h",xlab="Date",ylab="Total Number of Steps")
```

The answer is
```{r,echo=TRUE}
mean2<-mean(s2$sum)
median2<-median(s2$sum)
c(mean2,median2)
```
Do these values differ from the estimates from the first part of the assignment?

Yes.

What is the impact of imputing missing data on the estimates of the total daily number of steps?

The mean is equal to the median.


##Are there differences in activity patterns between weekdays and weekends?

1.Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
```{r,echo=TRUE}
weekdays(activity2$date,abbreviate = T)->weekdays
head(weekdays)

day<-integer(nrow(activity2))

for(i in 1:nrow(activity2)){
  if(weekdays[i]=="Sat"|weekdays[i]=="Sun"){
    day[i]<-"weekend"
  }
  else{
    day[i]<-"weekday"
  }
}

as.factor(day)->day
head(day)
cbind(activity2,day)->activity2
head(activity2)
```

2.Make a panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days.
```{r,echo=TRUE}
aggregate(activity2$steps,by=list(activity2$interval,activity2$day),FUN="mean")->m2
names(m2)<-c("interval","day","mean")
head(m2)

library(lattice)
xyplot(mean~interval|day,data=m2,layout=c(1,2),type="l")
```
