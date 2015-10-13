## Loading and preprocessing the data
act <- read.csv("activity.csv")
library(plyr)
library(dplyr)
library(ggplot2)
library(lubridate)
## What is mean total number of steps taken per day?
totstep<-tapply(act$steps,act$date,FUN = sum,na.rm=TRUE)
act$date<-as.Date(act$date,"%Y-%m-%d")
hist(totstep,main="Histogram of Total Steps",xlab="Total Steps taken per day")
meanstep<-mean(totstep)
medstep<-median(totstep)
## What is the average daily activity pattern?
stepinter <- act %>% group_by(interval) %>% summarize(mean_steps = mean(steps, na.rm = T))
plot(stepinter,type="l",main="Average number of steps taken at 5 min",xlab="Time intervals",ylab="Mean of steps number taken")
stepinter[which.max(stepinter$steps),]$interval
## Imputing missing values
sum(is.na(act$steps))
mean(is.na(act$steps))
fill<-act
for(i in 1:nrow(fill)){
  if(is.na(fill$step[i])){
    index<-fill$interval[i]
    value<-subset(stepinter,interval==index)
    fill$steps[i]<-value$steps
  }
}
head(fill)
totalstep<-tapply(fill$steps,fill$date,sum,na.rm=TRUE)
hist(totalstep)
mean(totalstep)
median(totalstep)
## Are there differences in activity patterns between weekdays and weekends? 
dayofweek <- function(date) {
  if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
    "weekend"
  } else {
    "weekday"
  }
}
fill$daytype <- as.factor(sapply(fill$date, dayofweek))

for (type in c("weekend", "weekday")) {
  steps.type <- aggregate(steps ~ interval, data = fill, subset = fill$daytype == 
                            type, FUN = mean)
  plot(steps.type, type = "l", main = type)
}