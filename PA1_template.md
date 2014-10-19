---
title: "Peer Assignment1"
author: "ACK"
date: "Wednesday, October 19, 2014"
output: html_document
---


### Q1 Loading and preprocessing the data

```{r}
activity <- read.csv("activity.csv")
activity$date <- as.Date(activity$date) 
head(activity)
```
---
###Q2 What is mean total number of steps taken per day?

1.Make Histogram

```{r, echo=TRUE}

steps_date <- aggregate(steps ~ date, data = activity, FUN = sum,na.rm=T)
hist(steps_date$steps, main="Histogram of the total number of steps taken each day", xlab = "Total number of steps taken daily" , ylab= "frequency " , col="green" )
```
 
 2.Calculating Mean Median
```{r,echo=TRUE}
MeanSteps_date <- mean(steps_date$steps,na.rm=T)
Median <- median(steps_date$steps,na.rm=T)
cat("Mean total number of steps taken",MeanSteps_date)
cat("Median of total number of steps taken daily",Median)


```
###Q3 What is the average daily activity pattern?

1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and   the average number of steps taken, averaged across all days (y-axis)



```{r,echo=TRUE}
stepsMeanPerInterval <- aggregate(steps ~ interval, data =activity ,FUN = mean, na.rm = T)
plot(stepsMeanPerInterval, type = "l")
```


2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r,echo=TRUE}
max <- stepsMeanPerInterval$interval[which.max(stepsMeanPerInterval$steps)]
cat("max number of steps in 5 min interval is  ",max)
```

###Q4. Imputing missing values

1.Calculate and report the total number of missing values in the dataset (i.e. the           total number of rows with NAs)





```{r.echo=FALSE}
# 1. Calculate and eport total number missing values in the database
ActivityNa <- sum(is.na(activity))
cat("1. Total number of missing values in the dataset " , ActivityNa)
```

2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3.Create a new dataset that is equal to the original dataset but with the missing data filled in.
```{r.echo=FALSE}
#2. Fill NAs and create a database with filled NAs
StepsMean <- aggregate(steps ~ interval, data = activity, FUN = mean)
ActivityfilledNA <- activity
for (i in 1:nrow(activity)){
  if (is.na(activity$steps[i])){
    interval_val <- activity$interval[i]
    row_id <- which(StepsMean$interval == interval_val)
    steps_val <- StepsMean$steps[row_id]
    ActivityfilledNA$steps[i] <- steps_val
  }
}
```

4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r.echo=TRUE}
# Draw Histogram 
stepMeanNA <- aggregate(steps ~ interval, data = ActivityfilledNA, FUN = mean)

hist(stepMeanNA$steps,main ="histogram of total number of steps taken each day filled NAs", xlab ="Total number of steps taken each day",ylab="frequency",col= "green")

MeanStepsNA <- mean(ActivityfilledNA$steps)
MeanStepsNA
MedianstepsNA <- median(ActivityfilledNA$steps)
MedianstepsNA
```
Report : There is difference in graphs in original database and imputed database.

###Q5  Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1.Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```{r.echo=TRUE}
daytype <- function(date) {
    if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
        "weekend"
    } else {
        "weekday"
    }
}
activity$daytype <- as.factor(sapply(activity$date, daytype))
```
2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
```{r.echo=TRUE}
par(mfrow = c(1,1))
for (type in c("weekend", "weekday")) {
    steps.type <- aggregate(steps ~ interval, data = activity, subset = activity$daytype == type, FUN = mean)
    plot(steps.type, type = "l", main = type)
}


```