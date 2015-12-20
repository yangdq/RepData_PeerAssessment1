# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.2.3
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
rawactivity <- read.csv("activity.csv", header = TRUE)
rawactivity$date <- as.Date(as.character(rawactivity$date), "%Y-%m-%d")
```


## What is mean total number of steps taken per day?

```r
activity <- rawactivity[!is.na(rawactivity$steps),]
meanStep <- mean(activity$steps)
medianStep <- median(activity$steps)
meanStep
medianStep
hist(activity$steps)
```

![](PA1_template_files/figure-html/totalSteps-1.png) 
The average totoal number of steps is 37.3825996, and the median total number of steps is 0.

## What is the average daily activity pattern?

```r
intervalGroups <- group_by(activity, interval)
intervalReport <- summarize(intervalGroups, averageSteps=mean(steps))
plot(intervalReport$interval, intervalReport$averageSteps, type = "l" )
```

![](PA1_template_files/figure-html/stepsInterval-1.png) 

```r
maxInterval <- intervalReport[(intervalReport$averageSteps == max(intervalReport$averageSteps)), c("interval")]
```
The interval of 835 minutes, on average across all the days in the dataset, contains the maximum number of steps

## Imputing missing values

```r
naIndexes <- which( is.na(rawactivity$steps))
totalMissingValues <- length(naIndexes)
fullactivity <- rawactivity

for(index in naIndexes){
  inter <- fullactivity[index,]$interval
  avgIndex <- which(intervalReport$interval == inter)
  fullactivity[index,]$steps <- intervalReport[avgIndex, ]$averageSteps
}

filledmeanStep <- mean(fullactivity$steps)
filledmedianStep <- median(fullactivity$steps)
hist(fullactivity$steps)
```

![](PA1_template_files/figure-html/fillMissingValues-1.png) 
The average step filled data of averages is 37.3825996, and the median step with filled data of average is 0.  The values are the same as the filled values are just average from the real data.

## Are there differences in activity patterns between weekdays and weekends?

```r
activity$weekday <- "weekday"
weekendIndex <-  which(format(activity$date, "%a") %in% c("Sat", "Sun"))
activity[weekendIndex,]$weekday <- "weekend"
activity$weekday <- factor(activity$weekday)
weekGroups <- group_by(activity, interval, weekday)
weekReport <- summarize(weekGroups, averageSteps=mean(steps))

library(lattice)
xyplot(averageSteps ~ interval | weekday , data =weekReport, type = "l" , layout=c(1,2), ylab="Number of Steps")
```

![](PA1_template_files/figure-html/weekdayPattern-1.png) 
Based on the Plot, the pattern between weekend and weekend are similar.

