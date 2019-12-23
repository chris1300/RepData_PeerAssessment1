---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


###1. Load the data (i.e. \color{red}{\verb|read.csv()|}read.csv())

```r
activity <- read.csv("activity.csv", header = TRUE, sep = ",")
```

###2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
activity$date <- as.Date(activity$date)
```

## What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

###1. Calculate the total number of steps taken per day

```r
sumStepsByDay <- aggregate(steps~date, activity, sum)
```

###2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

```r
hist(sumStepsByDay$steps, main="Total Number of Steps Taken Each Day", xlab = "Total Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

###3. Calculate and report the mean and median of the total number of steps taken per day

```r
stepsMean <- mean(sumStepsByDay$steps, na.rm = TRUE)
stepsMedian <- median(sumStepsByDay$steps, na.rm = TRUE)
```

The steps mean is: 1.0766189\times 10^{4}  
The steps median is: 10765


## What is the average daily activity pattern?

###1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
meanStepsByInterval <- aggregate(steps~interval, activity, mean)
names(meanStepsByInterval)[names(meanStepsByInterval == 'steps')] <- "meanSteps"
plot(meanStepsByInterval, meanStepsByInterval$meanSteps, type="l", 
     main="Average Number of Steps Taken per Five Minute Interval", xlab="Five Minute Interval (Across All Days)", ylab="Average Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

###2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
meanStepsSorted <- meanStepsByInterval[with(meanStepsByInterval, order(-steps)),]
max <- meanStepsSorted$interval[1]
```

Max steps is: 835 

## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

###1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sumNA <- sum(rowSums(is.na(activity)))
```
Total number of missing values is: 2304 

###2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
#using mean of 5 minute interval to fill in. Will merge datasets raw values and means together. 
imputedActivity = merge(activity, meanStepsByInterval, by.x = "interval", by.y = "interval")
names(imputedActivity)[names(imputedActivity) == "steps.x"] <- "stepsRaw"
names(imputedActivity)[names(imputedActivity) == "steps.y"] <- "stepsMean"
```

###3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
imputedActivity$stepsMerged <- imputedActivity$stepsRaw  
imputedActivity$stepsMerged[is.na(imputedActivity$stepsRaw)] <- 
    imputedActivity$stepsMean[is.na(imputedActivity$stepsRaw)]
```

###4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 

```r
totalStepsImputed <- aggregate(stepsMerged~interval, imputedActivity, sum)
hist(totalStepsImputed$steps, main="Total Number of Steps Taken Each Day", xlab="Total Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps? 

```r
imputedMean <- mean(imputedActivity$stepsMerged, na.rm = TRUE)
imputedMedian <- median(imputedActivity$stepsMerged, na.rm = TRUE)
```

The previous steps mean is 1.0766189\times 10^{4} and the imputed mean is 37.3825996    
The steps median is: 10765 and the imputed median is 0


## Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.


###1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
imputedActivity$isWeekday <- 
  ifelse(weekdays(as.Date(imputedActivity$date)) %in% c("Saturday", "Sunday"), "weekend", "weekday")
head(imputedActivity)
```

```
##   interval stepsRaw       date stepsMean stepsMerged isWeekday
## 1        0       NA 2012-10-01  1.716981    1.716981   weekday
## 2        0        0 2012-11-23  1.716981    0.000000   weekday
## 3        0        0 2012-10-28  1.716981    0.000000   weekend
## 4        0        0 2012-11-06  1.716981    0.000000   weekday
## 5        0        0 2012-11-24  1.716981    0.000000   weekend
## 6        0        0 2012-11-15  1.716981    0.000000   weekday
```

```

###2. Make a panel plot containing a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
grp <- group_by(imputedActivity, interval, isWeekday)
grpAvg <- summarise(grp, mean=mean(stepsMerged))

library(lattice)
xyplot(mean~interval|isWeekday,data=grpAvg,type="l",layout = c(1,2),
       main="Number of steps (Average by interval)",
       ylab="Number of steps (average by interval)",xlab="Interval (5 minute increments)")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
