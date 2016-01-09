# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

set libraries

```r
library(dplyr)
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
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.2.3
```

```r
library(Hmisc)
```

```
## Warning: package 'Hmisc' was built under R version 3.2.3
```

```
## Loading required package: lattice
## Loading required package: survival
## Loading required package: Formula
## 
## Attaching package: 'Hmisc'
## 
## The following objects are masked from 'package:dplyr':
## 
##     combine, src, summarize
## 
## The following objects are masked from 'package:base':
## 
##     format.pval, round.POSIXt, trunc.POSIXt, units
```
1. Load the data (i.e. 𝚛𝚎𝚊𝚍.𝚌𝚜𝚟())

set directory, download file

```r
fileURL<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
setwd("~/Documents/coursera/RepoResearch/RepoResearch")
download.file(fileURL, "./project1.zip", method = "curl")
```

unzip download

```r
unzip("./project1.zip")
```

read in the data

```r
activity<- read.csv("./activity.csv")
```

2. Process/transform the data (if necessary) into a format suitable for your analysis
None needed immediately. All Data transformation (for dates) are detailed in that portion of code.
***
## What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day
If you do not understand the difference between a histogram and a barplot, research the difference between them. 


```r
stepsPerDay <- tapply(activity$steps, activity$date, sum, na.rm=TRUE)
```
2. Make a histogram of the total number of steps taken each day


```r
hist(stepsPerDay, xlab = "Steps Per Day", ylab = "Frequency", main = "Distribution of Steps Per Day")
```

![](Project1MarkDown_files/figure-html/unnamed-chunk-6-1.png) 

3. Calculate and report the mean and median of the total number of steps taken per day


```r
mean(stepsPerDay)
```

```
## [1] 9354.23
```

```r
median(stepsPerDay)
```

```
## [1] 10395
```

```r
MeanSteps<- mean(stepsPerDay)
MedianSteps<- median(stepsPerDay)
```
***
## What is the average daily activity pattern?
1. Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
##Calculate Average Steps per Interval
stepsPerInt <- tapply(activity$steps, activity$interval, mean, na.rm=TRUE)

##plot in a time line
plot(names(stepsPerInt), stepsPerInt, type="l", xlab="Time Interval", ylab=expression("Steps"),main=expression("Daily Steps per 5 Minute Interval"))
```

![](Project1MarkDown_files/figure-html/unnamed-chunk-8-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
##Find highest Step Interval
MaxSteps<-which.max(stepsPerInt)
```
***
## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)

```r
##Find number of NA
sum(is.na(activity))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
##Create a duplicate
ActivityImpute<- activity

##Impute Steps with Mean
ActivityImpute <- data.frame(ActivityImpute)
ActivityImpute$imputed_steps <- with(ActivityImpute, impute(steps), mean)
ActivityImpute <-select(ActivityImpute, date, interval, imputed_steps)
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
## Calculate Mean Imputed Steps
ImputedStepsPerDay <- tapply(ActivityImpute$imputed_steps, ActivityImpute$date, sum, na.rm=TRUE)

##Create Histogram of Imputed Mean Steps
hist(ImputedStepsPerDay, xlab = "Steps Per Day", ylab = "Frequency", main = "Distribution of Steps Per Day (Imputed)")
```

![](Project1MarkDown_files/figure-html/unnamed-chunk-12-1.png) 

```r
##report mean and median of imputed steps
ImputedMean<- mean(ImputedStepsPerDay)
ImputedMedian <- median(ImputedStepsPerDay)
mean(ImputedStepsPerDay)
```

```
## [1] 9354.23
```

```r
median(ImputedStepsPerDay)
```

```
## [1] 10395
```

```r
##graph for differences
par(mfrow=c(2,1))
hist(stepsPerDay, xlab = "Steps Per Day", ylab = "Frequency", main = "Distribution of Steps Per Day")
hist(ImputedStepsPerDay, xlab = "Steps Per Day", ylab = "Frequency", main = "Distribution of Steps Per Day (Imputed)")
```

![](Project1MarkDown_files/figure-html/unnamed-chunk-12-2.png) 
***
## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
##Add column with Day of the Week
ActivityImpute$day = strftime(ActivityImpute$date,'%A')
```


2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```r
##Subset Weekend and WeekDay Data
WeekdayData<- filter(ActivityImpute, day == "Monday" | day == "Tuesday" |day == "Wednesday" |day == "Thursday" |day == "Friday" )
WeekendData<- filter(ActivityImpute, day == "Sunday" | day == "Saturday")

##Calculate Interval Steps for Weekend and Weekday
stepsPerIntWDay <- tapply(WeekdayData$imputed_steps, WeekdayData$interval, mean, na.rm=TRUE)
stepsPerIntWEnd <- tapply(WeekendData$imputed_steps, WeekendData$interval, mean, na.rm=TRUE)

##Create Panel Graph for Weekend and Weekday
par(mfrow=c(2,1))
plot(names(stepsPerIntWDay), stepsPerIntWDay, type="l", xlab="Time Interval", ylab=expression("Steps"),main=expression("Weekday"))
plot(names(stepsPerIntWEnd), stepsPerIntWEnd, type="l", xlab="Time Interval", ylab=expression("Steps"),main=expression("Weekend"))
```

![](Project1MarkDown_files/figure-html/unnamed-chunk-14-1.png) 
