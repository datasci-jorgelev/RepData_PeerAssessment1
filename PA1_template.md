# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data



```r
activ <- read.csv("activity.csv", colClasses = c("numeric", "factor", "factor"))
```


## What is mean total number of steps taken per day?

The total mean is:


```r
StepsPerDay <- tapply(activ$steps, activ$date, sum)
mean(StepsPerDay, na.rm=T)
```

```
## [1] 10766
```
The total median is:

```r
median(StepsPerDay, na.rm=T)
```

```
## [1] 10765
```
The histogram looks like this:


```r
hist(StepsPerDay, breaks=10)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 


## What is the average daily activity pattern?


```r
StepsPerInterval <- tapply(activ$steps, activ$interval, mean, na.rm = T)
plot(StepsPerInterval, type="l")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r
maxSteps <- StepsPerInterval[StepsPerInterval == max(StepsPerInterval)]
```
The maximum steps per interval is 206.1698.



## Imputing missing values


```r
numOfnas <- length(which(is.na(activ$steps)))
```
The total of nas is 2304.

As we see in these pictures, the NAs are concentrate in eight days.


```r
NAsteps <- activ[is.na(activ$steps),]
NAstepsPerDay <- tapply(NAsteps$date, NAsteps$date, length)
plot(NAstepsPerDay)
```

![plot of chunk nasperday](figure/nasperday1.png) 

```r
NAstepsPerInterval <- tapply(NAsteps$interval, NAsteps$interval, length)
plot(NAstepsPerInterval)
```

![plot of chunk nasperday](figure/nasperday2.png) 


So, I think is best to fill NAs with the mean of the interval, that we calculated before:


```r
noNAsactiv <- activ
noNAsactivSteps <- apply(noNAsactiv, 1, function (x) if (is.na(x["steps"])) as.numeric(StepsPerInterval[x["interval"]]) else as.numeric(x["steps"]))

noNAsactiv$steps <- noNAsactivSteps

StepsPerDayNew <- tapply(noNAsactiv$steps, noNAsactiv$date, sum)
hist(StepsPerDayNew, breaks=10)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 


The new mean is:


```r
mean(StepsPerDayNew, na.rm=T)
```

```
## [1] 10766
```

... the same mean as before, because we used the average intervals for fill the NAs


## Are there differences in activity patterns between weekdays and weekends?


```r
activeDate <- strptime(as.character(activ$date), "%Y-%m-%d")
activeDayWeek <- weekdays(activeDate)
# This is necesary for use apply later...
dim(activeDayWeek) <- c(17568)

activWeek <- apply(activeDayWeek, 1, function (x) if (x %in% c("sábado", "domingo")) "weekend" else "weekday")

activWeekFactor <- as.factor(activWeek)

activeWW <- data.frame(noNAsactiv, activWeekFactor)

#Splitting the data in weekday an weekend

splitActive <- split(activeWW, activeWW$activWeekFactor)

StepsPerIntervalWeekday <- tapply(splitActive$weekday$steps, splitActive$weekday$interval, mean, na.rm = T)
StepsPerIntervalWeekend <- tapply(splitActive$weekend$steps, splitActive$weekend$interval, mean, na.rm = T)


par(mfrow=c(2,1))

plot(StepsPerIntervalWeekday, type="l")
plot(StepsPerIntervalWeekend, type="l")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 


So... yes, the are differences between weedays and weekends. He/She is more active in weekends :)



