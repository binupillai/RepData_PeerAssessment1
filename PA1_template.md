# Reproducible Research: Peer Assessment 1


### Loading and preprocessing the data

Assuming the data file is in the same folder as the R code is.


```r
library(lattice)
activity <- read.csv("activity.csv")
```



### What is mean total number of steps taken per day?

1.Make a histogram of the total number of steps taken each day


```r
total.steps.day <- aggregate(steps ~ date, data = activity, FUN = sum,na.rm = TRUE)
barplot(total.steps.day$steps, names.arg = total.steps.day$date, xlab = "date", ylab = "steps")
```

![plot of chunk unnamed-chunk-2](./figure/unnamed-chunk-21.png) 

```r
hist(total.steps.day$steps,main="Frequency of Steps",col="red",xlab="Total steps",ylab="Frequency")
```

![plot of chunk unnamed-chunk-2](./figure/unnamed-chunk-22.png) 


2. Calculate and report the **mean** and **median** total number of
steps taken per day


```r
mean(total.steps.day$steps)
```

```
## [1] 10766
```

```r
median(total.steps.day$steps)
```

```
## [1] 10765
```

### What is the average daily activity pattern?

1. Make a time series plot (i.e. `type = "l"`) of the 5-minute
interval (x-axis) and the average number of steps taken, averaged
across all days (y-axis)


```r
steps.interval <- aggregate(steps ~ interval, data=activity, FUN=mean)
plot(steps.interval, type="l")
```

![plot of chunk unnamed-chunk-4](./figure/unnamed-chunk-4.png) 

2. Which 5-minute interval, on average across all the days in the dataset,contains the maximum number of steps?


```r
steps.interval$interval[which.max(steps.interval$steps)]
```

```
## [1] 835
```

### Imputing missing values

1. Calculate and report the total number of missing values in the
dataset (i.e. the total number of rows with `NA`s)

```r
sum(is.na(activity))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the
dataset. The strategy does not need to be sophisticated. For
example, you could use the mean/median for that day, or the mean
for that 5-minute interval, etc.

Using mean for the 5-minute intervals as filler for missing
values.

3. Create a new dataset that is equal to the original dataset but with
the missing data filled in.


```r
activity <- merge(activity, steps.interval, by = "interval", suffixes = c("", ".mean"))
nas <- is.na(activity$steps)
activity$steps[nas] <- activity$steps.mean[nas]
activity <- activity[, c(1:3)]
```

4. Make a histogram of the total number of steps taken each day and
Calculate and report the **mean** and **median** total number of
steps taken per day. Do these values differ from the estimates from
the first part of the assignment? What is the impact of imputing
missing data on the estimates of the total daily number of steps?

Plot

```r
total.steps.day <- aggregate(steps ~ date, data = activity, FUN = sum)
barplot(total.steps.day$steps, names.arg = total.steps.day$date, xlab = "date", ylab = "steps")
```

![plot of chunk unnamed-chunk-8](./figure/unnamed-chunk-81.png) 

```r
hist(total.steps.day$steps,main="Frequency of Steps after data correction",col="green",xlab="Total steps",ylab="Frequency")
```

![plot of chunk unnamed-chunk-8](./figure/unnamed-chunk-82.png) 


Mean & Median

```r
mean(total.steps.day$steps)
```

```
## [1] 10766
```

```r
median(total.steps.day$steps)
```

```
## [1] 10766
```

These is a small impact in missing data.


### Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels --
  "weekday" and "weekend" indicating whether a given date is a
weekday or weekend day.



```r
daytype <- function(date) {
  if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
    "weekend"
  } else {
    "weekday"
  }
}
activity$daytype <- as.factor(sapply(activity$date, daytype))
```

2. Make a panel plot containing a time series plot (i.e. `type = "l"`)
of the 5-minute interval (x-axis) and the average number of steps
taken, averaged across all weekday days or weekend days
(y-axis).


```r
steps.type <- aggregate(steps ~ interval, data = activity, mean)
xyplot(steps ~ interval | activity$daytype, steps.type, type = "l", layout = c(1, 2), 
    xlab = "Interval", ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-11](./figure/unnamed-chunk-11.png) 
