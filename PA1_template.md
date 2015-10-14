---
title: "Reproducible Research: Peer Assessment 1"
author: Daniel Nordlund
date: October 9, 2015
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
## unzip project data and read it in
## unzip('./activity.zip')
activity <- read.csv('activity.csv', stringsAsFactors = FALSE)
dim(activity)
```

```
## [1] 17568     3
```

```r
## create date/time variable 
library(lubridate)
activity$datetime <- with(activity, ymd_hm(paste(date, interval%/%100, interval%%100, sep=':')))

## time_of_day is a "hack" to ease aggregating and plotting the data with ggplot2
## any calendar date could have been chosen for this, but for a variety of reasons I chose
## the R/Unix epoch start date
activity$time_of_day <- with(activity, ymd_hm(paste('1970-01-01', interval%/%100, interval%%100, sep=':')))
```

## What is mean total number of steps taken per day?

```r
## identify rows where values of steps is missing
na_index <- is.na(activity$steps)

total_steps_perday <- aggregate(steps ~ date, data=activity, FUN=sum)

hist(total_steps_perday[,2], breaks=8, main='Histogram of total steps per day', xlab='Total steps')
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

```r
steps_summary <- summary(total_steps_perday[,2])
sprintf('Mean = %7.1f and Median = %7.1f', steps_summary['Mean'], steps_summary['Median'])
```

```
## [1] "Mean = 10770.0 and Median = 10760.0"
```

## What is the average daily activity pattern?

```r
## plot time series of total steps within time periods
## averaged over days
df <- aggregate(steps ~ time_of_day, data=activity, FUN=mean)
plot(df$time_of_day, df$steps, type='l', main='Mean steps per 5-minute interval across days', xlab='Time of day', ylab='Mean number of steps')
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
## which interval had the maximum mean number of steps across all days
max_interval <- df[which.max(df$steps),"time_of_day"]
sprintf('Interval with maximum mean number of steps began at %s', format(max_interval,'%H:%M'))
```

```
## [1] "Interval with maximum mean number of steps began at 08:35"
```

## Imputing missing values

```r
## how much missing data is there?
percent_missing <- sum(is.na(activity$steps)) / nrow(activity) * 100
sprintf("Percent of intervals where variable, steps, was NA: %5.1f", percent_missing)
```

```
## [1] "Percent of intervals where variable, steps, was NA:  13.1"
```

```r
## impute the the mean value for the same time interval and day of week
activity$day_of_week <- wday(activity$datetime)
imputation_df <- aggregate(steps ~ time_of_day + day_of_week, data=activity, FUN=mean)
imputed_activity <- merge(activity, imputation_df, by=c('day_of_week','time_of_day'), all.x=TRUE) 
imputed_activity <- imputed_activity[order(imputed_activity$datetime),]
imputed_activity$imputed_steps <- ifelse(is.na(imputed_activity$steps.x), imputed_activity$steps.y, imputed_activity$steps.x)

imputed_total_steps_perday <- aggregate(imputed_steps ~ date, data=imputed_activity,  FUN=sum)

hist(imputed_total_steps_perday[,2], breaks=8, main='Histogram of total steps per day\nMissing data imputed by day of week and interval', xlab='Total steps')
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

```r
imp_steps_summary <- summary(imputed_total_steps_perday[,2])
sprintf('Mean = %s and Median = %s', imp_steps_summary['Mean'], imp_steps_summary['Median'])
```

```
## [1] "Mean = 10820 and Median = 11020"
```



## Are there differences in activity patterns between weekdays and weekends?

```r
## 
library(ggplot2)
library(scales)
imputed_activity$weekday <- ifelse(imputed_activity$day_of_week %in% c(1,7), 'Weekend', 'Weekday')
df <- aggregate(imputed_steps ~ weekday + time_of_day, data=imputed_activity, FUN=mean)
qplot(time_of_day, imputed_steps, data=df, facets = weekday ~ ., geom=c('line'), xlab="Time of Day", ylab='Mean number of steps') + scale_x_datetime(breaks=date_breaks('2 hour'), labels=date_format('%H:%M')) + labs(title='Mean steps per 5-minute interval - Weekdays vs. Weekends')
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

