---
title: "Reproducible Research: Peer Assessment 1"
author: Daniel Nordlund
date: October 17, 2015
output: 
  html_document:
    keep_md: true
---

## Introduction

For first peer assessment project we were asked to download, read into R, and analyze a data set containing information on two months of daily walking activity for an individual.  The data set contained three variables;

- **steps**: Number of steps taking in a 5-minute interval (missing values are coded as NA)
- **date**: The date on which the measurement was taken in YYYY-MM-DD format
- **interval**: Identifier for the 5-minute interval in which measurement was taken

## Loading and preprocessing the data

The data was downloaded from https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip and unzipped in the current working directory of the project.  The activity data was read into a data frame called **activity**.  

Some additional variables were created to ease the analysis and plotting of the data.  The date and interval variables were combined into a date/time variable called **datetime** and added to the activity data frame.  A **time_of_day** variable was created from the interval identifier.  The variable **time_of_day** is a "hack" to ease aggregating the data over time intervals and then subequently plotting the intervals using the **ggplot2** package.


```r
## unzip project data and read it in
## unzip('https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip')
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

The first thing we were asked to explore was the distribution of total number of steps per day.  The number of steps per 5-minute interval was summed up within days, and a histogram of the total steps per day was plotted.  This analysis ignored time intervals where the **steps** variable was missing.


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
The mean number of steps per day was 10770.0 and the median was 10760.0.


## What is the average daily activity pattern?

Next, we were asked to examine the pattern of activity by time interval averaged across days.  So the mean number of steps per time interval aggregated across days was computed and a histogram of the mean number of steps was plotted.


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

The interval with the maximum mean number of steps across all days was the interval that began at 08:35. 


## Imputing missing values

Next we were asked to determine the number of intervals with missing data.  


```r
## how much missing data is there?
count_missing <- sum(is.na(activity$steps))
percent_missing <- count_missing / nrow(activity) * 100
sprintf("Percent of intervals where variable, steps, was NA: %5.1f", percent_missing)
```

```
## [1] "Percent of intervals where variable, steps, was NA:  13.1"
```

There were 2304 intervals with missing data which was  13.1 percent of the intervals. 

The next task was to devise a method for imputing values for the intervals where the number of steps was missing. There are a variety of approaches that can be used for imputation of data.  For this analysis, I chose to do a fairly simple mean imputation.  However, subsequent analyses were going to be looking at differences between patterns of activities for weekdays versus weekend days. So, for a missing value for a particular time interval in a particular day of the week (e.g. Mon or Tue or ... ), I decided to impute the mean of just that time interval and day of the week.  Therefore, I created a variable **day_of_week** to use for computing mean number of steps within interval and day of week. Finally a variable **imputed_steps** was created from the original **steps** variable, where missing values of **steps** were replaced by the imputed mean values.

Next, a histogram of the total number of steps per day (based on the imputed data) was plotted.


```r
## create a day of week variable for imputation and for flagging weekdays and weekends
activity$day_of_week <- wday(activity$datetime)

## compute mean values by day_of_week and time_of_day
imputation_df <- aggregate(steps ~ time_of_day + day_of_week, data=activity, FUN=mean)
## merge imputed values with original data and replace NA with imputed means
imputed_activity <- merge(activity, imputation_df, by=c('day_of_week','time_of_day'), all.x=TRUE) 
imputed_activity <- imputed_activity[order(imputed_activity$datetime),]
imputed_activity$imputed_steps <- ifelse(is.na(imputed_activity$steps.x), imputed_activity$steps.y, imputed_activity$steps.x)

## compute total steps per day for imputed data
imputed_total_steps_perday <- aggregate(imputed_steps ~ date, data=imputed_activity,  FUN=sum)

## plot histogram of imputed data frame
hist(imputed_total_steps_perday[,2], breaks=8, main='Histogram of total steps per day\nMissing data imputed by day of week and interval', xlab='Total steps')
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

```r
## compute summary statistics for the imputed data
imp_steps_summary <- summary(imputed_total_steps_perday[,2])
sprintf('Mean = %s and Median = %s', imp_steps_summary['Mean'], imp_steps_summary['Median'])
```

```
## [1] "Mean = 10820 and Median = 11020"
```

The distribution of the imputed data is similar to the original distribution of total steps where missing values were ignored. The mean and median number of steps for the imputed data was 10820.0 and 11020.0, respectively, as compared to the original values of 10770.0 and 10760.0.


## Are there differences in activity patterns between weekdays and weekends?

Finally we were asked to determine if there were differences between weekdays and weekends with respect to activity at various times of day.  We were asked to use the imputed data for this comparison.  I created a **weekday** variable that took on the value 'Weekend' if the day of the week was either a Saturday or a Sunday, and took on the value of 'Weekday' otherwise.  The mean number of steps was computed within **weekday** and **time_of_day** and the **ggplot2** package (along with the **scales** package) was used to plot time series for weekdays and weekends, separately.


```r
## 
library(ggplot2)
library(scales)
imputed_activity$weekday <- ifelse(imputed_activity$day_of_week %in% c(1,7), 'Weekend', 'Weekday')
df <- aggregate(imputed_steps ~ weekday + time_of_day, data=imputed_activity, FUN=mean)
qplot(time_of_day, imputed_steps, data=df, facets = weekday ~ ., geom=c('line'), xlab="Time of Day", ylab='Mean number of steps') + scale_x_datetime(breaks=date_breaks('2 hour'), labels=date_format('%H:%M')) + labs(title='Mean steps per 5-minute interval - Weekdays vs. Weekends')
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

The activity pattern did differ between weekdays and weekends with increased activity starting earlier on weekdays and ending later on weekends.  There also seems to be more activity during the middle hours of the day on weekends than on weekdays.
