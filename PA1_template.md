---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

``` r
data <- read.csv("activity.csv")
data$date <- as.Date(data$date)
```

## What is mean total number of steps taken per day?

``` r
daily_steps <- aggregate(steps ~ date, data, sum, na.rm = TRUE)
hist(daily_steps$steps, main = "Total Steps per Day", xlab = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

``` r
mean(daily_steps$steps)
```

```
## [1] 10766.19
```

``` r
median(daily_steps$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

``` r
interval_avg <- aggregate(steps ~ interval, data, mean, na.rm = TRUE)
plot(interval_avg$interval, interval_avg$steps, type = "l",
     xlab = "Interval", ylab = "Average steps", main = "Daily Activity Pattern")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

``` r
interval_avg[which.max(interval_avg$steps), ]
```

```
##     interval    steps
## 104      835 206.1698
```

``` r
## Imputing missing values
sum(is.na(data$steps))
```

```
## [1] 2304
```

``` r
# Impute using mean for the interval
data_imputed <- data
data_imputed$steps <- ifelse(is.na(data$steps),
                             interval_avg$steps[match(data$interval, interval_avg$interval)],
                             data$steps)
```

## Are there differences in activity patterns between weekdays and weekends?

``` r
data_imputed$day <- ifelse(weekdays(data_imputed$date) %in% c("Saturday", "Sunday"),
                           "weekend", "weekday")

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

``` r
avg_by_day_type <- data_imputed %>%
  group_by(interval, day) %>%
  summarise(avg_steps = mean(steps), .groups = "drop")

library(lattice)
xyplot(avg_steps ~ interval | day, data = avg_by_day_type, type = "l", layout = c(1, 2))
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->
