---
title: 'Reproducible Research: Peer Assessment 1'
output:
  html_document:
    keep_md: yes
  pdf_document: default
---


## Loading and preprocessing the data

```r
setwd("C:/Users/Pete/Desktop/coursera")
# read data
activity <- read.csv("activity.csv")

# convert dates
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")

library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.5.3
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
# create a variable for steps per day
activity %>% 
  group_by(date) %>% 
  mutate(steps_day = sum(steps, na.rm = TRUE)) -> activity
```

## What is mean total number of steps taken per day?

```r
hist(activity$steps_day, main = "Steps Per Day", xlab = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
mean_day <- mean(activity$steps_day, na.rm = TRUE)
median_day <- median(activity$steps_day, na.rm = TRUE)
```
The mean number of steps per day is `mean_day`, while the median is `median_day`.

## What is the average daily activity pattern?

```r
# group by time interval and day
activity %>% 
  group_by(interval) %>% 
  mutate(avg_steps = mean(steps, na.rm = TRUE), interval2 = interval) -> activity

#graph
plot(activity$avg_steps ~ activity$interval, type = "l", xlab = "Interval", ylab = "Steps", 
     main = "Steps at Daily Intervals")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
# find the interval with the most average steps
activity %>% arrange(desc(avg_steps)) %>% 
  head(1) -> max_interval
```
The interval with the most steps is `max_interval[1, 6`].

## Imputing missing values

```r
# calculate rows with missing data and display
paste("There are", sum(is.na(activity$steps)), "rows with missing data.")
```

```
## [1] "There are 2304 rows with missing data."
```

```r
# function to fill nas
nafill <- function(activity) {
  for (i in 1:nrow(activity)) {
    activity[i, 1] <- ifelse(is.na(activity[i, 1]), activity[i, 5], activity[i, 1])
  }
  return(activity)
}

# fill in df with mean for the day where values are missing and 
# save in a new dataset called activity 2
activity2 <- nafill(activity)

# create a histogram for steps per day
activity2 %>% 
  group_by(date) %>% 
  mutate(steps_day2 = sum(steps, na.rm = TRUE)) -> activity2
hist(activity2$steps_day2, main = "Steps Per Day", xlab = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
# calculate new mean and median steps per day
paste("The new average daily stepcount is", mean(activity2$steps_day2), ".")
```

```
## [1] "The new average daily stepcount is 10766.1886792453 ."
```

```r
paste("The new median daily stepcount is", median(activity2$steps_day2), ".")
```

```
## [1] "The new median daily stepcount is 10766.1886792453 ."
```


## Are there differences in activity patterns between weekdays and weekends?

```r
# create a weekday column
dayweek <- function(date){
        weekday <- weekdays(as.Date(date))
        if(weekday %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")){
                return("weekday")
        } else if(weekday %in% c("Saturday", "Sunday")){
                return("weekend")
        } else {
                stop("Not a valid date")
        }
}

activity3 <- activity2
activity3$weekday <- sapply(activity3$date, FUN = dayweek)

# create plot
aggdata <- aggregate(steps ~ interval + weekday, data=activity3, mean)
library(lattice)
with(aggdata,
     xyplot(steps ~ interval | weekday, type="l", xlab = "Interval", ylab = "Number of steps", layout = c(1, 2)))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
