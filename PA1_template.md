# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
setwd("~/Desktop/github/RepData_PeerAssessment1")
activity<- read.csv("activity.csv")
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


## What is mean total number of steps taken per day?

```r
total_steps_per_day <- tapply(activity$steps, activity$date, sum)
hist(total_steps_per_day, breaks = 10)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
average_daily<- mean(total_steps_per_day, na.rm = TRUE)
median_daily<- median(total_steps_per_day, na.rm = TRUE)
average_daily
```

```
## [1] 10766.19
```

```r
median_daily
```

```
## [1] 10765
```


## What is the average daily activity pattern?

```r
mean_steps_per_interval<- tapply(activity$steps, activity$interval, mean, na.rm = TRUE)
str(mean_steps_per_interval)
```

```
##  num [1:288(1d)] 1.717 0.3396 0.1321 0.1509 0.0755 ...
##  - attr(*, "dimnames")=List of 1
##   ..$ : chr [1:288] "0" "5" "10" "15" ...
```

```r
plot(mean_steps_per_interval, type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
max(mean_steps_per_interval)
```

```
## [1] 206.1698
```

```r
my_obs<- which(mean_steps_per_interval== max(mean_steps_per_interval))
my_obs
```

```
## 835 
## 104
```

## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
