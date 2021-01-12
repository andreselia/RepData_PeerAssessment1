---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
dataurl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

if (!file.exists("data.zip")) {
        download.file(url = dataurl, destfile = "data.zip", method = "curl") #Download the file
        unzip("data.zip") 
        activity <- read.csv("activity.csv") # Load data
        activity$date <- as.Date(activity$date, "%Y-%m-%d") # Change to date format
        }
# Check the data
colnames(activity) <- c("steps", "dates", "interval")
summary(activity)
```

```
##      steps            dates               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```

## What is mean total number of steps taken per day?

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
mean_steps_day <- activity %>% group_by(dates) %>% summarise(mean_steps = sum(steps, na.rm = TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
ggplot(data=mean_steps_day) + geom_histogram( aes(mean_steps), binwidth = 2000) +
        labs(title = "Total steps taken by day") +
        ylab("Number fo days") +
        xlab("Number of steps") +
        geom_vline(aes(xintercept = mean(mean_steps, na.rm = TRUE)), color = "red", linetype = "dashed")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
meansteps <- mean(mean_steps_day$mean_steps, na.rm = TRUE)
mediansteps <- median(mean_steps_day$mean_steps, na.rm = TRUE)
```
The mean steps per day is 9354.2295082 and the median is 10395


## What is the average daily activity pattern?


```r
steps_interval <- activity %>% group_by(interval) %>% summarise(steps = sum(steps, na.rm = TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
ggplot(data= steps_interval, aes(x= interval, y= steps)) + geom_line() +
        labs(title = "Time series plot of the aveergae number of steps in a 5-minutes interval") +
        xlab("Interval") + 
        ylab("Average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
max_interval <- steps_interval[which.max(steps_interval$steps),]$interval
```

The 835 interval contains the maximum number of steps.


## Imputing missing values

```r
number_nas <- sum(is.na(activity))
```

The number of NAs in the dataset is 2304, therefore I will replace them with the mean of the 5-minute interval and call the dataset "data_new".


```r
# I couldn`t find a more "sophisticated" way
steps_interval_mean <- activity %>% group_by(interval) %>% summarise(steps = round(mean(steps, na.rm = TRUE)))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
data_new <- merge(activity, steps_interval_mean, by = "interval")
data_new$steps <- coalesce(data_new$steps.x, data_new$steps.y)
data_new <- select(data_new,-c("steps.x", "steps.y"))
summary(data_new)
```

```
##     interval          dates                steps       
##  Min.   :   0.0   Min.   :2012-10-01   Min.   :  0.00  
##  1st Qu.: 588.8   1st Qu.:2012-10-16   1st Qu.:  0.00  
##  Median :1177.5   Median :2012-10-31   Median :  0.00  
##  Mean   :1177.5   Mean   :2012-10-31   Mean   : 37.38  
##  3rd Qu.:1766.2   3rd Qu.:2012-11-15   3rd Qu.: 27.00  
##  Max.   :2355.0   Max.   :2012-11-30   Max.   :806.00
```

```r
mean_steps_day2 <- data_new %>% group_by(dates) %>% summarise(mean_steps = sum(steps, na.rm = TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
ggplot(data=mean_steps_day2) + geom_histogram( aes(mean_steps), binwidth = 2000) +
        labs(title = "Total steps taken by day (with Na values imputed)") +
        ylab("Number fo days") +
        xlab("Number of steps") +
        geom_vline(aes(xintercept = mean(mean_steps, na.rm = TRUE)), color = "red", linetype = "dashed")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
meansteps2 <- mean(mean_steps_day2$mean_steps, na.rm = TRUE)
mediansteps2 <- median(mean_steps_day2$mean_steps, na.rm = TRUE)
```
According to the modified dataset the mean steps per day is 1.0765639\times 10^{4} and the median is 1.0762\times 10^{4}
There is a slighty difference with the imputed data. Althoguh the mean is almost the same, a higher number of days is found with around 10000 steps. 

## Are there differences in activity patterns between weekdays and weekends?

```r
#Create a new factor variable with levels weekdays and weekend
data_new$day <- weekdays(data_new$dates)
data_new$day[data_new$day %in% c("sábado","domingo")] <- "Weekend"
data_new$day[data_new$day != "Weekend"] <- "Weekday"
data_new$day <- as.factor(data_new$day)

mean_week_interval <- data_new %>% group_by(day, interval) %>% summarise(steps = mean(steps, na.rm = TRUE))
```

```
## `summarise()` regrouping output by 'day' (override with `.groups` argument)
```

```r
ggplot(data= mean_week_interval, aes(x= interval, y= steps)) + geom_line() + facet_grid(rows = vars(day)) +
        labs(title = "Time series plot of the aveergae number of steps in a 5-minutes interval") +
        xlab("Interval") + 
        ylab("Average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
