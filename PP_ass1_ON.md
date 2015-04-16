# Reproducible Research - Assignment 1
Paulina Popik  
Thursday, April 16, 2015  
This is my first rmarkdown file!
...so please, be tolerant :-)

**To obtain a solution to the problem follow the steps below:**

1. Set global options for this document:


```r
library(knitr)
library(ggplot2) # for plotting our figures
opts_chunk$set(echo = TRUE, results = 'hold') # in accordance with guidelines for the task
```


2. Load my/your data from csv file:


```r
activity <- read.csv('activity.csv', header = TRUE, sep = ",",
                  colClasses=c("numeric", "character", "numeric"))
```

...and preprocessing:


```r
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")
activity$interval <- as.factor(activity$interval)
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...
```

3. Calculate the total number of steps per day:


```r
total_steps_per_day <- aggregate(steps ~ date, activity, sum)
colnames(total_steps_per_day) <- c("date","steps")
head(total_steps_per_day)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

4. Make histogram:


```r
ggplot(total_steps_per_day, aes(x = steps)) + 
       geom_histogram(fill = "blue", binwidth = 1000) + 
        labs(title="Histogram of total steps taken per day by a particular person", 
             x = "Number of steps per day", y = "Number of times in a day(Count)") + theme_bw() 
```

![](PP_ass1_ON_files/figure-html/unnamed-chunk-5-1.png) 

5. Calculate mean and median of the total number of steps taken per day:


```r
total_steps_mean   <- mean(total_steps_per_day$steps, na.rm=TRUE)
total_steps_median <- median(total_steps_per_day$steps, na.rm=TRUE)
total_steps_mean # displaying results
total_steps_median # displaying results
```

```
## [1] 10766.19
## [1] 10765
```

6. Calculate average daily activity pattern:


```r
steps_per_interval <- aggregate(activity$steps, 
                                by = list(interval = activity$interval),
                                FUN=mean, na.rm=TRUE)
steps_per_interval$interval <- 
        as.integer(levels(steps_per_interval$interval)[steps_per_interval$interval])
colnames(steps_per_interval) <- c("interval", "steps")
```

7. Make a plot:


```r
ggplot(steps_per_interval, aes(x=interval, y=steps)) +   
        geom_line(color="red", size=1) +  
        labs(title="Average daily activity pattern", x="Interval", y="Number of steps") +  
        theme_bw()
```

![](PP_ass1_ON_files/figure-html/unnamed-chunk-8-1.png) 

8.  Find the 5-minute interval with the containing the maximum number of steps:


```r
max_interval <- steps_per_interval[which.max(  
        steps_per_interval$steps),]
max_interval # displaying results
```

```
##     interval    steps
## 104      835 206.1698
```

9. Imput missing values - calculate the total number of missing values in the dataset:

```r
missing_values <- sum(is.na(activity$steps))
missing_values # displaying results
```

```
## [1] 2304
```

10.  Fill in all of the missing values in the dataset:


```r
na_filling <- function(data, pervalue) {
        na_index <- which(is.na(activity$steps))
        na_replace <- unlist(lapply(na_index, FUN=function(idx){
                interval = data[idx,]$interval
                pervalue[pervalue$interval == interval,]$steps
        }))
        fill_steps <- activity$steps
        fill_steps[na_index] <- na_replace
        fill_steps
}

activity_fill <- data.frame(  
        steps = na_filling(activity, steps_per_interval),  
        date = activity$date,  
        interval = activity$interval)
str(activity_fill)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...
```

11. Check that are there any missing values remaining:


```r
sum(is.na(activity_fill$steps))
```

```
## [1] 0
```

12. Plot a histogram of the total number of steps taken each day:


```r
fill_steps_per_day <- aggregate(steps ~ date, activity_fill, sum)
colnames(fill_steps_per_day) <- c("date","steps")

ggplot(fill_steps_per_day, aes(x = steps)) + 
       geom_histogram(fill = "violet", binwidth = 1000) + 
        labs(title="Histogram of steps taken per day by a particulat person", 
             x = "number of steps per day", y = "number of times in a day(Count)") + theme_bw() 
```

![](PP_ass1_ON_files/figure-html/unnamed-chunk-13-1.png) 

13. Calculate the mean and median total number of steps taken per day:


```r
steps_mean_fill   <- mean(fill_steps_per_day$steps, na.rm=TRUE)
steps_median_fill <- median(fill_steps_per_day$steps, na.rm=TRUE)
steps_mean_fill # displaying results
steps_median_fill # displaying results
```

```
## [1] 10766.19
## [1] 10766.19
```

14. Answer the questions:

**Do these values differ from the estimates from the first part of the assignment?**

Yes, values do differ slightly:

1.Before filling the data   
-Mean : 10766.189  
-Median: 10765

2.After filling the data  
-Mean : 10766.19  
-Median: 10766.19  

The values after filling the data mean and median are equal.

**What is the impact of imputing missing data on the estimates of the total daily number of steps?**

We can observe that while the mean value remains unchanged, the median value has shifted and virtual matches to the mean.

Since our data has shown a t-student distribution, it seems that the impact of imputing missing values has increase the peak of the histogram, but it's not affect negatively the predictions.

15. Check if there are any differences in activity patterns between weekdays and weekends:


```r
weekdays_steps <- function(data) {
    weekdays_steps <- aggregate(activity$steps, by=list(interval = activity$interval),FUN=mean, na.rm=T)
  
    weekdays_steps$interval <- 
            as.integer(levels(weekdays_steps$interval)[weekdays_steps$interval])
    colnames(weekdays_steps) <- c("interval", "steps")
    weekdays_steps
}

data_by_weekdays <- function(data) {
    data$weekday <- 
            as.factor(weekdays(activity$date)) 
    weekend_data <- subset(data, weekday %in% c("Saturday","Sunday"))
    weekday_data <- subset(data, !weekday %in% c("Saturday","Sunday"))

    weekend_steps <- weekdays_steps(weekend_data)
    weekday_steps <- weekdays_steps(weekday_data)

    weekend_steps$dayofweek <- rep("weekend", nrow(weekend_steps))
    weekday_steps$dayofweek <- rep("weekday", nrow(weekday_steps))

    data_by_weekdays <- rbind(weekend_steps, weekday_steps)
    data_by_weekdays$dayofweek <- as.factor(data_by_weekdays$dayofweek)
    data_by_weekdays
}

data_weekdays <- data_by_weekdays(activity_fill)
```

16. Plot the two data sets side by side for comparison:


```r
ggplot(data_weekdays, aes(x=interval, y=steps)) + 
        geom_line(color="pink") + 
        facet_wrap(~ dayofweek, nrow=2, ncol=1) +
        labs(x="Interval", y="Number of steps") +
        theme_bw()
```

![](PP_ass1_ON_files/figure-html/unnamed-chunk-16-1.png) 

17. Give an answer:

At the graph we can observe that activity on the weekday has the greatest peak from all steps intervals. We can also see that weekends activities has more peaks over a hundred than weekday.   
*This is probably due to the fact that during the week the activity is limited by the work - used the time off from work. During the weekend we can see better distribution of effort along the time.*

#Thank you :)
