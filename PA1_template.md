Reproducible Research: Peer Assessment 1
===========================================

by **Yong Wu**

## Introduction
This is the first assignment for the course *Reproducible Research* from John Hopkins University. In this assignment, we are looking at a data set which was a result of an anoymous person's activity monitoring data from 01 Oct. to 30 Nov. 2012. The data set has three variables:

* **steps**: Number of steps taking in a 5-minute interval (`NA` for missing values)
* **date**: The date on which the measurement was taken in YYYY-MM-DD format
* **interval**: Identifier for the 5-minute interval in which measurement was taken.

## Loading and preprocessing the data
The first step involoved in this analysis is to read the data set into R. Please make sure that the zipped data file is in the current working directory, and we can use the following code to read in the data set.


```r
activity <- read.csv(unz("activity.zip", "activity.csv"),header=T)
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

We also need to convert the `date` variable to a `Date` data type in R to make our analysis easier. 


```r
# convert the variable `date' to the data type of `Date'
activity$date <- as.Date(activity$date)
```


## What is mean total number of steps taken per day?

Once we have loaded the data set, we can conduct some analysis on it. First we would like to see the average number of steps taken per day. As the data contains `NA` values, we need to ignore them to get a true feeling about the data set. Therefore, we create a new data set called `act_na_rm` to filter out all the missing data points.


```r
act_na_rm <- na.omit(activity)
```

We can use `tapply` to summarize the total number of steps taken per day, so that we can plot a histogram of total number of steps per day later.


```r
daily_steps <- tapply(act_na_rm$steps, act_na_rm$date, sum)
```

Now the data set has been summarized on a daily basis, we can plot a histogram of total number of steps per day.


```r
hist(daily_steps, breaks = 10, main = "Histogram of total number of steps per day", 
                               xlab = "Total number of steps per day")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

As we can see from the histogram, the most frequent case include a total number of steps between 10,000 and 12,000 steps, while there are also some cases with total number of steps over 20,000.


```r
mean(daily_steps)
```

```
## [1] 10766.19
```

```r
median(daily_steps)
```

```
## [1] 10765
```

The average number of total steps per day is 10,766.2 steps, while, the median number of total steps per day is 10,765 steps. We can see that the mean and the median are very close to each other.


## What is the average daily activity pattern?

The mean, median and the histogram calculated and plotted so far give us a rough understanding of the data set. Another facet of looking at the data set can be done via the average daily activity pattern. We can use the average for every 5-minute interval during the two months to see the daily activity pattern. The vector `daily_act` averages the number of steps per interval over the two months' time.


```r
daily_act <- tapply(act_na_rm$steps, act_na_rm$interval, mean)
plot(y = daily_act, x = names(daily_act), type = "l", 
     main = "Daily Activity Pattern",
     xlab = "5-Minute-Interval", 
     ylab = "Average number of steps")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

Please note that as the 5-minute intervals were used directly as the x-axis, the numbers (e.g., 500, 1500) on the x-axis would directly translate into hours (i.e, 5:00, 15:00). It can observed that the subject had more steps taken in the morning, especaially between 8am and 9:30am.


```r
daily_act[daily_act == max(daily_act)]
```

```
##      835 
## 206.1698
```

To be more precise, the 5-minute interval with maximum number of steps happened at the `interval` 835, i.e., 8:35am. The maximum number of steps at this time corresponded to 206.2 steps.

## Imputing missing values

As mentioned earlier in this report, there were missing values (coded as `NA`) in the data set. Let's check how many cases of missing values happened for the whole data set, and how many of these were related to the **steps** variable.


```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

```r
sum(is.na(activity))
```

```
## [1] 2304
```

We can see that the two counts of missing values report the same number 2,304, which means that all missing values happened for the **steps** variable. Once these cases are imputed properly, the whole data set would be without `NA`. Imputing these `NAs` would help to address any bias that might have been introduced. We will use the mean of the 5-minute interval to imput these `NAs`.



```r
act_new <- activity
act_new[which(is.na(act_new$steps)),1] <-
        daily_act[as.character(act_new[which(is.na(act_new$steps)),3])]
sum(is.na(act_new))
```

```
## [1] 0
```

The new data set `act_new` has no missing values, which validates our approach to handle missing values and now we have a complete data set. We can, as shown earlier, calculate the total number of steps per day.


```r
daily_steps_new <- tapply(act_new$steps, act_new$date, sum)
```

Now let's plot the histograms for both data sets side by side so that we can make a comparison. We also want to plot the median of total number of steps on the histograms to visually check whether they differ significantly. 


```r
par(mfrow=c(1,2))

hist(daily_steps, breaks = 10, ylim =c(0, 25),
     main = "Discounting NAs", 
     xlab = "Total number of steps per day")

abline(v = median(daily_steps), col = 2, lwd = 2)

hist(daily_steps_new, breaks = 10, ylim =c(0, 25),
     main = "Imputing NAs with interval means)", 
     xlab = "Total number of steps per day")

abline(v = median(daily_steps_new), col = 2, lwd = 2)
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 

We can observe that both histograms have similar shape and the medians of total number of steps look to be close to each other. The noticable difference is that the frequency of total steps between 10,000 and 12,000 increases. This might due to the way how the missing values are imputed as we used the average steps per interval.


```r
mean(daily_steps_new)
```

```
## [1] 10766.19
```

```r
median(daily_steps_new)
```

```
## [1] 10766.19
```

```r
mean(daily_steps_new) - mean(daily_steps)
```

```
## [1] 0
```

```r
median(daily_steps_new) - median(daily_steps)
```

```
## [1] 1.188679
```

The comparison of the `mean` and `median` indicates that the impact of imputing missing data is not significant on these two metrics. However, it should be noted that the difference between the two histograms is noticable.


## Are there differences in activity patterns between weekdays and weekends?

We now would like to check whether the subject's activities differed between weekends and weekdays. We start by creating a new variable **weektime** in the data set `act_new`.

```r
act_new$weektime <- as.factor(ifelse(weekdays(act_new$date) %in% 
                    c("Saturday","Sunday"),"weekend", "weekday"))
str(act_new)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ weektime: Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...
```

Once we have date correspond to `weekend` and `weekday`, we can make a graph to see whether there is a difference. In this case, we used the `lattice` plotting system.


```r
library("plyr")
library("lattice")
wkend_vs_wkday <- ddply (act_new,
                .(interval, weektime), 
                summarize, 
                stepMean = mean(steps))
xyplot (stepMean ~ interval | weektime, 
        data = wkend_vs_wkday,
        type='l',
        layout=c (1, 2))
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15-1.png) 

We can see from the two plots that the subject's activities differ on weekends from weekdays. The subject seems to get up later on weekends and the steps are more spread out across the day on weekends. The intensity of activities, using the maximum number of steps as an indicator, also suggests that the subject is more related on weekends.
