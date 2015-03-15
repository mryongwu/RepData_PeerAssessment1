# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
The first step involoved in this analysis is to read the data into R. Please make sure that the zipped data file is in the current working directory, and we can use the following code to read in the data set.


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

Once we have loaded the data set, we can conduct some analysis on it. First we would like to see the average number of steps taken per day. As the data contains `NA` values, we need to ignore them to get a true feeling about the data set.


```r
act_na_rm <- na.omit(activity)
```



```r
daily_steps <- tapply(act_na_rm$steps, act_na_rm$date, sum)
```



```r
hist(daily_steps, breaks = 10, main = "Histogram of total number of steps per day", 
                               xlab = "Total number of steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 



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



## What is the average daily activity pattern?


```r
daily_act <- tapply(act_na_rm$steps, act_na_rm$interval, mean)
plot(y = daily_act, x = names(daily_act), type = "l", 
     main = "Daily Activity Pattern",
     xlab = "5-Minute-Interval", 
     ylab = "Average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 



```r
daily_act[daily_act == max(daily_act)]
```

```
##      835 
## 206.1698
```


## Imputing missing values


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



```r
act_new <- activity
act_new[which(is.na(act_new$steps)),1] <-
        daily_act[as.character(act_new[which(is.na(act_new$steps)),3])]
```


```r
daily_steps_new <- tapply(act_new$steps, act_new$date, sum)
```


Now let's plot the histogram for both data sets side by side.

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

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png) 



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
The impact of imputing missing data is minimal, as only the median seems to be changing but by just over one step.

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

## Are there differences in activity patterns between weekdays and weekends?

```r
act_new$weektime <- as.factor(ifelse(weekdays(act_new$date) %in% 
                    c("Saturday","Sunday"),"weekend", "weekday"))
```

act_new$wd<-weekdays(act_new$date)
act_new$fwd<- as.factor(c("weekend", "weekday"))
act_new[act_new$wd == "Sunday" | act_new$wd == "Saturday" ,5]<- factor("weekend")
act_new[!(act_new$wd == "Sunday" | act_new$wd == "Saturday"),5 ]<- factor("weekday")
Now we will create two aggregated arrays for the total number of steps taken per 5-minyute time interval for weekdays and weekends, and make a graph in order to compare it there is a difference.

Note that the plot has been created in the base system.


spi <- ddply (fixie,
              .(interval, dayTypes), 
              summarize, 
              stepMean = mean(steps, na.rm=TRUE))
xyplot (stepMean ~ interval | dayTypes, 
        data=spi,
        type='l',
        layout=c (1, 2))

library("lattice")
p <- xyplot(mean.steps ~ interval | factor(weektime), data=t5, 
       type = 'l',
       main="Average Number of Steps Taken 
       \nAveraged Across All Weekday Days or Weekend Days",
       xlab="5-Minute Interval (military time)",
       ylab="Average Number of Steps Taken")
print (p)    


```r
wkend <- subset(act_new, weektime == "weekend") 
wkday <- subset(act_new, weektime == "weekday") 
wkend_avg <- tapply(wkend$steps, wkend$interval, mean)
wkday_avg <- tapply(wkday$steps, wkday$interval, mean)

par(mfrow=c(2,1))
plot(y = wkday_avg, x = names(wkday_avg), type = "l", xlab = "5-Minute Interval", 
     main = "Daily Activity Pattern on Weekdays", ylab = "Average number of steps", 
     ylim =c(0, 250))
plot(y = wkend_avg, x = names(wkend_avg), type = "l", xlab = "5-Minute Interval", 
     main = "Daily Activity Pattern on Weekends", ylab = "Average number of steps", 
     ylim =c(0, 250))
```

![](PA1_template_files/figure-html/unnamed-chunk-17-1.png) 
