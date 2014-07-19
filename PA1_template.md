Reproducible Research - Peer Assessment 1.
========================================================

## Loading the dependencies


```r
require("lattice")
```

```
## Loading required package: lattice
```

```
## Warning: package 'lattice' was built under R version 3.0.3
```

```r
require("plyr")
```

```
## Loading required package: plyr
```

```
## Warning: package 'plyr' was built under R version 3.0.3
```

```r
library(plyr)
library(lattice)
```


## Loading and Processing the Data

### 1. Load the data


```r
# Set the working directory
setwd("D:/Archiv/Vladimir/projekt/course/coursera/Reproducible_Research/Projekt_1")
# Read the data from csv-file
df <- read.csv("activity.csv")
# Summarized the data
summary(df)
```

```
##      steps               date          interval   
##  Min.   :  0.0   2012-10-01:  288   Min.   :   0  
##  1st Qu.:  0.0   2012-10-02:  288   1st Qu.: 589  
##  Median :  0.0   2012-10-03:  288   Median :1178  
##  Mean   : 37.4   2012-10-04:  288   Mean   :1178  
##  3rd Qu.: 12.0   2012-10-05:  288   3rd Qu.:1766  
##  Max.   :806.0   2012-10-06:  288   Max.   :2355  
##  NA's   :2304    (Other)   :15840
```


### 2. Process/transform the data if necessary

The data column should be transformed into the standard Date format. 


```r
df$date <- as.Date(df$date)
summary(df)
```

```
##      steps            date               interval   
##  Min.   :  0.0   Min.   :2012-10-01   Min.   :   0  
##  1st Qu.:  0.0   1st Qu.:2012-10-16   1st Qu.: 589  
##  Median :  0.0   Median :2012-10-31   Median :1178  
##  Mean   : 37.4   Mean   :2012-10-31   Mean   :1178  
##  3rd Qu.: 12.0   3rd Qu.:2012-11-15   3rd Qu.:1766  
##  Max.   :806.0   Max.   :2012-11-30   Max.   :2355  
##  NA's   :2304
```


## What is mean total number of steps taken per day?

### 1. Make a histogram of the total steps taken each day

Creating the new data frame containing the sum of steps for each day. 


```r
total_df <- ddply(df, ~date, summarise, total_steps = sum(steps, na.rm = TRUE))
summary(total_df)
```

```
##       date             total_steps   
##  Min.   :2012-10-01   Min.   :    0  
##  1st Qu.:2012-10-16   1st Qu.: 6778  
##  Median :2012-10-31   Median :10395  
##  Mean   :2012-10-31   Mean   : 9354  
##  3rd Qu.:2012-11-15   3rd Qu.:12811  
##  Max.   :2012-11-30   Max.   :21194
```


Then we simply plot a histogram of the total steps. We use breaks=10 for clarity. 


```r
hist(total_df$total_steps, breaks = 10, main = "Histogram of Total Steps", xlab = "Total Steps", 
    ylab = "Frequency")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 


### 2. Calculate and report the mean and median

Using the data frame was created above.


```r
median(total_df$total_steps)
```

```
## [1] 10395
```

```r
mean(total_df$total_steps)
```

```
## [1] 9354
```


## What is the average daily activity pattern?

### 1. Make a time series plot of average steps taken vs. interval 

To plot this graph, it is necessary to summarise the mean steps accross each interval.


```r
idf <- ddply(df, ~interval, summarise, meansteps = mean(steps, na.rm = TRUE))
summary(idf)
```

```
##     interval      meansteps     
##  Min.   :   0   Min.   :  0.00  
##  1st Qu.: 589   1st Qu.:  2.49  
##  Median :1178   Median : 34.11  
##  Mean   :1178   Mean   : 37.38  
##  3rd Qu.:1766   3rd Qu.: 52.83  
##  Max.   :2355   Max.   :206.17
```


Creating the plot.


```r
plot(idf$interval, idf$meansteps, type = "l", main = "", xlab = "Interval", 
    ylab = "Average Steps")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 


### 2. Which 5-minute interval has the highest average number of steps? 

The maximum value in the meansteps column and print the interval. 


```r
max_meansteps <- idf[which.max(idf$meansteps), ]
max_meansteps$interval
```

```
## [1] 835
```


## Imputing missing values

### 1. Calculate and report the total number of missing values in the data set. 

The missing values in the dataset codes as NA. To calculate the total numbers of missing values will be used is.na() and sum() functions. 


```r
missing_values <- sum(is.na(df$steps))
missing_values
```

```
## [1] 2304
```


### 2. Strategy to fill in missing values

To fill the missing values, will be used the strategy to insert the mean for each 5 minute interval. To accomplish this, is necessary to calculate first the mean for each interval over all days. After that with help of plyr the mean will be applied to each missing value. 


```r
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
imputed_df <- ddply(df, ~interval, transform, steps = impute.mean(steps))

# Sort the imputed data
imputed_df <- df[with(imputed_df, order(date, interval)), ]
```


### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

The imputed_df data set, which was created above, has the missing data filled in. 


```r
summary(imputed_df)
```

```
##      steps            date               interval   
##  Min.   :  0.0   Min.   :2012-10-01   Min.   :   0  
##  1st Qu.:  0.0   1st Qu.:2012-10-16   1st Qu.: 589  
##  Median :  0.0   Median :2012-10-31   Median :1178  
##  Mean   : 37.4   Mean   :2012-10-31   Mean   :1178  
##  3rd Qu.: 12.0   3rd Qu.:2012-11-15   3rd Qu.:1766  
##  Max.   :806.0   Max.   :2012-11-30   Max.   :2355  
##  NA's   :2304
```


### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

To plot a histogram and calculate mean and median on the imputed data set is has to be repeated the plotting and calculating steps above (s. Chapter "What is the average daily activity pattern").

The first step is creating a new data frame with the total steps for each day. 


```r
imputed_tdf <- ddply(imputed_df, ~date, summarise, totalsteps = sum(steps, na.rm = TRUE))
summary(total_df)
```

```
##       date             total_steps   
##  Min.   :2012-10-01   Min.   :    0  
##  1st Qu.:2012-10-16   1st Qu.: 6778  
##  Median :2012-10-31   Median :10395  
##  Mean   :2012-10-31   Mean   : 9354  
##  3rd Qu.:2012-11-15   3rd Qu.:12811  
##  Max.   :2012-11-30   Max.   :21194
```


After that plot a histogram for the imputed totals


```r
hist(imputed_tdf$totalsteps, breaks = 10, main = "Total Steps of Imputed Data", 
    xlab = "Total Steps", ylab = "Frequency")
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14.png) 


Finally calculate the mean and median.


```r
median(imputed_tdf$totalsteps)
```

```
## [1] 10395
```

```r
mean(imputed_tdf$totalsteps)
```

```
## [1] 9354
```


#### 4.1 Do these values differ from the estimates from the first part of the assignment? 

Because of using the mean value of each interval to imoute the missing values, the the mean and median to differ from those above.

#### 4.2 What is the impact of imputing missing data on the estimates of the total daily number of steps?

The imputing method does not have have any effect. The summary and plot could verify this. 

```r
imputed_idf <- ddply(imputed_df, ~interval, summarise, meansteps = mean(steps, 
    na.rm = TRUE))
summary(imputed_idf)
```

```
##     interval      meansteps     
##  Min.   :   0   Min.   :  0.00  
##  1st Qu.: 589   1st Qu.:  2.49  
##  Median :1178   Median : 34.11  
##  Mean   :1178   Mean   : 37.38  
##  3rd Qu.:1766   3rd Qu.: 52.83  
##  Max.   :2355   Max.   :206.17
```

```r
plot(imputed_idf$interval, imputed_idf$meansteps, type = "l", main = "Imputed Data", 
    xlab = "Interval", ylab = "Average Steps")
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16.png) 


There is no any difference in the imputed data.

## Are there differences in activity patterns between weekdays and weekends?

### 1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

First create a boolean factor, after that map the boolean values to "weekend" or "weekday".


```r
df$daytype <- weekdays(df$date) %in% c("Samstag", "Sonntag")
df$daytype <- mapvalues(df$daytype, from = c(TRUE, FALSE), to = c("weekend", 
    "weekday"))
head(df)
```

```
##   steps       date interval daytype
## 1    NA 2012-10-01        0 weekday
## 2    NA 2012-10-01        5 weekday
## 3    NA 2012-10-01       10 weekday
## 4    NA 2012-10-01       15 weekday
## 5    NA 2012-10-01       20 weekday
## 6    NA 2012-10-01       25 weekday
```


### 2. Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

First of all calculate the mean steps for each time interval over weekends and weekdays separately. To do this the data will be splited into separate frames for weekends and weekdays and then the mean will be assigned to each interval using plyr. 


```r
weekdays_df <- df[df$daytype == "weekday", ]
weekdays_idf <- ddply(weekdays_df, ~interval, summarise, meansteps = mean(steps, 
    na.rm = TRUE))
weekdays_idf$daytype <- "weekday"

weekends_df <- df[df$daytype == "weekend", ]
weekends_idf <- ddply(weekends_df, ~interval, summarise, meansteps = mean(steps, 
    na.rm = TRUE))
weekends_idf$daytype <- "weekend"
```


After that the data frames will be binded back together. 


```r
binded_df <- rbind(weekends_idf, weekdays_idf)
summary(binded_df)
```

```
##     interval      meansteps        daytype         
##  Min.   :   0   Min.   :  0.00   Length:576        
##  1st Qu.: 589   1st Qu.:  1.85   Class :character  
##  Median :1178   Median : 26.29   Mode  :character  
##  Mean   :1178   Mean   : 39.21                     
##  3rd Qu.:1766   3rd Qu.: 62.32                     
##  Max.   :2355   Max.   :234.10
```


Creating multi-panel plots the lattice system. 


```r
xyplot(meansteps ~ interval | daytype, data = binded_df, type = "l", main = "", 
    xlab = "Interval", ylab = "Average Steps")
```

![plot of chunk unnamed-chunk-20](figure/unnamed-chunk-20.png) 


The plot of weekdays and weekends are not identical. There is a difference between both.

It shows that normally on weekdays, people are more active in the mornings (maybe the way to work etc.) and sedentary for the day (maybe the work in the office). On the weeken the people are not so active in the mornings, because of the possible relaxing after hard woking week.
