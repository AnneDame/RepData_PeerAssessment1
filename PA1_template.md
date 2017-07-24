# Reproducible Research - Course Project 1
Anne Suchel  
Monday, July 24, 2017  

# Loading and preprocessing the data

My working directory will be :

```r
setwd("C:/documents/s626191/Documents/Bibliotheque/Formation Coursera/Reproductible Research/Week 2/Course Project")
```

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

### Loading the data :

```r
data <- read.csv("activity.csv")
```

The variables included in this dataset are:

* steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

* date: The date on which the measurement was taken in YYYY-MM-DD format

* interval: Identifier for the 5-minute interval in which measurement was taken

### Overview of the data :

```r
summary(data)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

```r
head(data)
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

### Processing the data :


```r
data$date <- as.Date(data$date,"%Y-%m-%d")
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
head(data)
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

# What is mean total number of steps taken per day ?

We need to load the following R packages :

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

For this part of the assignment, we will ignore the missing values in the dataset.

Total number of steps taken per day :

```r
Steps.per.day <- data %>% group_by(date) %>% summarize(Day.Total.Steps = sum(steps));

head(Steps.per.day)
```

```
## Source: local data frame [6 x 2]
## 
##         date Day.Total.Steps
##       (date)           (int)
## 1 2012-10-01              NA
## 2 2012-10-02             126
## 3 2012-10-03           11352
## 4 2012-10-04           12116
## 5 2012-10-05           13294
## 6 2012-10-06           15420
```

Histogram of the total number of steps taken each day :

```r
ggplot(Steps.per.day, aes(x = factor(date), y = Day.Total.Steps)) + geom_bar(stat = "identity") + xlab("date") + ylab(expression("Total number of Steps taken")) + 
  ggtitle(expression("Total number of steps taken per day")) +
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5, hjust = 1))
```

```
## Warning: Removed 8 rows containing missing values (position_stack).
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

Mean of the total number of steps taken per day :

```r
mean(Steps.per.day$Day.Total.Steps,na.rm=TRUE)
```

```
## [1] 10766.19
```

Median of the total number of steps taken per day :

```r
median(Steps.per.day$Day.Total.Steps,na.rm=TRUE)
```

```
## [1] 10765
```

# What is the average daily activity pattern ?

Making a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis):


```r
Steps.per.interval <- data %>% group_by(interval) %>% summarize(Interval.Average.Steps = mean(steps,na.rm=TRUE));

head(Steps.per.interval)
```

```
## Source: local data frame [6 x 2]
## 
##   interval Interval.Average.Steps
##      (int)                  (dbl)
## 1        0              1.7169811
## 2        5              0.3396226
## 3       10              0.1320755
## 4       15              0.1509434
## 5       20              0.0754717
## 6       25              2.0943396
```

```r
plot(Steps.per.interval,type="l",ylab="Average number of steps",main="Average number of steps per interval across all days")
abline(v=c(800,900),col="red")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
Steps.per.interval[which.max(Steps.per.interval$Interval.Average.Steps),1]
```

```
## Source: local data frame [1 x 1]
## 
##   interval
##      (int)
## 1      835
```

# Imputing missing values

There are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

Total number of missing values in the dataset :

```r
Missing.values <- is.na(data$steps)
sum(Missing.values)
```

```
## [1] 2304
```

```r
sum(Missing.values)/length(data$steps)
```

```
## [1] 0.1311475
```

Strategy for filling in all of the missing values in the dataset : for each missing value, we impute the mean for that 5-minute interval.

New dataset with the missing data filled in :

```r
data2 <- merge(data,Steps.per.interval,by="interval",all.x=TRUE)

data2[which(is.na(data2[,"steps"])),"steps"]<-data2[which(is.na(data2[,"steps"])),"Interval.Average.Steps"]

head(data2)
```

```
##   interval    steps       date Interval.Average.Steps
## 1        0 1.716981 2012-10-01               1.716981
## 2        0 0.000000 2012-11-23               1.716981
## 3        0 0.000000 2012-10-28               1.716981
## 4        0 0.000000 2012-11-06               1.716981
## 5        0 0.000000 2012-11-24               1.716981
## 6        0 0.000000 2012-11-15               1.716981
```

Histogram of the total number of steps taken each day :

```r
Steps.per.day2 <- data2 %>% group_by(date) %>% summarize(Day.Total.Steps = sum(steps,na.rm=TRUE));

head(Steps.per.day2)
```

```
## Source: local data frame [6 x 2]
## 
##         date Day.Total.Steps
##       (date)           (dbl)
## 1 2012-10-01        10766.19
## 2 2012-10-02          126.00
## 3 2012-10-03        11352.00
## 4 2012-10-04        12116.00
## 5 2012-10-05        13294.00
## 6 2012-10-06        15420.00
```

```r
ggplot(Steps.per.day2, aes(x = factor(date), y = Day.Total.Steps)) + geom_bar(stat = "identity") + xlab("date") + ylab(expression("Total number of Steps taken")) + 
  ggtitle(expression("Total number of steps taken per day")) +
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5, hjust = 1))
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

Mean of the total number of steps taken per day :

```r
mean(Steps.per.day2$Day.Total.Steps)
```

```
## [1] 10766.19
```

Median of the total number of steps taken per day :

```r
median(Steps.per.day2$Day.Total.Steps)
```

```
## [1] 10766.19
```

Imputing missing data on the estimates of the total daily number of steps :

* does not have an impact on the mean since it does note differ from the first part of the assignment. The raison is that the missing values are equal to the mean of the 5-minutes interval.

* does have a very slight impact on the median.

# Are there differences in activity patterns weekdays and weekends ?

For this part, the dataset with the filled-in missing values is used.

Creation of a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day :

```r
data3 <- data.frame(data2,Weekdays = weekdays(data2$date),Weekend.Or.Not=rep(0,length(data2[,1])))
for(i in 1:length(data3[,1])){
  if ( (data3[i,5]== "samedi") |(data3[i,5]== "dimanche")){
    data3[i,6] <- "weekend"
  } else {
    data3[i,6] <- "weekday"
  }
}
head(data3)
```

```
##   interval    steps       date Interval.Average.Steps Weekdays
## 1        0 1.716981 2012-10-01               1.716981    lundi
## 2        0 0.000000 2012-11-23               1.716981 vendredi
## 3        0 0.000000 2012-10-28               1.716981 dimanche
## 4        0 0.000000 2012-11-06               1.716981    mardi
## 5        0 0.000000 2012-11-24               1.716981   samedi
## 6        0 0.000000 2012-11-15               1.716981    jeudi
##   Weekend.Or.Not
## 1        weekday
## 2        weekday
## 3        weekend
## 4        weekday
## 5        weekend
## 6        weekday
```

Time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
Steps.per.interval.per.weekday <- data3 %>% group_by(Weekend.Or.Not,interval) %>% summarize(Interval.Average.Steps = mean(steps));

library(lattice)

xyplot( Steps.per.interval.per.weekday$Interval.Average.Steps ~ Steps.per.interval.per.weekday$interval | Steps.per.interval.per.weekday$Weekend.Or.Not,layout=c(1,2),type='l',xlab="Interval",ylab="Average Steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-18-1.png)<!-- -->
