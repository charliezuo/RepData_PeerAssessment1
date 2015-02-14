---
title: "RR_PA1.rmd"
author: "Charlie Zuo"
output: html_document
---

## Loading and preprocessing the Activity Dataset


```r
setwd("C:/Users/Charlie/Desktop/Data Science/Reproducible Research/repdata-data-activity")
repdata <- read.csv ("activity.csv", sep=",")
dates <- as.Date(repdata[,2],format="%Y-%m-%d")
repdata1 <- cbind(repdata, dates)
head(repdata1)
```

```
##   steps       date interval      dates
## 1    NA 2012-10-01        0 2012-10-01
## 2    NA 2012-10-01        5 2012-10-01
## 3    NA 2012-10-01       10 2012-10-01
## 4    NA 2012-10-01       15 2012-10-01
## 5    NA 2012-10-01       20 2012-10-01
## 6    NA 2012-10-01       25 2012-10-01
```

## What is mean total number of steps taken per day?  
**For the calculations, we summarize this dataset with the very useful reshape2 package**


```r
require(reshape2)
sumdata <- melt(repdata1, id.vars = c("dates", "interval"), measure.vars="steps", na.rm=TRUE)
newsum <- tapply(sumdata$value,sumdata$dates,sum)
newsum <- data.frame(newsum)
hist(as.numeric(newsum[,1]), main = "Steps Per Day", xlab="", col="blue")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

```r
mean(newsum[,1])
```

```
## [1] 10766.19
```

```r
median(newsum[,1])
```

```
## [1] 10765
```
## What is the average daily activity pattern?
**Time Series Plot for average number of steps taken, and Calculating the interval contains maximum number of steps**

```r
require(reshape2)
sumdata2 <- melt(repdata1, id.vars = c("interval"), measure.vars="steps", na.rm = TRUE)
alldays <- dcast(sumdata2, interval ~ variable, mean)
plot(alldays, type="l",lwd="3",cex=288, col='red')
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
head(alldays[order(alldays$steps,decreasing=TRUE),], n=1L)
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values
**Total number of rows with `NAs`**


```r
length(repdata[is.na(repdata)])
```

```
## [1] 2304
```

**Replacing `NAs` with the mean value of that interval, and data frame repdata2 is the new dataset**

```r
require(lubridate)
join <- merge(repdata, alldays, by.x="interval", by.y="interval")
head(join)
```

```
##   interval steps.x       date  steps.y
## 1        0      NA 2012-10-01 1.716981
## 2        0       0 2012-11-23 1.716981
## 3        0       0 2012-10-28 1.716981
## 4        0       0 2012-11-06 1.716981
## 5        0       0 2012-11-24 1.716981
## 6        0       0 2012-11-15 1.716981
```

```r
join$date <- as.Date(join[,3],format="%Y-%m-%d")
NAs <- join[is.na(join$steps.x),] 
NAs[,2] <- as.numeric(NAs[,4])
row.names(NAs) <- NULL
new <- NAs[,c(2,3,1,4)]
new1 <- new[,1:3]
names(new1) <- names(repdata)
repdata2 <- rbind(new1,repdata)
repdata2 <- repdata2[!is.na(repdata2[,1]),]
head(repdata2)
```

```
##      steps       date interval
## 1 1.716981 2012-10-01        0
## 2 1.716981 2012-11-04        0
## 3 1.716981 2012-11-30        0
## 4 1.716981 2012-11-14        0
## 5 1.716981 2012-11-09        0
## 6 1.716981 2012-11-01        0
```


```r
require(reshape2)
repdata2$date <- as.Date(repdata2[,2],format="%Y-%m-%d")
#sumdata <- melt(repdata2, id.vars = c("date", "interval"), measure.vars="steps")
newsum2 <- tapply(repdata2$steps,repdata2$date,sum)
newsum2 <- data.frame(newsum2)
hist(as.numeric(newsum2[,1]), main = "Steps Per Day", xlab="", col="red")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

```r
mean(newsum2[,1])
```

```
## [1] 10766.19
```

```r
median(newsum2[,1])
```

```
## [1] 10766.19
```
**We can conclude mean value did not differ from the first part of the assignment, but median value now is the same as mean value. In the first part of assignment, mean and median were not the same.**

## Are there differences in activity patterns between weekdays and weekends?


```r
library(lattice)
Day <- weekdays(repdata2$date)
Day[Day=="Sunday"|Day=="Saturday"] <- 'Weekend'
Day[Day!="Weekend"] <- 'Weekday'
Day <- data.frame(Day)
repdata3 <- cbind(repdata2,Day)
head(repdata3)
```

```
##      steps       date interval     Day
## 1 1.716981 2012-10-01        0 Weekday
## 2 1.716981 2012-11-04        0 Weekend
## 3 1.716981 2012-11-30        0 Weekday
## 4 1.716981 2012-11-14        0 Weekday
## 5 1.716981 2012-11-09        0 Weekday
## 6 1.716981 2012-11-01        0 Weekday
```

```r
sumdata3 <- melt(repdata3, id.vars = c("interval","Day"), measure.vars="steps", na.rm = TRUE)
weekdata <- dcast(sumdata3, interval + Day ~ variable, mean)
xyplot(steps ~ interval|Day, weekdata,type='l',layout = c(1,2),xlab = 'Interval', ylab = 'Number of Steps')
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

