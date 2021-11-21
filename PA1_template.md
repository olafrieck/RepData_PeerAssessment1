---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## 1. Load Data


```r
Activity<-read.csv("activity.csv")
```
<br />  
<br />   


## 2. What is mean total number of steps taken per day?

```r
# Prepare Data
SumSteps<-tapply(Activity$steps,Activity$date,sum)
MeanSteps<-tapply(Activity$steps,Activity$date,mean)
MedianSteps<-tapply(Activity$steps,Activity$date,median,na.rm=TRUE)

SumAct<-data.frame(date=unique(Activity$date),SumSteps=SumSteps,MeanSteps=MeanSteps,MedianSteps=MedianSteps,row.names=NULL) 

# Analyze and report
hist(SumAct$SumSteps,main="Histogram of Daily Steps",xlab="Sum of daily steps")
```

![](PA1_template_files/figure-html/Analysis SumSteps-1.png)<!-- -->

```r
MeanofSumSteps<-mean(SumAct$SumSteps,na.rm=TRUE)
MedianofSumSteps<-median(SumAct$SumSteps,na.rm=TRUE)
SumofSumSteps<-sum(SumAct$SumSteps,na.rm=TRUE)
```

### The mean of daily steps is **10766.19**.  
### The median of daily steps is **10765**.
<br />  
<br />   




## 3. What is the average daily activity pattern?

```r
# Prepare DataIn
IntervalMean<-tapply(Activity$steps,Activity$interval,mean,na.rm=TRUE)
IntervalMedian<-tapply(Activity$steps,Activity$interval,median,na.rm=TRUE)
IntervalSum<-tapply(Activity$steps,Activity$interval,sum,na.rm=TRUE)

ActPattern<-data.frame(Interval=unique(Activity$interval),IntervalMean=IntervalMean,IntervalSum=IntervalSum,row.names=NULL) 

plot(ActPattern$IntervalMean,main="Average daily activity pattern",xlab="Time interval",ylab="Average number of steps",type="l")
```

![](PA1_template_files/figure-html/ActivityPattern-1.png)<!-- -->

```r
MaxInt<-names(IntervalSum[which(IntervalSum==max(IntervalSum))])
```

### The interval with the largest number of steps is the interval identifier **835**.
<br />  
<br />   
  


## 4. Imputing missing values


```r
## calculate the number of missing values

MisVal<-sum(is.na(Activity$steps))
```

### The number of missing values in the Dataset is **2304**.

<br />   

### My strategy for replacing missing values is to replace every NA with the average of all value of the that interval. 

<br />   

```r
## replace missing values
## step 1: Expand the Activity matrix by a colum that contains the respective interval means

ExpActivity<-cbind(Activity,stack(IntervalMean))

##step 2; Replace the NAs values with the values from the Means-column

ExpActivity$steps[is.na(ExpActivity$steps)]<-ExpActivity$values[is.na(ExpActivity$steps)]
CleanActivity<-ExpActivity[,c("steps","date","interval")]
head(CleanActivity)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

```r
DailySteps<-tapply(CleanActivity$steps,CleanActivity$date,sum)

hist(DailySteps,main="Histogram of the sum of daily steps - Missing values replaced ",xlab="Sum of daily steps")
```

![](PA1_template_files/figure-html/ReplaceAndAnalze-1.png)<!-- -->

```r
MeanofDailyStepsClean<-mean(DailySteps)
MedianofDailyStepsClean<-median(DailySteps)
SumofDailyStepsClean<-sum(DailySteps)
```


### In the dataset without missing values, the mean of daily steps is **10766.19**.  
<br />  

### In the dataset without missing values, the median of daily steps is **10766.19**.

<br /> 

### The mean has not changed, as NAs were replaced by the means.

<br /> 

### The total sum changed from **570608** (without missing values filled in) to **656737.51** (with missing values filled in).

<br />  
<br />   

## 5. Are there differences in activity patterns between weekdays and weekends?

```r
## Create a weekday factor variable and set it to 1 (for weekday). Then overwrite the values for all Saturdays and Sundays to 0



CleanActivity$weekday<-"weekday"
CleanActivity$weekday[(weekdays(as.Date(CleanActivity$date))=="Sonntag")|(weekdays(as.Date(CleanActivity$date))=="Samstag")]<-"weekend"


## Now create the Activity patterns for 

CleanIntervalMeanWD<-stack(tapply(CleanActivity$steps[CleanActivity$weekday=="weekday"],CleanActivity$interval[CleanActivity$weekday=="weekday"],mean))
CleanIntervalMeanWD$weekday<-as.vector("weekday")

CleanIntervalMeanWE<-stack(tapply(CleanActivity$steps[CleanActivity$weekday=="weekend"],CleanActivity$interval[CleanActivity$weekday=="weekend"],mean))
CleanIntervalMeanWE$weekday<-as.vector("weekend")

CleanIntervalMeanALL<-rbind(CleanIntervalMeanWD,CleanIntervalMeanWE)
head(CleanIntervalMeanALL)
```

```
##       values ind weekday
## 1 2.25115304   0 weekday
## 2 0.44528302   5 weekday
## 3 0.17316562  10 weekday
## 4 0.19790356  15 weekday
## 5 0.09895178  20 weekday
## 6 1.59035639  25 weekday
```

```r
class(CleanIntervalMeanALL$ind)
```

```
## [1] "factor"
```

```r
CleanIntervalMeanALL$ind<-as.integer(CleanIntervalMeanALL$ind)

library(ggplot2)
ggplot(data = CleanIntervalMeanALL, mapping = aes(x = ind, y = values)) + geom_line(color = "red", size = 1) +labs(title = "Comparision of Exercise Patterns", subtitle = "AVG number of steps at different times of the day", y = "Average number of steps", x = "time interval")+
    geom_line() + facet_wrap(~weekday, nrow = 2)
```

![](PA1_template_files/figure-html/Analyze_weekdays-1.png)<!-- -->
<br />  
<br />   

# THE END
