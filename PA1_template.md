# Reproducible Research: Peer Assessment 1
Mano Yakandawala  
#### Report created on : Thu Oct 13 2016



## Loading and preprocessing the data

```r
#Read the data.
alldata = read.csv("activity.csv")
#we want to group the data by date and the summarise by date to get the count
ProData =  alldata %>% group_by(date) %>%
        summarise(total = sum(steps))
```

## What is mean total number of steps taken per day?

```r
ggplot(ProData, aes(x=date, y=total)) + 
        geom_bar(stat="identity", color="black", fill="wheat") + 
        labs(x="Date", y="Total Steps", title="Total Steps per Day")
```

```
## Warning: Removed 8 rows containing missing values (position_stack).
```

![](PA1_template_files/figure-html/MeanTotal-1.png)<!-- -->

```r
ggsave("figures/TotalStepsPerDay.png")
```

```
## Saving 7 x 5 in image
```

```
## Warning: Removed 8 rows containing missing values (position_stack).
```

```r
summary(ProData)
```

```
##          date        total      
##  2012-10-01: 1   Min.   :   41  
##  2012-10-02: 1   1st Qu.: 8841  
##  2012-10-03: 1   Median :10765  
##  2012-10-04: 1   Mean   :10766  
##  2012-10-05: 1   3rd Qu.:13294  
##  2012-10-06: 1   Max.   :21194  
##  (Other)   :55   NA's   :8
```

```r
mean(ProData$total, na.rm = TRUE) #Mean Steps Taken.
```

```
## [1] 10766.19
```

```r
median(ProData$total, na.rm = TRUE) #Median Steps Taken.
```

```
## [1] 10765
```

## What is the average daily activity pattern?

```r
AvgDailyData =  na.omit(alldata %>% group_by(interval))  %>%
        summarise(averageSteps = mean(steps))

ggplot(AvgDailyData, aes(x=interval, y=averageSteps)) + 
        geom_line() + 
        labs(x="Time Interval", y="Average Steps", title="Average Steps per Time Interval of 5 mins")
```

![](PA1_template_files/figure-html/MeanTotal2-1.png)<!-- -->

```r
ggsave("figures/AverageStepsPerTimeInterval.png")
```

```
## Saving 7 x 5 in image
```

```r
AvgDailyData[which.max(AvgDailyData$averageSteps),]
```

```
## # A tibble: 1 × 2
##   interval averageSteps
##      <int>        <dbl>
## 1      835     206.1698
```

## Imputing missing values

```r
#total number of missing values..
nrow(alldata[!complete.cases(alldata),])
```

```
## [1] 2304
```

```r
#strategy - fill the interval mean if data is missing for an interval
MissingData =  alldata %>% group_by(interval) %>%
        mutate(steps = ifelse(is.na(steps), mean(steps, na.rm = TRUE), steps))

#this dataset will be used later
CompleteSet = MissingData 

MissingData =  MissingData %>% 
        ungroup() %>%
        group_by(date) %>%
        summarise(total = sum(steps))
```


```r
ggplot(MissingData, aes(x=date, y=total)) + 
        geom_bar(stat="identity", color="black", fill="wheat") + 
        labs(x="Date", y="Total Steps", title="Total Steps per Day with Imputed Data")
```

![](PA1_template_files/figure-html/ImputeData-1.png)<!-- -->

```r
ggsave("figures/TotalStepsPerDayWithImputedData.png")
```

```
## Saving 7 x 5 in image
```

```r
summary(MissingData)
```

```
##          date        total      
##  2012-10-01: 1   Min.   :   41  
##  2012-10-02: 1   1st Qu.: 9819  
##  2012-10-03: 1   Median :10766  
##  2012-10-04: 1   Mean   :10766  
##  2012-10-05: 1   3rd Qu.:12811  
##  2012-10-06: 1   Max.   :21194  
##  (Other)   :55
```

```r
mean(MissingData$total, na.rm = TRUE) #Mean Steps Taken.
```

```
## [1] 10766.19
```

```r
median(MissingData$total, na.rm = TRUE) #Median Steps Taken.
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

```r
WeekData =  CompleteSet %>% 
        ungroup() %>%
        mutate(day=ifelse(weekdays(as.Date(date)) %like% ("S"),
                          "Weekend"," Weekday")) %>%
        group_by(day, interval)  %>%
        summarise(averageSteps = mean(steps))

ggplot(WeekData, aes(x=interval, y=averageSteps, fill=day)) + 
        geom_line() + 
        facet_grid(day ~ .) + 
        labs(x="Time Interval", y="Average Steps", title="Average Steps per Time Interval of 5 mins")
```

![](PA1_template_files/figure-html/Weekdata-1.png)<!-- -->

```r
ggsave("figures/TotalStepsByIntervalandWeekdays.png")
```

```
## Saving 7 x 5 in image
```

#https://github.com/sefakilic/coursera-repdata/blob/master/project1/PA1_template.Rmd
