# Reproducible Research: Peer Assessment 1
Mano Yakandawala  
#### Report created on : Thu Oct 13 2016



## Loading and preprocessing the data
Read the dataset without removing NA values.

```r
alldata = read.csv("activity.csv")
```

## What is mean total number of steps taken per day?
Here we group the data by date and the summarise by date to get the count per date.  

```r
ProData =  alldata %>% group_by(date) %>%
        summarise(total = sum(steps))

ggplot(ProData, aes(x=date, y=total)) + 
        geom_bar(stat="identity", color="deepskyblue4", fill="deepskyblue") + 
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
        geom_line(size=.8, color="deepskyblue4") + 
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
Here we do a few things. first we get the missing values through a logic index.  

Then we impliment a strategy to fill the NA values by interval. We go through each record, and fill the interval mean if we have an NA for that observation.

```r
#total number of missing values..
nrow(alldata[!complete.cases(alldata),])
```

```
## [1] 2304
```

```r
#strategy implemented.
ImputedData =  alldata %>% group_by(interval) %>%
        mutate(steps = ifelse(is.na(steps), mean(steps, na.rm = TRUE), steps))

#this dataset will be used later, so we dont have to missing values again.
#why, because we have to summarize the dataset which impacts the strucutre..
CompleteSet = ImputedData 

ImputedData =  ImputedData %>% 
        ungroup() %>%
        group_by(date) %>%
        summarise(total = sum(steps))

ggplot(ImputedData, aes(x=date, y=total)) + 
        geom_bar(stat="identity", color="deepskyblue4", fill="deepskyblue")
```

![](PA1_template_files/figure-html/MissingValues-1.png)<!-- -->

```r
        labs(x="Date", y="Total Steps", title="Total Steps per Day with Imputed Data")
```

```
## $x
## [1] "Date"
## 
## $y
## [1] "Total Steps"
## 
## $title
## [1] "Total Steps per Day with Imputed Data"
## 
## attr(,"class")
## [1] "labels"
```

```r
ggsave("figures/TotalStepsPerDayWithImputedData.png")
```

```
## Saving 7 x 5 in image
```

```r
summary(ImputedData) #Summary with Imputed data
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
mean(ImputedData$total, na.rm = TRUE) #Mean Steps Taken for imputed data.
```

```
## [1] 10766.19
```

```r
median(ImputedData$total, na.rm = TRUE) #Median Steps Taken for imputed data.
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
        geom_line(size=.8, color="deepskyblue4") + 
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
