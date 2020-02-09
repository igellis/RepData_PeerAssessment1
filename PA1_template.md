---
title: "Reproducable research PA1"
author: "Isadora"
date: "9-2-2020"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
library(readr)
df <- read_csv("activity.csv")
```

```
## Parsed with column specification:
## cols(
##   steps = col_double(),
##   date = col_date(format = ""),
##   interval = col_double()
## )
```


## What is mean total number of steps taken per day?

```r
step_all <- aggregate(steps ~ date, df, FUN=sum)
print("Histogram - total number of steps taken each day:")
```

```
## [1] "Histogram - total number of steps taken each day:"
```

```r
hist(step_all$steps)
```

![](PA1_template_files/figure-html/mean-1.png)<!-- -->

```r
print("mean steps:")
```

```
## [1] "mean steps:"
```

```r
mean(step_all$steps, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
print("median steps:")
```

```
## [1] "median steps:"
```

```r
median(step_all$steps, na.rm = TRUE)
```

```
## [1] 10765
```


## What is the average daily activity pattern?

```r
library(ggplot2)
m_step_int <- aggregate(steps ~ interval, df, mean)

print("Time series plot of the average number of steps taken:")
```

```
## [1] "Time series plot of the average number of steps taken:"
```

```r
ggplot(data = m_step_int, aes(x = interval, y = steps)) +
  geom_line()
```

![](PA1_template_files/figure-html/timeseries mean-1.png)<!-- -->

```r
print("The 5-minute interval that, on average, contains the maximum number of steps:")
```

```
## [1] "The 5-minute interval that, on average, contains the maximum number of steps:"
```

```r
m_step_int[which.max(m_step_int$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```


## Imputing missing values

```r
print("Code to describe and show a strategy for imputing missing data:")
```

```
## [1] "Code to describe and show a strategy for imputing missing data:"
```

```r
sum(is.na(df$steps))
```

```
## [1] 2304
```

```r
df_imputed<- transform(df, steps = ifelse(is.na(df$steps),m_step_int$steps[match(df$interval, m_step_int$interval)],df$steps))
```


## Are there differences in activity patterns between weekdays and weekends?

```r
print("Histogram of the total number of steps taken each day after missing values are imputed:")
```

```
## [1] "Histogram of the total number of steps taken each day after missing values are imputed:"
```

```r
step_all_imp <- aggregate(steps ~ date, df_imputed, FUN=sum)
hist(step_all_imp$steps)
```

![](PA1_template_files/figure-html/compare differences-1.png)<!-- -->

```r
print("Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends:")
```

```
## [1] "Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends:"
```

```r
df_imputed$weekday <- ifelse(  weekdays(df_imputed$date) %in% c('maandag', 'dinsdag', 'woensdag', 'donderdag', 'vrijdag'), 'weekday', 'weekend')

m_steps_day<- aggregate(steps ~ interval + weekday, df_imputed, mean)
ggplot(data = m_steps_day, aes(x = interval, y = steps)) + 
  geom_line() +
  facet_grid(weekday ~ .) 
```

![](PA1_template_files/figure-html/compare differences-2.png)<!-- -->
