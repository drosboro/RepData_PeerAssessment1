# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

The activity data set is stored in the repository as activity.zip.  First, we'll unzip this file to get at the data file, if it hasn't already been unzipped.


```r
if (!file.exists("activity.csv")) {
  unzip("activity.zip")
}
```

We will read the csv file into memory, making sure that the date column is stored as the Date class.


```r
data <- read.csv("activity.csv", header=TRUE, colClasses=c("numeric", "Date", "numeric"))
```

## What is mean total number of steps taken per day?

We will use the `dplyr` package to group the data by day, and generate a summary containing the total number of steps taken for each day.


```r
library(dplyr)

daily <- group_by(data, date)
daily_totals <- summarise(daily, total = sum(steps, na.rm=TRUE))
```

We next generate a histogram of the total steps per day over the 61 days.


```r
hist(daily_totals$total, breaks=10, main="Histogram of total steps per day", xlab="Total steps per day")
```

![](analysis_files/figure-html/unnamed-chunk-4-1.png) 

The mean and median number of steps are calculated below:


```r
mean(daily_totals$total)
```

```
## [1] 9354.23
```

```r
median(daily_totals$total)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

Again, we will use `dplyr` to group the data by interval (time during the day).  We will then generate a summary containing the mean number of steps taken during each interval across the 61 days.


```r
interval_data <- group_by(data, interval)
interval_summary <- summarise(interval_data, meansteps = mean(steps, na.rm=TRUE))
plot(interval_summary$interval, interval_summary$meansteps, type="l")
```

![](analysis_files/figure-html/unnamed-chunk-6-1.png) 

Next, we will find the time interval with the maximum mean number of steps:


```r
max <- interval_summary[interval_summary$meansteps == max(interval_summary$meansteps), ]
```

The maximum mean number of steps (206.1698113) occurs at the time interval 835.

## Imputing missing values

Many rows of the data have missing values.  The number of rows is computed below.


```r
nrow(data[!complete.cases(data),])
```

```
## [1] 2304
```

In order to impute these missing values, we will fill them with the mean number of steps for that interval, across the 61 days in the data set.


```r
complete_rows <- data[complete.cases(data), ]
incomplete_rows <- data[!complete.cases(data), ]

for (i in 1:nrow(incomplete_rows)) {
  incomplete_rows[i, "steps"] <- interval_summary[interval_summary$interval == incomplete_rows[i, "interval"], "meansteps"]
}

imputed_data <- rbind(complete_rows, incomplete_rows)
```

As before, we will group the data by day, and calculate the total number of steps per day.  We will generate a histogram, and calculate the mean and median number of steps per day.


```r
daily <- group_by(imputed_data, date)
daily_totals <- summarise(daily, total = sum(steps, na.rm=TRUE))

hist(daily_totals$total, breaks=10, main="Histogram of total steps per day", xlab="Total steps per day")
```

![](analysis_files/figure-html/unnamed-chunk-10-1.png) 

```r
mean(daily_totals$total)
```

```
## [1] 10766.19
```

```r
median(daily_totals$total)
```

```
## [1] 10766.19
```

As you can see, the histogram is primarily different in the number of days with very few steps (e.g. 0 steps, when there was no data reported for that day).  This has also increased the mean and median of the numbers of steps per day, as you might expect.

## Are there differences in activity patterns between weekdays and weekends?

First, we will create a new column that describes each date as either a weekday (Monday - Friday) or a weekend (Saturday - Sunday).


```r
imputed_data$weekday <- weekdays(imputed_data$date)
imputed_data$daytype <- ifelse(imputed_data$weekday == "Saturday" | imputed_data$weekday == "Sunday", "weekend", "weekday")
```

Then, we will calculate the mean number of steps in each interval - making a seperate mean for weekends and for weekdays.  These mean step counts will be then be plotted for comparison.


```r
library(lattice)

interval_data <- group_by(imputed_data, daytype, interval)
interval_summary <- summarise(interval_data, meansteps=mean(steps))
xyplot(interval_summary$meansteps ~ interval_summary$interval | interval_summary$daytype, type="l", layout = c(1, 2), xlab="Interval", ylab="Number of Steps")
```

![](analysis_files/figure-html/unnamed-chunk-12-1.png) 

As you can see, while the weekday has most activity concentrated earlier in the morning, the activity is more spread out on weekends.  It would also seem that on weekends, fewer steps are taken first thing in the morning, perhaps indicating a later time for getting out of bed.
