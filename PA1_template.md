# Reproducible Research: Peer Assessment 1
This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day. There are several questions to be answered as a part of this assignment, and both they and the steps necessary to analyze the data are documented below.

## Loading and preprocessing the data
The first step in the process is to load the data into a data frame for later analysis. The data file (activity.csv) is assumed to have been placed in the directory that this program is run from.

```r
#setwd("~/Documents/datasciencecoursera/Reproducible research/Project 1/RepData_PeerAssessment1")
library(plyr)
act <- read.csv("activity.csv", colClasses = c("integer", "Date", "integer"))
```

## What is mean total number of steps taken per day?
The first step to do this is to calculate the total number of steps per day, ignoring and missed (NA) values in the data set. The *plyr* package is used to summarize the number of steps, using an embedded function to perform the summary on each returned data frame (each date will be returned as a separate data frame to the function).

```r
tot.steps <- ddply(act, "date", function(df)(sum(df$steps)))
```
### First, create a histogram of the total number of steps per day
The following histogram displays the distribution of total steps taken per day over the data collection period. Note that in this figure, any missing data is ignored.

```r
hist(tot.steps$V1, breaks = 10, main = "Histogram of total steps per day", xlab = "Total steps per day")
```

![plot of chunk unnamed-chunk-3](./PA1_template_files/figure-html/unnamed-chunk-3.png) 

### Next, calculate and report the mean and median steps taken per day
Since the total steps per day have already been calculated in the previous step, all that is required is to process the output generated from the previously run *plyr* package. As before, any missing data is ignored.

```r
median.steps  <- median(tot.steps$V1, na.rm = TRUE)
mean.steps  <- mean(tot.steps$V1, na.rm = TRUE)
```
This results in a mean steps per day count of 1.0766 &times; 10<sup>4</sup> with a median count of 10765.


## What is the average daily activity pattern?
First, calculate the mean for each of the 288 individual 5-minute intervals that occur during the day for all days. That is, each day has a total of 288 5-minute intervals, so calculate the mean for each interval over the course of all observations. Once this is done, plot a time series of the indiviudal intervals. This time any missing values will be removed and not included in the calculations.

```r
act.good <- act[complete.cases(act), ]
int.steps <- ddply(act.good, "interval", function(df)(mean(df$steps)))
```
Once the indivudal intervals have been averages across all days in the study, they can be plotted as a time series showing the average activity (number of steps) per interval:

```r
plot(int.steps$interval, int.steps$V1, type = "l", main = "Mean activity per interval", ylab = "Steps", xlab = "Interval")
```

![plot of chunk unnamed-chunk-6](./PA1_template_files/figure-html/unnamed-chunk-6.png) 

```r
max.activity <- max(int.steps$V1)
max.interval <- int.steps$interval[int.steps$V1 == max.activity]
```
Looking at the above plot, there is a clear spike of 206.1698 mean steps per day at interval 835.

## Imputing missing values

```r
missing  <- nrow(act) - nrow(act.good)
```
Since there are 2304 missing rows of data (about 13% of the total), the question arises as to the effect of the missing data. To this end, the missing data was fabricated by linear interpolation between the last valid data point and the next valid data point, repeating this for each series of missing data.



## Are there differences in activity patterns between weekdays and weekends?
