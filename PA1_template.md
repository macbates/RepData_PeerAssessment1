# Reproducible Research: Peer Assessment 1
This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals throughout the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day. There are several questions to be answered as a part of this assignment, and both they and the steps necessary to analyze the data are documented below.

## Loading and preprocessing the data
The first step in the process is to load the data into a data frame for later analysis. The data file (activity.csv) is assumed to have been placed in the directory that this program is run from.

```r
#setwd("~/Documents/datasciencecoursera/Reproducible research/Project 1/RepData_PeerAssessment1")
library(plyr)
act <- read.csv("activity.csv", colClasses = c("integer", "Date", "integer"))
```

## What is mean total number of steps taken per day?
Once the data has been loaded, the total number of steps per day will be calculated, ignoring any missed (NA) values in the data set. The *plyr* package is used to summarize the number of steps, using an embedded function to perform the summary on each returned data frame (each date will be returned as a separate data frame to the function).

```r
tot.steps <- ddply(act, "date", function(df)(sum(na.omit(df$steps))))
```

### First, create a histogram of the total number of steps per day
The following histogram displays the distribution of total steps taken per day over the data collection period. Note that in this figure, any missing data is ignored since it has been discarded.

```r
hist(tot.steps$V1, breaks = 20, main = "Histogram of total steps per day", sub = "(Missing values not included)", 
     xlab = "Total steps per day")
```

![plot of chunk totalHistogram](./PA1_template_files/figure-html/totalHistogram.png) 

### Next, calculate and report the mean and median steps taken per day
Since the total steps per day have already been calculated in the previous step, all that is required is to process the output generated from the previously run *plyr* package. As before, any missing data is ignored since it was removed.

```r
median.steps  <- median(tot.steps$V1)
mean.steps  <- mean(tot.steps$V1)
```
This results in a mean steps per day count of 9354.2295 with a median count of 10395.

## What is the average daily activity pattern?
To obtain the average, the mean is calculated for each of the 288 individual 5-minute intervals that occur during the day for all days. That is, each day has a total of 288 5-minute intervals, so the mean is calculated for each interval over the course of all observations. Once this is done, a time series of the indiviudal intervals is plotted. Prior to these steps, any missing values will be removed and not included in the calculations.

```r
act.good <- act[complete.cases(act), ]
int.steps <- ddply(act.good, "interval", function(df)(mean(df$steps)))
```

Once the indivudal intervals have been averaged across all days in the study, they will be plotted as a time series showing the average activity (number of steps) per interval:

```r
plot(int.steps$interval, int.steps$V1, type = "l", main = "Mean activity per interval", ylab = "Steps", xlab = "Interval")
```

![plot of chunk plotActivity](./PA1_template_files/figure-html/plotActivity.png) 

```r
max.activity <- max(int.steps$V1)
max.interval <- int.steps$interval[int.steps$V1 == max.activity]
```
Looking at the above plot, there is a very evident spike of 206.1698 mean steps per day at interval 835.

## Imputing missing values

```r
missing  <- nrow(act) - nrow(act.good)
```

Since there are 2304 missing rows of data (about 13% of the total), the question arises as to the effect of the missing data. To this end, the missing data was fabricated by the suggested (and admittedly simplistic) method of using the mean of all days for that particular interval and replacing the NA value in the original data set with that mean.

```r
new.act <- act
for (i in 1:nrow(new.act)) {
    if (is.na(new.act$steps[i])) { # Missing value?
        index <- match(new.act$interval[i], int.steps$interval) # Get the interval offset
        new.act$steps[i] <- int.steps$V1[index] # Put in the mean steps for that interval
    }
}
```

From these steps, a histogram of the total number of steps taken each day may be created. This histogram, which uses the fabricated data to replace any missing values, may then be compared against the histogram that was drawn earlier to see what effect the missing values may have.

```r
tot.new.steps <- ddply(new.act, "date", function(df)(sum(df$steps)))
hist(tot.new.steps$V1, breaks = 20, main = "Histogram of total steps per day", sub = "(Missing values fabricated)", 
     xlab = "Total steps per day")
```

![plot of chunk newHistogram](./PA1_template_files/figure-html/newHistogram.png) 

As may be seen by comparing the two histograms, there is a significant difference between the two in both the low end (0+ steps) and middle (10,000+ steps). A visual examination of the frequencies in both histograms suggests that the steps per day in the left most bar of the first histogram have been added to the middle bar of the second histogram.


```r
median.new.steps  <- median(tot.new.steps$V1)
mean.new.steps  <- mean(tot.new.steps$V1)
```
The medians of both the original data set with missing values ignored (10395) and the data set with missing values replaced with daily interval means (10766) are relatively close. The means are quite different however, with the original data set havng a mean of 9354.2295 and the "fabricated" data set having a mean of 1.0766 &times; 10<sup>4</sup>. This increase in the mean of the second data set strengthens (but does not prove) the suggestion that the replacement of missing values with daily interval means has reduced the contribution of small numbers of steps, yielding a corresponding increase in the number of typical (median) steps. Clearly an area worthy of not only more analysis, but also a more sophisticated replacement strategy for the smissing values.

## Are there differences in activity patterns between weekdays and weekends?
To evaluate this question, the data set with fabricated replacement values was processed to extract the mean number of steps for both weekdays and weekends using the *plyr* package as previously described. Per the assignment instructions, two factors ("weekday" and "weekend") were also created and added to the data set, although there is an alternative way (comented out below) that some might consider easier to understand.

```r
#new.act$we <- weekdays(new.act$date) %in% c("Saturday", "Sunday")
#we.steps <- ddply(new.act[new.act$we == TRUE, ], "interval", function(df)(mean(df$steps)))
#wd.steps <- ddply(new.act[new.act$we == FALSE, ], "interval", function(df)(mean(df$steps)))
new.act$date.we <- factor(ifelse(weekdays(new.act$date) %in% c("Saturday", "Sunday"), "Weekend", "Weekday"))
we.steps <- ddply(subset(new.act, new.act$date.we == "Weekend"), "interval", function(df)(mean(df$steps)))
wd.steps <- ddply(subset(new.act, new.act$date.we == "Weekday"), "interval", function(df)(mean(df$steps)))
```

After the weekday and weekend totals were accumulated, they were plotted as individual graphs similar to the preceeding activity graph. The vertical scales were set to the same value in both plots to allow ease of comparison.

```r
max.both <- c(0, max(wd.steps$V1, we.steps$V1))
par(mfrow = (c(2, 1)))
plot(wd.steps$interval, wd.steps$V1, type = "l", ylim = max.both, main = "Mean weekday activity per interval", 
     sub = "(Missing values fabricated)", ylab = "Steps", xlab = "Interval")
plot(we.steps$interval, we.steps$V1, type = "l", ylim = max.both, main = "Mean weekend activity per interval", 
     sub = "(Missing values fabricated)", ylab = "Steps", xlab = "Interval")
```

![plot of chunk weekdayPlots](./PA1_template_files/figure-html/weekdayPlots.png) 

Comparison of weekday and weekend activity reveal a few interesting areas:

1. The activity appears to start earlier in the day (lower interval number) on weekdays than on the weekend. This may be due to the individual being measures beginning their day earlier, perhaps by going into work or other activities that require their presence on a weekday.
2. The peak activity on weekends is lower than than the peak on weekdays. This may be due to work or other types of travel that occur during the weekdays but not on weekends.
3. Visual inspection of the twp graphs suggests that the mean and median activities are lower on weekends than during the weekdays. This may be a side effect of the previous item in that more work (or other) related travel is required on weekdays.
4. The activities on a weekend ramp up gradually, while the activites on weekdays begin abruptly, which is consistent with a leisurly morning (weekends) as opposed to rushing to get to work (weekdays).

No additional analyses were performed on the data, although the preceeding data suggests that there remain several unanswered questions from the data set.
