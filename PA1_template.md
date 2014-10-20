Peer Assessment 1 for Reproducible Research
===================================================
# Objective
The objective in this assignmanet is to analyse data collected from an activity minitoring device that measures steps walked in 5 minute intervals. A course objective is to use R markdown to present the analysis.

## Loading and preprocessing the data
The next chunk of R code reads the data from the activity.csv file


```r
# Read the data into the variable stepsData
stepsData <- read.table("activity.csv", header = TRUE, sep = ",", na.strings = "NA")
```

## What is mean total number of steps taken per day?

Next we compute and plot a histogram of the steps in a day. The tapply function is used to add all the rows of data that correspond to the same date.


```r
# Sum the steps data for a given date over all intervals
stepsInDay <- tapply(stepsData$steps, stepsData$date, sum)

# Plot histogram of stepsInDay
hist(stepsInDay, main = "Histogram of steps in a day", xlab = "Steps in day", ylab = "Count")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

Then we compute and display the mean and median of the number of steps in a day


```r
# Compute the mean and median of stepsInDay
meanStepsInDay <- mean(stepsInDay, na.rm = TRUE)
medianStepsInDay <- median(stepsInDay, na.rm = TRUE)
sprintf("Mean number of steps in a day = %f\n", meanStepsInDay)
```

```
## [1] "Mean number of steps in a day = 10766.188679\n"
```

```r
sprintf("Median number of steps in a day = %f\n", medianStepsInDay)
```

```
## [1] "Median number of steps in a day = 10765.000000\n"
```

## What is the average daily activity pattern?

In this section, the anlaysis is done in the 5 minute intervals of data every day.
First we compute and plot the avearge number of steps in each 5 minute interval across all days


```r
# Compute the average number of steps across all days in each 5 minute interval
meanStepsInIntervals <- tapply(stepsData$steps, as.factor(stepsData$interval), mean, na.rm = TRUE)

# Plot the average number of steps across all days in each 5 minute interval
intervals <- unique(stepsData$interval)
plot(intervals, meanStepsInIntervals, type = "l",
     xlab = "Time interval", ylab = "Average steps",
     main = "Average steps in 5 minute intervals")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

Next we find the 5 minute interval with the maxiumum steps avearegd across all days


```r
# Get the interval index of the maximum of meanStepsInIntervals
intervalWithMaxIndex <- which.max(meanStepsInIntervals)
IntervalWithMax <- intervals[intervalWithMaxIndex]
sprintf("The interval with the maxiumun number of steps across all days is %f", IntervalWithMax)
```

```
## [1] "The interval with the maxiumun number of steps across all days is 835.000000"
```

## Imputing missing values

In this section, the missing data is replaced by statistics of the available data. I have chosen to repalce the missing data in any 5 minute interval by the average of the data across the same 5 minute interval averaged across all days. Obviously, the average does not include the NA data.

First we find the number of rows with missing data


```r
# Get the number of rows with NAs
rowsWithNA <- is.na(stepsData$steps)
numberNA <- sum(rowsWithNA)
sprintf("Number of 5 minute intervals with missing data is %f", numberNA)
```

```
## [1] "Number of 5 minute intervals with missing data is 2304.000000"
```

Next we use the average for a given 5 minute interval across all dates to replace the missing values.


```r
# Replace the NA step values by the average steps in that 5 minute time interval
# The method below seems to work, but may not be the best way.
# A new data frame, imputedStepsData is used to hold the steps data with the imputed values
imputedStepsData <- stepsData
imputedStepsData$steps[rowsWithNA] <- 
  meanStepsInIntervals[as.character(stepsData$interval[rowsWithNA])]
```

Now we repeat the histogram and mean and median caclulations as before, but now with the data set including the imputed data


```r
# Sum the imputed steps data for a given date over all intervals
imputedStepsInDay <- tapply(imputedStepsData$steps, imputedStepsData$date, sum)
# Plot histogram of stepsInDay
hist(imputedStepsInDay, main = "Histogram of imputed steps in a day",
     xlab = "Imputed steps in day", ylab = "Count")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

```r
# Compute the mean and median of stepsInDay
meanImputedStepsInDay <- mean(imputedStepsInDay, na.rm = TRUE)
medianImputedStepsInDay <- median(imputedStepsInDay, na.rm = TRUE)
sprintf("Mean number of steps in a day using imputed data = %f\n", meanImputedStepsInDay)
```

```
## [1] "Mean number of steps in a day using imputed data = 10766.188679\n"
```

```r
sprintf("Median number of steps in a day using imputed data = %f\n", medianImputedStepsInDay)
```

```
## [1] "Median number of steps in a day using imputed data = 10766.188679\n"
```

## Are there differences in activity patterns between weekdays and weekends?

First we create a factor variable that distinguishes between weekdays and weekends and add to the data frame.


```r
# Add a new column called daytype to imputedStepsData that is a factor
# variable with values weekday or weekend depending on whether the corresponding
# date is a weekday or weekend
weekend <- c("Saturday", "Sunday")
imputedStepsData$daytype <- as.factor(ifelse(weekdays(as.POSIXlt(imputedStepsData$date)) %in% weekend, "weekend", "weekday"))

# Calculate the weekday and weekend average stes in each 5 minute interval
xx <- as.data.frame(tapply(imputedStepsData$steps, list(as.factor(imputedStepsData$interval),
                                          imputedStepsData$daytype), mean))
```

Now we plot average number of steps in 5 minutes intervals for weekdays and weekends separately.


```r
par(mfrow = c(1,2))
plot(intervals, xx$weekday, type = "l",
     xlab = "Time interval", ylab = "Average steps",
     main = "Average steps in 5 minute intervals for weekdays")
plot(intervals, xx$weekend, type = "l",
     xlab = "Time interval", ylab = "Average steps",
     main = "Average steps in 5 minute intervals for weekends")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 
