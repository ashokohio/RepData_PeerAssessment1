# Read the data into the variable stepsData
stepsData <- read.table("activity.csv", header = TRUE, sep = ",", na.strings = "NA")

# Sum the steps data for a given date over all intervals
stepsInDay <- tapply(stepsData$steps, stepsData$date, sum)

# Plot histogram of stepsInDay
hist(stepsInDay, main = "Histogram of steps in a day", xlab = "Steps in day", ylab = "Count")

# Compute the mean and median of stepsInDay
meanStepsInDay <- mean(stepsInDay, na.rm = TRUE)
medianStepsInDay <- median(stepsInDay, na.rm = TRUE)
sprintf("Mean number of steps in a day = %f\n", meanStepsInDay)
sprintf("Median number of steps in a day = %f\n", medianStepsInDay)

# Compute the average number of steps across all days in each 5 minute interval
meanStepsInIntervals <- tapply(stepsData$steps, as.factor(stepsData$interval), mean, na.rm = TRUE)

# Plot the average number of steps across all days in each 5 minute interval
intervals <- unique(stepsData$interval)
plot(intervals, meanStepsInIntervals, type = "l",
     xlab = "Time interval", ylab = "Average steps",
     main = "Average steps in 5 minute intervals")

# Get the interval index of the maximum of meanStepsInIntervals
intervalWithMaxIndex <- which.max(meanStepsInIntervals)
IntervalWithMax <- intervals[intervalWithMaxIndex]
sprintf("The interval with the maxiumun number of steps across all days is %f", IntervalWithMax)

# Get the number of rows with NAs
rowsWithNA <- is.na(stepsData$steps)
numberNA <- sum(rowsWithNA)
sprintf("Number of 5 minute intervals with missing data is %f", numberNA)

# Replace the NA step values by the average steps in that 5 minute time interval
# The method below seems to work, but may not be the best way.
# A new data frame, imputedStepsData is used to hold the steps data with the imputed values
imputedStepsData <- stepsData
imputedStepsData$steps[rowsWithNA] <- 
  meanStepsInIntervals[as.character(stepsData$interval[rowsWithNA])]

# Sum the imputed steps data for a given date over all intervals
imputedStepsInDay <- tapply(imputedStepsData$steps, imputedStepsData$date, sum)
# Plot histogram of stepsInDay
hist(imputedStepsInDay, main = "Histogram of imputed steps in a day",
     xlab = "Imputed steps in day", ylab = "Count")

# Compute the mean and median of stepsInDay
meanImputedStepsInDay <- mean(imputedStepsInDay, na.rm = TRUE)
medianImputedStepsInDay <- median(imputedStepsInDay, na.rm = TRUE)

# Add a new column called daytype to imputedStepsData that is a factor
# variable with values weekday or weekend depending on whether the corresponding
# date is a weekday or weekend
weekend <- c("Saturday", "Sunday")
imputedStepsData$daytype <- as.factor(ifelse(weekdays(as.POSIXlt(imputedStepsData$date)) %in% weekend, "weekend", "weekday"))

# Calculate the weekday and weekend average stes in each 5 minute interval
xx <- as.data.frame(tapply(imputedStepsData$steps, list(as.factor(imputedStepsData$interval),
                                          imputedStepsData$daytype), mean))

par(mfrow = c(1,2))
plot(intervals, xx$weekday, type = "l",
     xlab = "Time interval", ylab = "Average steps",
     main = "Average steps in 5 minute intervals for weekdays")
plot(intervals, xx$weekend, type = "l",
     xlab = "Time interval", ylab = "Average steps",
     main = "Average steps in 5 minute intervals for weekends")
    