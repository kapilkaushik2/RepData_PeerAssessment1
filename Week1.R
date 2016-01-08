#1. Code for reading in the dataset and/or processing the data

# load the data
activity <- read.csv("D:\\Study\\DataScience\\Coursera_Assignments\\Course5\\Week1_Assignment\\RepData_PeerAssessment1\\activity.csv")

#2. Histogram of the total number of steps taken each day.

# Process/transform the data (if necessary) into a format suitable for our analysis.
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")

# the total number of steps taken per day is stored in the variable called "total_step"
total_step <- aggregate(steps ~ date, data = activity, sum, na.rm = TRUE)
par(mfrow = c(1, 1))

# use base plotting system and more bins than the default setting
hist(total_step$steps, breaks = 20, 
     main = "Total Number of Steps Taken Each Day",
     col = "grey", border = "white", xlab = "Step", axes = FALSE)
axis(1)
axis(2, las = 1)

#3. Mean and median number of steps taken each day

mean(total_step$steps)
median(total_step$steps)

#4. Time series plot of the average number of steps taken

avg_step <- aggregate(steps ~ interval, data = activity, mean, na.rm = TRUE)
plot(avg_step$interval, avg_step$steps, type = "l", lwd = 2, col = "navy",
     main = "Time Series: Average Number of Steps Taken", axes = FALSE,
     xlab = "5-minute interval", ylab = "Average number of steps")
axis(1)
axis(2, las = 1)

#5. The 5-minute interval that, on average, contains the maximum number of steps

avg_step$interval[which.max(avg_step$steps)]

#6. Code to describe and show a strategy for imputing missing data

sum(is.na(activity))


#7. Histogram of the total number of steps taken each day after missing values are imputed

imp <- activity # new dataset called imp
for (i in avg_step$interval) {
  imp[imp$interval == i & is.na(imp$steps), ]$steps <- 
    avg_step$steps[avg_step$interval == i]
}

total_step_imp <- aggregate(steps ~ date, data = imp, sum, na.rm = TRUE)
hist(total_step_imp$steps, breaks = 20, 
     main = "Total Number of Steps Taken Each Day (Imputed)",
     col = "grey", border = "white", xlab = "Step", axes = FALSE)
axis(1)
axis(2, las = 1)

mean(total_step_imp$steps)
median(total_step_imp$steps)

#8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

# Created a new factor variable in the dataset with two levels weekday and weekend
# indicating whether a given date is a weekday or weekend day.

imp$day <- weekdays(imp$date)
imp$week <- ""
imp[imp$day == "Saturday" | imp$day == "Sunday", ]$week <- "weekend"
imp[!(imp$day == "Saturday" | imp$day == "Sunday"), ]$week <- "weekday"
imp$week <- factor(imp$week)


avg_step_imp <- aggregate(steps ~ interval + week, data = imp, mean)
library(lattice)
xyplot(steps ~ interval | week, data = avg_step_imp, type = "l", lwd = 2,
       layout = c(1, 2), 
       xlab = "5-minute interval", 
       ylab = "Average number of steps",
       main = "Average Number of Steps Taken (across all weekday days or weekend days)")