# Peer Assignment 1 : "Physical Activity Data"
## Reproducible Research
### *By Erika E.*

## *Load "Activity" data*

setwd("C:/Users/Erika/Desktop/Reproducible Research/repdata%2Fdata%2Factivity")
activity <- read.csv("C:/Users/Erika/Desktop/Reproducible Research/repdata%2Fdata%2Factivity/activity.csv", header = T)

# What is mean total number of steps taken per day?

## 1. Calculate total steps per day (account for missing values)

tot_steps_day <- aggregate(steps ~ date, data = activity, FUN = "sum", na.rm = TRUE)

## 2. [...] Make a histogram of the total number of steps taken each day

hist(tot_steps_day$steps, xlab = "Steps", main = "Total Number Of Steps Per Day", col = "green")

## 3. Calculate and report the mean and median of the total number of steps taken per day

print(paste('The mean of the total number of steps per day is', round(mean(tot_steps_day$steps))))
      
print(paste('The median of the total number of steps per day is', round(median(tot_steps_day$steps))))
  
# What is the average daily activity pattern?
  
## 4. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

avg_steps_day <- aggregate(steps ~ interval, data = activity, FUN = mean)

interval <- 'column 3'

avg_steps_interval <- aggregate(steps ~ interval, data = activity, FUN = mean)

plot(x = avg_steps_interval$interval, y = avg_steps_day$steps, type = "l",  xlab = "Interval", ylab = "Steps", main = "Intervals V.S Avg number of steps taken")

## 5. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
  
print(paste('The 5-minute interval with maximum number of steps in entire dataset is', (max(avg_steps_interval$interval))))

# Imputing missing values
  ### Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.*
  
## 6. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
print(paste('The number of missing values in the dataset is', sum(is.na(activity$steps))))
    
## 7. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
## 8. Create a new dataset that is equal to the original dataset but with the missing data filled in.

newActivity <- activity

for (i in 1:nrow(newActivity)) {
  if (is.na(newActivity$steps[i])) { 
    NAreplacement <- avg_steps_day$steps[which(avg_steps_day$interval == newActivity$interval[i])]
    newActivity$steps[i] <- NAreplacement 

  }  
}

head(newActivity)
tail(newActivity)

## 9. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

tot_steps_dayNew <- aggregate(steps ~ date, data = newActivity, FUN = "sum", na.rm = TRUE)

hist(tot_steps_dayNew$steps, xlab = "Steps", main = "Total Number Of Steps Per Day (No Missing Values)", col = "orange")

# Are there differences in activity patterns between weekdays and weekends? For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

print(paste('The mean of the total number of steps (no NAs) per day is', round(mean(tot_steps_dayNew$steps))))

print(paste('The median of the total number of steps (no NAs) per day is', round(median(tot_steps_dayNew$steps))))

## 10. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

newActivity$date <- strptime(x = newActivity$date, format = '%Y-%m-%d')

newActivity$weekdays <- weekdays(newActivity$date, abb=T)

newActivity$day[(newActivity$weekdays == "Sat" | newActivity$weekdays == "Sun")] <- "weekend"
newActivity$day[!(newActivity$weekdays == "Sat" | newActivity$weekdays == "Sun")] <- "weekday"

print(table(newActivity$day))

## 11. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
avg_steps_dayNew <- aggregate(steps ~ interval, data = newActivity, FUN = "mean")

interval <- 'column 3'

avg_steps_intervalNew <- aggregate(steps ~ interval + day, data = newActivity, FUN = "mean")

library(lattice)
```{r}plot(xyplot(steps ~ interval | day, data=avg_steps_intervalNew, type = "l", grid = T, layout=c(1,2), xlab = "Interval", ylab = "Steps", main = "Weekdays VS Weekends"))
```
