# Read dataset
activity.data <- read.csv("Raw data/activity.csv")

# Q: What is the mean total number of steps taken per day?

## Calculate total number of steps taken per day
steps.per.day <- aggregate(steps ~ date, data = activity.data, sum)

## Make a histogram
library(ggplot2)

hist1 <- ggplot(data = steps.per.day, aes(steps)) + 
  geom_histogram(bins = 10, col = "white") +
  ggtitle("Histogram of total number of steps per day") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())
print(hist1)

## Mean and median number of steps per day

mean.steps.per.day <- round(mean(steps.per.day$steps))
median.steps.per.day <- median(steps.per.day$steps)

print(paste("Mean steps per day:", mean.steps.per.day))
print(paste("Median steps per day:", median.steps.per.day))

# Average daily activity pattern

## Time series plot of the 5 minute interval and the average number of steps taken, averaged across all days
mean.steps.per.interval <- aggregate(steps ~ interval, data = activity.data, mean)

ggplot(data = mean.steps.per.interval, aes(interval, steps)) + 
  geom_line() +
  ggtitle("Average number of steps taken per interval across all days")

## Which interval, on average across all days in the dataset, contains the maximum number of steps?
mean.steps.per.interval$interval[which.max(mean.steps.per.interval$steps)]

# Imputing missing values

## find out number of missing values in the dataset
sum(is.na(activity.data$steps))

## Replace all missing values with median value for that interval across all days

median.steps.per.interval <- aggregate(steps ~ interval, data = activity.data, median)

activity.data.imputed <- activity.data
na.indices <- which(is.na(activity.data.imputed$steps))

activity.data.imputed$steps[na.indices] <- 
  median.steps.per.interval$steps[match(activity.data.imputed$interval[na.indices], 
                                        median.steps.per.interval$interval)]

## Make a histogram

steps.per.day.imputed <- aggregate(steps ~ date, data = activity.data.imputed, sum)

hist2 <- ggplot(data = steps.per.day.imputed, aes(steps)) + 
  geom_histogram(bins = 10, col = "white") +
  ggtitle("Histogram of total number of steps per day after imputing missing values") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())
print(hist2)

library(gridExtra)
grid.arrange(hist1, hist2, ncol=2)

## Mean and median number of steps per day

mean.steps.per.day.imputed <- round(mean(steps.per.day.imputed$steps))
median.steps.per.day.imputed <- median(steps.per.day.imputed$steps)

print(paste("Mean steps per day:", mean.steps.per.day.imputed))
print(paste("Median steps per day:", median.steps.per.day.imputed))

# Activity patterns

activity.data.imputed$daytype <- factor(
  ifelse(weekdays(
    as.Date(as.character(activity.data.imputed$date))
    ) %in% c("zaterdag", "zondag"), 
    "weekend", 
    "weekday"))

mean.steps.per.interval.and.daytype <- aggregate(steps ~ interval + daytype, data = activity.data.imputed, mean)

ggplot(data = mean.steps.per.interval.and.daytype, aes(interval, steps)) + 
  geom_line() +
  facet_wrap(~ daytype, ncol = 1) +
  ggtitle("Average number of steps taken per interval across all days, for weekdays and weekends")

