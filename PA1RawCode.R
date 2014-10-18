# 1. Load and preprocess data

unzip("activity.zip")
dat = read.csv("./repdata-data-activity/activity.csv", 
	colClasses = c("integer", "Date", "integer"))

# 2.What is mean total number of steps taken per day?

# Make a histogram of the total number of steps taken each day.

datComplete = na.omit(dat)
library(ggplot2)
ggplot(datComplete, aes(date, steps)) +
	geom_bar(stat = "identity", fill = "purple3", width = 0.8) +
	labs(title = "Total number of steps taken each day")

#Calculate and report the mean and median total number of steps 
#taken per day

stepsTotal = tapply(datComplete$steps, datComplete$date, sum)
mean(stepsTotal)
median(stepsTotal)

# 3. What is the average daily activity pattern?

stepsAverage = aggregate(datComplete$steps, 
	list(interval = datComplete$interval), mean)
ggplot(stepsAverage, aes(interval, x)) + geom_line(color = "purple3") +
	labs(title = "Daily activity by steps taken (on average)", 
	x = "Seconds", y = "Steps")

#Which 5-minute interval, on average across all the days in the dataset, 
#contains the maximum number of steps?

stepsAverage[which.max(stepsAverage$x),]

# 4. Imputing missing values

sum(is.na(dat))
datFilled = dat
for (i in 1:nrow(datFilled)) {
	if (is.na(datFilled$steps[i])) {
	   datFilled$steps[i] = 
		stepsAverage$x[which(datFilled$interval[i] == datFilled$interval)]
		}
	}

ggplot(datFilled, aes(date, steps)) +
	geom_bar(stat = "identity", fill = "purple3", width = 0.8) +
	labs(title = "Total number of steps taken each day")

stepsTotalFilled = tapply(datFilled$steps, datFilled$date, sum)
mean(stepsTotalFilled)
median(stepsTotalFilled)

# 5. Are there differences in activity patterns between weekdays and weekends?

datFilled$dateWeekday = as.factor(weekdays(datFilled$date))
levels(datFilled$dateWeekday) = list(weekday = c("Friday", "Monday", 
	"Thursday", "Tuesday", "Wednesday"), 
	weekend = c("Saturday", "Sunday"))

stepsFilled = aggregate(datFilled$steps, 
	list(interval = datFilled$interval, dateWeekday = datFilled$dateWeekday),
	mean)
ggplot(stepsFilled, aes(interval, x)) + geom_line(color = "purple3") +
	facet_grid(dateWeekday ~ .) +
	labs(title = "Daily activity on weekdays and weekends by steps taken (on average)", 
	x = "Seconds", y = "Steps")


