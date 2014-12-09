### Loading and preprocessing the data

#   Load the data
setwd("C://Users//patrick.sanzo/Downloads/Reproducible research//Week 2/RepData_PeerAssessment1/")
activityData <- read.csv("activity.csv")

# Process/transform the data (if necessary) 
# into a format suitable for your analysis
filteredData <- activityData[!is.na(activityData$steps),]




### What is mean total number of steps taken per day?

# For this part of the assignment, you can ignore the missing values in the dataset.

# Make a histogram of the total number of steps taken each day
totalStepsByDay <- tapply(activityData$steps, activityData$date, sum, simplify = T, na.rm=T)
hist(totalStepsByDay)

# Calculate and report the mean and median total number of steps taken per day
meanStepsByDay <- tapply(activityData$steps, activityData$date, mean, simplify = T)
medianStepsByDay <- tapply(activityData$steps, activityData$date, median, simplify = T,na.rm=T)


## What is the average daily activity pattern?

# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
y <- tapply(activityData$steps, activityData$interval, mean, simplify = T, na.rm=T)
x <- unique(activityData$interval) # automatically ordered
plot(x, y, type = "l", xlab ="Interval", ylab="Average number of steps",)

# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
optimum <- order(y,decreasing=T)[1]
abline(v=unique(activityData$interval)[optimum], col="red")
legendText = paste("Maximum value: ", as.character(unique(activityData$interval)[optimum]) )
legend( legend = legendText ,max(y)*.8,x=max(x)*0.4 )



## Imputing missing values

# Note that there are a number of days/intervals where there are missing values (coded as NA). 
# The presence of missing days may introduce bias into some calculations or summaries of the data.

# Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
logicalVectorMissingValues <- is.na(activityData$steps) | is.na(activityData$steps) | is.na(activityData$interval)
numberOfMissingValues <- sum(logicalVectorMissingValues)
#Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.



temp <- activityData
# firstCountNAs <- nrow(temp[is.na(temp$steps),])

# Replace values with the mean of that interval throughout the time interval
intervalMeans <- tapply(activityData$steps, activityData$interval, mean, simplify = T, na.rm=T)
for (var in 1:nrow(temp)) {
  if ( is.na(temp[var,]$steps) ){
    newValue <- intervalMeans[as.character(temp[var,]$interval)] # Interval mean throughout the time period
    temp[var,]$steps <- newValue
    print("It's NA")
    print(temp[var,]$date)
    print( 
        paste(
          "New value: ", newValue
        )
    )
  }  
}
# lastCount <- nrow(temp[is.na(temp$steps),])
# print("------------------")
# print(paste("First count: ", firstCountNAs))
# print(paste("Second count: ", lastCount))
# print("------------------")




#Create a new dataset that is equal to the original dataset but with the missing data filled in.

#Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

