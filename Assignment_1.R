library(ggplot2)
library(lattice)

data <- read.table('activity.csv',sep = ",",header = TRUE, na.strings ="NA",colClasses = c('integer','Date','factor'))
new.data <- na.omit(data)

total.steps <- tapply(new.data$steps, new.data$date, FUN = sum)

plot1 <- ggplot(new.data, aes(date, steps)) + geom_bar(stat = "identity",binwidth = .5) +labs(title = "Histogram of Total Number of Steps Taken Each Day",x = "Date", y = "Total Number of Steps")

print(plot1)

mean(total.steps)

averages <- aggregate(new.data$steps, list(interval = as.numeric(as.character(new.data$interval))), FUN = "mean")
names(averages)[2] <- "Avg.Steps"



plot2 <- ggplot(averages, aes(interval, Avg.Steps)) + geom_line(color = "green", size = 0.7) + labs(title = "Time Series Plot of the 5-minute Intervals", x = "5-minute intervals", y = "Average Number of Steps Taken")

print(plot2)


averages[averages$Avg.Steps == max(averages$Avg.Steps),]

sum(!complete.cases(data))

impData <- data 
for (i in 1:nrow(impData)) {
    if (is.na(impData$steps[i])) {
        impData$steps[i] <- averages[which(impData$interval[i] == averages$interval), ]$Avg.Steps
    }
}

plot3 <- ggplot(impData, aes(date, steps)) + geom_bar(stat = "identity",binwidth = .5) +labs(title = "Histogram of Total Number of Steps Taken Each Day (Imputed Data)",x = "Date", y = "Total Number of Steps")


print(plot3)

total.steps.impute <- tapply(impData$steps, impData$date, FUN = sum)
mean(total.steps.impute)

median(total.steps.impute)

impData$weekdays <- factor(format(impData$date, "%A"))
levels(impData$weekdays)

new.averages <- aggregate(impData$steps, 
                      list(interval = as.numeric(as.character(impData$interval)), 
                           weekdays = impData$weekdays),
                      FUN = "mean")
names(new.averages)[3] <- "meanOfSteps"

plot4 <- xyplot(new.averages$meanOfSteps ~ new.averages$interval | new.averages$weekdays, 
       layout = c(1, 2), type = "l", 
       xlab = "Interval", ylab = "Number of steps")
print(plot4)