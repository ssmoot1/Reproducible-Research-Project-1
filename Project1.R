# Reproducible Research Project 1

act<- read.csv("activity.csv", header = TRUE)

steps<- aggregate(steps ~ date, act, sum)
hist(steps$steps, main= "Histogram of total number of steps taken each day", xlab="Steps")

mean(steps$steps)

median(steps$steps)

avg_step_int<- aggregate(steps ~ interval, act, mean)

plot(avg_step_int$interval, avg_step_int$steps, type="l", col=1, main="Average number of steps by Interval", 
     xlab="Time Intervals", ylab="Average number of steps")

avg_step_int[which.max(avg_step_int$steps), ]

missing_value<- is.na(act$steps)
table(missing_value)

for (i in 1:nrow(act)) {
        if(is.na(act$steps[i])) {
                val <- avg_step_int$steps[which(avg_step_int$interval == act$interval[i])]
                act$steps[i] <- val 
        }
}

steps_impute <- aggregate(steps ~ date, act, sum)

hist(steps_impute$steps, main = "Histogram of total number of steps taken each day (Imputed)", 
     xlab = "Steps")

mean(steps_impute$steps)

median(steps_impute$steps)

week_day <- function(date_valid) {
        wd <- weekdays(as.Date(date_valid, '%Y-%m-%d'))
        if  (!(wd == 'Saturday' || wd == 'Sunday')) {
                x <- 'Weekday'
        } else {
                x <- 'Weekend'
        }
        x
}

act$day_type <- as.factor(sapply(act$date, week_day))

library(ggplot2)

steps_impute <- aggregate(steps ~ interval+day_type, act, mean)

graph <- ggplot(steps_impute, aes(interval, steps)) +
        geom_line(stat = "identity", aes(colour = day_type)) +
        facet_grid(day_type ~ ., scales="fixed", space="fixed") +
        labs(x="Interval", y=expression("Steps")) +
        ggtitle("Steps per Interval by day type")
print(graph
