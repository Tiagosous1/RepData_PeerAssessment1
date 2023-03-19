#starting library
library(dplyr)
library(ggplot2)

#setting english time
Sys.setlocale("LC_TIME", "English")

#reading data
data <- read.csv("activity.csv")

#Setting date from character to date
data$date <- as.Date(data$date, "%Y-%m-%d")

#remove Na´s
clean_data <- na.omit(data)

#What is mean total number of steps taken per day?
total_steps <- clean_data %>% 
              group_by(date) %>%
              summarise(sum = sum(steps))

#make the histogram (without Na´s)
ggplot(total_steps, aes(sum)) +
    geom_histogram(color=4, fill="White", bins = 30) +
    xlab("Number steps per day") +
    ylab("Frequency") +
    ggtitle("Total steps per day") +
    theme(plot.title = element_text(hjust = 0.5))

#mean of steps
mean_steps_day <- mean(total_steps$sum)
mean_steps_day

#median of steps
median_steps_day <- median(total_steps$sum)
median_steps_day

#average mean steps by interval
mean_steps <- clean_data %>% 
    group_by(interval) %>%
    summarise(mean = mean(steps))

#make the plot
ggplot(mean_steps, aes(interval, mean)) +
                    geom_line(col="blue") +
                    xlab("5 minutes interval") +
                    ylab("Average number of steps taken by day") +
                    ggtitle("The average daily activity pattern")

#max number of mean steps
max_five_min <- mean_steps$interval[which.max(mean_steps$mean)]
max_five_min

#total numbers of NA´s
sum_nas <- sum(is.na(data))
sum_nas

#mean steps by day
mean_steps_day <- clean_data %>% 
    group_by(date) %>%
    summarise(mean = mean(steps))

#Fill Na´s with 0
datav2 <- data
nas_num <- is.na(datav2$steps)
datav2$steps[nas_num] <- 0

#Confirm number of NA´s (must be 0)
sum(is.na(datav2$steps))

#Group by date
total_stepsv2 <- datav2 %>% 
    group_by(date) %>%
    summarise(sum = sum(steps))

#histogram with NA´s replaced by 0
ggplot(total_stepsv2, aes(sum)) +
    geom_histogram(color=4, fill="white", bins=30) +
    xlab("Number steps per day") +
    ylab("Frequency") +
    ggtitle("Total steps per day")

#mean of steps with NA´s
mean_steps_dayv2 <- mean(total_stepsv2$sum)
mean_steps_dayv2

#median of steps with NA´s
median_steps_dayv2 <- median(total_stepsv2$sum)
median_steps_dayv2

#What is the impact of imputing missing data
summary(total_steps)

summary(total_stepsv2)

#Are there differences between weekdays and weekends
datav2$weekday <- sapply(datav2$date, function(x){
    if(weekdays(x) == "Saturday" | weekdays(x)=="Sunday"){
        weekday <- "weekend"
    } else {
        weekday <- "weekday"
    }
})

#average steps by weekday
mean_steps_weekday <- datav2 %>% 
    group_by(interval, weekday) %>%
    summarise(mean = mean(steps))

#plot weekdays
ggplot(mean_steps_weekday, aes(interval, mean, color = weekday)) +
    geom_line() +
    facet_grid(weekday ~ .) +
    xlab("5 minutes interval") +
    ylab("Average number of steps") +
    ggtitle("Activity pattern by weekday")