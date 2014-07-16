# Reproducible research, project 1
library(dplyr)
data <- read.csv("activity.csv")
data <- mutate(data, date=as.Date(data$date)) #, interval=as.factor(data$interval)


## What is mean total number of steps taken per day?
totalstepbydate <- data %>%
        filter(!is.na(steps)) %>%
        group_by(date) %>%
        summarise(totalstep=sum(steps,na.rm=TRUE))

# histogram
hist(totalstepbydate$totalstep,breaks=30,xlab="Total steps per day", main="Distribution of total steps per day")

meantotalstep <- as.integer(round(mean(totalstepbydate$totalstep)))
mediantotalstep <- median(totalstepbydate$totalstep)


## What is the average daily activity pattern?
avgintervalstep <- data %>%
        group_by(interval) %>%
        summarise(avgstep=mean(steps,na.rm=TRUE))

# time series plot:
with(avgintervalstep,
     plot(interval,avgstep,type='l',xlab='Time of day',ylab='Average steps')
)
maxactivity <- avgintervalstep$interval[which.max(avgintervalstep$avgstep)]

## Imputing missing values
totalna <- sum(is.na(data$steps))
# use mean of interval as estimates
# datafilled <- data %>%
#         filter(is.na(steps)) 
# datafilled <- datafilled %>%
#         mutate(steps=avgintervalstep$avgstep[avgintervalstep$interval==datafilled$interval])

# data2 <- merge(data,avgintervalstep,by.x="interval",all=TRUE)
# data2 <- arrange(data2,date)
# View(data2)
 
data2 <- data %>%
        merge(avgintervalstep,by.x="interval",all=TRUE) %>%
        filter(is.na(steps)) %>%
        mutate(steps=avgstep) %>%
        select(steps,date,interval) %>%
        rbind_list(data[!is.na(data$steps),]) %>%
        arrange(date)
View(data2)

# data3 <- rbind(data2[,c("steps","date","interval")],data[!is.na(data$steps),])
# data3 <- arrange(data3,date)
# View(data3)
# 
# data3 <- data2 %>%
#         select(steps,date,interval) %>%
#         rbind_list(data[!is.na(data$steps),]) %>%
#         arrange(date)
# View(data3)

totalstepbydate2 <- data2 %>%
        filter(!is.na(steps)) %>%
        group_by(date) %>%
        summarise(totalstep=sum(steps,na.rm=TRUE))

hist(totalstepbydate2$totalstep,breaks=30,xlab="Total steps per day", main="Distribution of daily steps - missing value filled")
meantotalstep2 <- as.integer(round(mean(totalstepbydate2$totalstep)))
mediantotalstep2 <- median(totalstepbydate2$totalstep)


## Are there differences in activity patterns between weekdays and weekends?
data2 <- data2 %>%
        mutate(wd=weekdays(date),wday="")
data2$wday[data2$wd %in% c("Saturday","Sunday")] <- "weekend"
data2$wday[!data2$wd %in% c("Saturday","Sunday")] <- "weekday"
data2 <- data2 %>%
        select(-wd) %>%
        mutate(wday=as.factor(wday))

# time series plot for weekdays and weekends
avgintervalstep2 <- data2 %>%
        group_by(wday,interval) %>%
        summarise(avgstep=mean(steps))
library(ggplot2)
g <- ggplot(avgintervalstep2,aes(x=interval,y=avgstep))
g+facet_grid(.~wday)+geom_line()+labs(y="Average daily steps")+labs(title="Daily activity")


