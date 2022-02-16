library(lubridate)
library(magrittr)
library(dplyr)
library(ggplot2)

fileurl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
zipfile <- "../Reproducible Research/data.zip"

filedir <- "../Reproducible Research"
unzip_path <- "../Reproducible Research/data"

if (!file.exists(filedir)){
  dir.create(filedir)
}
download.file(fileurl, file.path(zipfile))
unzip(zipfile, exdir = unzip_path)
datafile <- file.path(unzip_path, "activity.csv")


stepdata <- read.csv(datafile, header = TRUE)
head(stepdata)

databydate <- stepdata %>% select(date, steps) %>% group_by(date) %>% summarise(tsteps = sum(steps)) %>% na.omit()
hist(databydate$tsteps, xlab = "Total daily Steps", main = "Histogram of Total Steps by day", breaks = 20)


mean(databydate$tsteps)
median(databydate$tsteps)

databyinterval <- stepdata %>% select(interval, steps) %>% na.omit() %>% group_by(interval) %>% summarise(tsteps=mean(steps))
ggplot(databyinterval, aes(x=interval, y=tsteps)) + geom_line()

databyinterval[which(databyinterval$tsteps==max(databyinterval$tsteps)), ]


missingVals <- sum(is.na(data))
missingVals

replacewithmean <- function(x) replace(x, is.na(x), mean(x, na.rm=TRUE))
meandata <- stepdata %>% group_by(interval) %>% mutate(steps= replacewithmean(steps))
head(meandata)

FullSummedDataByDay <- aggregate(meandata$steps, by = list(meandata$date), sum)

names(FullSummedDataByDay)[1] = "date"
names(FullSummedDataByDay)[2] = "totalsteps"
head(FullSummedDataByDay, 15)

summary(FullSummedDataByDay)

hist(FullSummedDataByDay$totalsteps, xlab = "Steps", ylab = "Frequency", main = "Total Daily Steps", breaks = 20)

oldmean <- mean(databydate$tsteps, na.rm = TRUE)
newmean <- mean(FullSummedDataByDay$totalsteps)
oldmean
newmean

oldmedian <- median(databydate$tsteps, na.rm = TRUE)
newmedian <- median(FullSummedDataByDay$totalsteps)
oldmedian
newmedian

meandata$date <- as.Date(meandata$date)
meandata$weekday <- weekdays(meandata$date)
meandata$weekend <- ifelse(meandata$weekday=="Saturday" | meandata$weekday=="Sunday", "Weekend", "Weekday")

meandataweekendweekday <- aggregate(meandata$steps, by = list(meandata$weekend, meandata$interval), na.omit(mean))
names(meandataweekendweekday) <- c("weekend", "interval", "steps")  

ggplot(meandataweekendweekday, aes(x=interval, y=steps, color=weekend)) + geom_line()+ facet_grid(weekend ~.) + xlab("Interval") + ylab("Mean of Steps") + ggtitle("Comparison of Average Number of Steps in Each Interval")

