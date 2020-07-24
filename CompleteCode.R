#package
library(dplyr)
library(ggplot2)
library(lubridate)
library(data.table)
#downlaoding file
file<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(file,destfile = "./dataset.zip")
#unziping file
unzip("./dataset.zip")
#reading the file
activity<-read.csv("./activity.csv",sep = ",",header = T)
#conversion of column data
activity$date<-ymd(activity$date)
##What is mean total number of steps taken per day?
#calculating total no of steps per day
total_step<-activity%>%group_by(date)%>%summarise(total=sum(steps,na.rm = T))
#output
head(total_step)
#histogram of the total number of steps taken each day
hist(total_step$total,xlab="Total Steps",main = "TOTAL STEPS PER DAY")
#mean of total steps per day
print(paste0("Mean is : ",mean(total_step$total)))
#median of total steps per day
print(paste0("median is : ",median(total_step$total)))
##What is the average daily activity pattern?
Time5minInter<-activity%>%group_by(interval)%>%summarise(average_step=mean(steps,na.rm = T))
#view new sub data
head(Time5minInter)
#ploting time series graph were average steps in depend on intervals
plot(Time5minInter$average_step~Time5minInter$interval,
     type = "l",col="red",
     xlab="5 min Interval",
     ylab = "Average Steps")
#max stepsin a intervals
inter<-Time5minInter[which.max(Time5minInter$average_step),]
print(paste0("interval with max step is : ",inter[,1],"with value : ",round(inter[,2],digits = 3)))
##Imputing missing values
#total no of missing values
print(paste0("total no of missing values in steps column : ",sum(is.na(activity$steps))))
#creating copy of original data set
newActivity<-activity
##imputing NAs
#function to fetch appropriate average/mean value for the particular interval
getMeanStep<-function(x){
    m<-Time5minInter[Time5minInter$interval==x,]
    return(as.numeric(m$average_step))
}
#for loop to iterate 
for (i in 1:nrow(newActivity)) {
    if(is.na(newActivity$steps[i])){#checking for NA values
        #if true send the interval to getMeanStep() function to get average step
        #and then set it to row
        newActivity$steps[i]<-getMeanStep(newActivity$interval[i])
    }
}
#new data set without Na
head(newActivity)#displaying new dataset
print(paste0("number of NAs : ",sum(is.na(newActivity$steps))))#print total Na values
total_step_newAct<-newActivity%>%group_by(date)%>%summarise(total=sum(steps,na.rm = T))
head(total_step_newAct)
#histogram of the total number of steps taken each day after imputing
hist(total_step_newAct$total,main = "after imputing",xlab = "Total steps")
#mean of total steps per day after imputing
print(paste0("Mean after imputing is : ",mean(total_step_newAct$total)))
#median of total steps per day after imputing
print(paste0("median after imputing is : ",median(total_step_newAct$total)))
#comparing old and new imputed dataset

par(mfrow=c(1,2))
hist(total_step$total,main = "before imputing",col = "red",xlab = "Total steps")
hist(total_step_newAct$total,main = "after imputing",col = "green",xlab = "Total steps")
#from this compair plot we can clearly understand that NAs impact on the dataset
#and the values clearly differes from the estimated data from original dataset
#if we compare mean and median from both for dataset
compareTable<-data.frame(imputed=c(mean(total_step_newAct$total),median(total_step_newAct$total)),
                         original=c(mean(total_step$total),median(total_step$total)))
rownames(compareTable)<-c("mean","median")
print(compareTable)

##Are there differences in activity patterns between weekdays and weekends?
#adding factor to dataset on basis of weekday and weekend
newActivity$days<-ifelse(weekdays(newActivity$date) %in% c("Saturday","Sunday"),"weekend","weekday")
newActivity$days<-as.factor(newActivity$days)
#visualizing factors in dataset
table(newActivity$days)
#subseting according to weekday and weekend
WeekdayData <- filter(newActivity, newActivity$day == "weekday")
WeekendData <- filter(newActivity, newActivity$day == "weekend")
#generating dataset on 5 min interval and average steps for weekday and weekend
totStepWD<-WeekdayData%>%group_by(interval)%>%summarise(avg_step=mean(steps,na.rm = T))
totStepWE<-WeekendData%>%group_by(interval)%>%summarise(avg_step=mean(steps,na.rm = T))
#ploting
par(mfrow=c(2,1),mar=c(4.5,4,2,2))
plot(totStepWD$avg_step~totStepWD$interval,
     col="red",type="l",main="On WeekDay",
     xlab="",ylab = "Average Steps")
plot(totStepWE$avg_step~totStepWE$interval,
     col="blue",type="l",main="On WeekEnd",
     xlab="Interval",ylab = "Average Steps")
