library(ggplot2)
library(plyr)
library(sqldf)

library(AppliedPredictiveModeling)
library(caret)

#importing the data file 

songfile <- read.csv("C:\\Users\\Sandhya\\data_sample\\end_song_sample.csv",na.strings=c("NA", "NULL"))

userfile <- read.csv("C:\\Users\\Sandhya\\data_sample\\user_data_sample.csv",na.strings=c("NA", "NULL"))

#converting the timestamp into datetime format
datets <- as.POSIXct(as.numeric(as.character(songfile$end_timestamp)), origin='1970-01-01', tz='GMT')


songfile$Datetime <- datets
justdate <- as.Date(songfile$Datetime)
songfile$DATE <- justdate

#grouping users and their over all session time

library(data.table)
DT <- data.table(songfile, key="user_id")
cdatasong <- DT[, list(ms_played=sum(ms_played)), by=user_id ]

dim(cdatasong)
head(cdatasong)

cdatausersong <- merge(cdatasong,userfile,by="user_id")

#plotting the user's country and minutes played
attach(cdatausersong)
plot(country, ms_played, main="Scatterplot Example",xlab="country ", ylab="ms_played ", pch=19) 




#creating temfile to merge with the data from db
tempfile <- cdatausersong
head(tempfile)

#tempfile$date <- NULL
tempfile$age_range <- NULL
tempfile$gender <- NULL
tempfile$user_id <- NULL
tempfile$acct_age_weeks <- NULL
head(tempfile)

tempfile$country <- as.numeric(tempfile$country)
#plotting correlation of the data 

pairs(tempfile)

attach(tempfile)
levels(as.factor(country))

str(cdatausersong)

cor(country, ms_played, method ="spearman")

plot(country,ms_played)

plot(country,ms_played, pch = 16, col = "red", main = "country VS session length")


