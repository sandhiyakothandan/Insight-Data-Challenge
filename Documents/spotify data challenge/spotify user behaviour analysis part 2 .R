#importing the modified data 
library(caret)
gc()
songfile <- read.csv("C:\\Users\\Sandhya\\data_sample\\end_song_sample.csv",na.strings=c("NA", "NULL"))

userfile <- read.csv("C:\\Users\\Sandhya\\data_sample\\user_data_sample.csv",na.strings=c("NA", "NULL"))

total <- merge(songfile,userfile,by="user_id")
dim(total)
head(total)

#date time conversion from UNIX Epoch

datets <- as.POSIXct(as.numeric(as.character(total$end_timestamp)), origin='1970-01-01', tz='GMT')

#mergin the changed date 
total$datetime <- datets

#fetching the days the users logged in 

dayslogged <- strptime(total$datetime, format="%Y-%m-%d %H:%M:%S" , tz = "GMT")+0
day = format(dayslogged, "%A")
unique(day)
head(day)

head(dayslogged)
#formatting the hours
hours = format(dayslogged, "%H")
head(hours)
head(total)


#ploting the data on days  for better understanding when the users are active
barplot( table(factor(day, levels=c("Monday", "Tuesday", "Wednesday","Thursday", "Friday", "Saturday", "Sunday"))), xlab="Day", ylab="Count",col="red", border="blue", main='Visits Per Weekday')

#plotting the hour to see the active time 
barplot(table(hours), xlab="Hour", ylab="Count", col="lightgreen",las=2, border="gray")

#plotting the age range of the users
histogram(total$age_range,total)

#plotting the type of context against the freq
histogram(total$context,frequency(total))



set.seed(3456)
trainIndex <- createDataPartition(total$user_id, p = .3,
                                  list = FALSE,
                                  times = 1)
head(trainIndex)

totalTrain <- total[ trainIndex,]
totalTest  <- total[-trainIndex,]





head(totalTest)
head(totalTrain)


#creating response and predictor variable 

responseY <- (totalTrain)[10]
head(responseY)
dim(responseY)
#predictorX <- links[,2:(dim(links)[2])]
predictorX <- data.frame(totalTrain$ms_played,as.numeric(totalTrain$gender), as.numeric(totalTrain$country),as.numeric(totalTrain$context))



head(predictorX)

#doing PCA to facilitate clusturing 
pca <- princomp(predictorX, cor=T)

pc.comp <- pca$scores
pc.comp1 <- -1*pc.comp[,1] # principal component 1 scores (negated for convenience)
pc.comp2 <- -1*pc.comp[,2] # principal component 2 scores (negated for convenience)


X <- cbind(pc.comp1, pc.comp2)

#ploting the clustered ,model
cl <- kmeans(X,13,iter.max = 10)
cl$cluster
plot(pc.comp1, pc.comp2,col=cl$cluster)
points(cl$centers, pch=10)


