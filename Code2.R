#You should create one R script called run_analysis.R that does the following. 

#Merges the training and the test sets to create one data set.

training = read.csv("UCI HAR Dataset/train/X_train.txt", sep="", header=FALSE)
training[,562] = read.csv("UCI HAR Dataset/train/Y_train.txt", sep="", header=FALSE)
training[,563] = read.csv("UCI HAR Dataset/train/subject_train.txt", sep="", header=FALSE)
testing = read.csv("UCI HAR Dataset/test/X_test.txt", sep="", header=FALSE)
testing[,562] = read.csv("UCI HAR Dataset/test/Y_test.txt", sep="", header=FALSE)
testing[,563] = read.csv("UCI HAR Dataset/test/subject_test.txt", sep="", header=FALSE)
activityLabels = read.csv("UCI HAR Dataset/activity_labels.txt", sep="", header=FALSE)
features = read.csv("UCI HAR Dataset/features.txt", sep="", header=FALSE)
features[,2] = gsub('-mean', 'Mean', features[,2])
features[,2] = gsub('-std', 'Std', features[,2])
features[,2] = gsub('[-()]', '', features[,2])

allData = rbind(training, testing)

# Extracts only the measurements on the mean and standard deviation for each measurement.
cols <- grep(".*Mean.*|.*Std.*", features[,2])

# Uses descriptive activity names to name the activities in the data set.

features <- features[cols,]
cols <- c(cols, 562, 563)
allData <- allData[,cols]

# Appropriately labels the data set with descriptive variable names. 
colnames(allData) <- c(features$V2, "Activity", "Subject")
colnames(allData) <- tolower(colnames(allData))

# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
currentActivity = 1
for (currentActivityLabel in activityLabels$V2) {
  allData$activity <- gsub(currentActivity, currentActivityLabel, allData$activity)
  currentActivity <- currentActivity + 1
}

allData$activity <- as.factor(allData$activity)
allData$subject <- as.factor(allData$subject)

tidy_data = aggregate(allData, by=list(activity = allData$activity, subject=allData$subject), mean)

write.table(tidy_data, "tidy_data.txt", row.names=FALSE)
