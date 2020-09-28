require("reshape2")

## Download test and training data
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipped_file <- "UCI_Dataset.zip"
unzipped_dir <- "UCI HAR Dataset"

if(!file.exists(zipped_file)){
  download.file(url, zipped_file)
}

##unzip downloaded file
if(!dir.exists(unzipped_dir)){
  unzip(zipped_file, files = NULL, exdir=".")
}

## read data
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
y_train <- read.table("UCI HAR Dataset/train/y_train.txt")

activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")
features <- read.table("UCI HAR Dataset/features.txt")  

## name the subject and activity table
subject <- rbind(subject_train, subject_test)
names(subject) <- 'subject'
activity <- rbind(y_train, y_test)
names(activity) <- 'activity'

## combine train table and test table
combined_data <- rbind(x_train, x_test)

## Extracts only the measurements on the mean and standard deviation for each measurement
value <- grepl("mean()|std()", features[,2])
extract_data <- combined_data[, value]

## labels the data set with descriptive variable names
CleanFeatureNames <- sapply(features[, 2], function(x) {gsub("[()]", "",x)})
names(extract_data) <- CleanFeatureNames[value]

# combine subject, activity, and mean and std only data set to create final data set.
extract_data <- cbind(subject,activity, extract_data)
act_group <- factor(extract_data$activity)
levels(act_group) <- activity_labels[,2]
extract_data$activity <- act_group

baseData <- melt(extract_data,(id.vars=c("subject","activity")))
secondDataSet <- dcast(baseData, subject + activity ~ variable, mean)
names(secondDataSet)[-c(1:2)] <- paste("[mean of]" , names(secondDataSet)[-c(1:2)] )
write.table(secondDataSet, "tidy_data.txt", sep = ",", row.names = FALSE)
