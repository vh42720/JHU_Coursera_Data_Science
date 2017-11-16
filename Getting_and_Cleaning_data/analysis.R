library(dplyr)
library(data.table)

#getting the data folder
if(!file.exists("projects")){dir.create("projects")}

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile = "./projects/UCI_data_set.zip")
setwd("./projects")
unzip("UCI_data_set.zip")

#Loading activity labels and features
activity_labels <- read.table("./UCI HAR dataset/activity_labels.txt", header = F, colClasses = "character")
features <- read.table("./UCI HAR dataset/features.txt", header = F, colClasses = "character")

#Extracting Mean and SD (Mean with capital "M" will be excluded because it returns different data)
features_names <- grep(".*mean.*|.*std.*", features[,2], value = TRUE)
features_names <- gsub("-mean", "Mean", features_names)
features_names <- gsub("-std", "Std", features_names) 
features_names <- gsub("[()-]", "", features_names)
logical_features <- grep(".*mean.*|.*std.*", features[,2])

#Loading data from the UCI HAR dataset

train <- read.table("./UCI HAR dataset/train/X_train.txt")[logical_features]
train_labels <- read.table("./UCI HAR dataset/train/y_train.txt")
train_subject <- read.table("./UCI HAR dataset/train/subject_train.txt")
train_data <- cbind(train_labels, train_subject, train)

test <- read.table("./UCI HAR dataset/test/X_test.txt")[logical_features]
test_labels <- read.table("./UCI HAR dataset/test/y_test.txt")
test_subject <- read.table("./UCI HAR dataset/test/subject_test.txt")
test_data <- cbind(test_labels, test_subject, test)

#Combine both train and test data and name them

data_combine <- rbind(train_data, test_data)
colnames(data_combine) <- c("labels", "subject", features_names)

#Create a tidy data 
data_tidy <- aggregate(data_combine, by = list(data_combine$labels, data_combine$subject), FUN = mean)
colnames(data_tidy[,1:2])<- c("activities","subjects")
write.table(data_tidy, "data_tidy.txt", row.names = FALSE, quote = FALSE)