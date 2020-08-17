# Reading the data into R
test_data <- read.table("UCI HAR Dataset/test/X_test.txt")
train_data <- read.table("UCI HAR Dataset/train/X_train.txt")
test_feature <- read.table("UCI HAR Dataset/test/y_test.txt")
train_feature <- read.table("UCI HAR Dataset/train/y_train.txt")
feature <- read.table("UCI HAR Dataset/features.txt")
activity <- read.table("UCI HAR Dataset/activity_labels.txt")
test_subject <- read.table("UCI HAR Dataset/test/subject_test.txt")
train_subject <- read.table("UCI HAR Dataset/train/subject_train.txt")

# Note in each observation who has done the activity
test_data <- cbind(test_subject, test_data)
train_data <- cbind(train_subject, train_data)

# Note in each observation which activity has been done
test_data <- cbind(test_feature, test_data)
train_data <- cbind(train_feature, train_data)

# Merge the training and the test sets to create one data set
total_data <- rbind(train_data, test_data)

# Use descriptive activity names to name the activities in the data set
for (i in 1:6) {
        total_data[, 1]<- gsub(as.character(i), activity[i, 2], total_data[, 1])
}

# labels the data set with descriptive variable names
colname <- vector(length = length(feature[, 2]) + 2)
colname[1] <- "activity"
colname[2] <- "subject"
colname[3:length(colname)] <- feature[, 2]
colname <- gsub("-", "", colname)
colname <- gsub("\\(", "", colname)
colname <- gsub(")", "", colname)
colnames(total_data) <- colname

# Extract only the measurements on the mean and standard deviation 
# for each measurement
extract <- grepl("mean|std", feature[, 2])
extract_data <- total_data[, extract]

# Create a  data set with the average of each variable 
# for each activity and each subject
ave_list <- list()
for (i in 3:ncol(extract_data)) {
        ave_list[[i - 2]] <- tapply(extract_data[, i], list(extract_data[, 1], 
                                       extract_data[, 2]), mean)
}
names(ave_list) <- colnames(extract_data)[3:ncol(extract_data)]

# Prepare for the output file
output <- data.frame()
for (i in 1: length(ave_list)) {
        for (k in 1: ncol(ave_list[[i]])) {
                 output[(6 * k - 5): (6 * k), i] <- ave_list[[i]][,k]
        }
}
Subject <- rep(1: 30, each = 6)
Activity <- rep(activity[, 2], length.out = 180)
output <- cbind(Subject, Activity, output)
colnames(output)[3: ncol(output)] <- colnames(extract_data)[3:ncol(extract_data)]
write.table(output, "output.txt", row.names = FALSE)
