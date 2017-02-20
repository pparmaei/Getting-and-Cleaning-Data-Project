#




#Set the working direcotry 
setwd("~/Getting and Cleaning Data/Project/getdata_2Fprojectfiles%2FUCI HAR Dataset/UCI HAR Dataset")
#Open up the Label and sets for Test and Train
trainLabel <- read.table("./train/y_train.txt")
trainSet <- read.table("./train/X_train.txt")
trainSubject <- read.table("./train/subject_train.txt")

#trainTotalAccX <- read.table("./train/Inertial Signals/total_acc_x_train.txt")
#trainTotalAccY <- read.table("./train/Inertial Signals/total_acc_y_train.txt")
#trainTotalAccZ <- read.table("./train/Inertial Signals/total_acc_z_train.txt")
#trainBodyAccX <- read.table("./train/Inertial Signals/body_acc_x_train.txt")
#trainBodyAccY <- read.table("./train/Inertial Signals/body_acc_y_train.txt")
#trainBodyAccZ <- read.table("./train/Inertial Signals/body_acc_z_train.txt")
#trainBodyGyroX <- read.table("./train/Inertial Signals/body_gyro_x_train.txt")
#trainBodyGyroY <- read.table("./train/Inertial Signals/body_gyro_y_train.txt")
#trainBodyGyroZ <- read.table("./train/Inertial Signals/body_gyro_z_train.txt")

activityLabels <- read.table("activity_labels.txt")

testLabel <- read.table("./test/y_test.txt")
testSet <- read.table("./test/X_test.txt")
testSubject <- read.table("./test/subject_test.txt")

#testTotalAccX <- read.table("./test/Inertial Signals/total_acc_x_test.txt")
#testTotalAccY <- read.table("./test/Inertial Signals/total_acc_y_test.txt")
#testTotalAccZ <- read.table("./test/Inertial Signals/total_acc_z_test.txt")
#testBodyAccX <- read.table("./test/Inertial Signals/body_acc_x_test.txt")
#testBodyAccY <- read.table("./test/Inertial Signals/body_acc_y_test.txt")
#testBodyAccZ <- read.table("./test/Inertial Signals/body_acc_z_test.txt")
#testBodyGyroX <- read.table("./test/Inertial Signals/body_gyro_x_test.txt")
#testBodyGyroY <- read.table("./test/Inertial Signals/body_gyro_y_test.txt")
#testBodyGyroZ <- read.table("./test/Inertial Signals/body_gyro_z_test.txt")

#Merge the Label and the Set together, and add the label "v562" and v563 for the label and subject
trainSet <- cbind(trainSet, data.frame(trainLabel), data.frame(trainSubject))
testSet <- cbind(testSet, data.frame(testLabel),data.frame(testSubject))
colnames(trainSet) <- c(colnames(trainSet)[1:561],"v562","v563")
colnames(testSet) <- c(colnames(testSet)[1:561],"v562","v563")

#Merging the Training and the Test DataSet together
totalDataSet <- rbind(trainSet, testSet)

#read the features Text File
features <- read.table("features.txt")

#add a column to the features table, which indicates
#if the word mean or std exists in each row
features$containsMeanorStd <- grepl('mean',features$V2) | grepl('std', features$V2) | grepl('Mean', features$V2)

#Add a row for the label that was added to achieve 562 columns
rowToAdd1 = data.frame(as.integer(562),as.factor('activityID'),as.logical('TRUE'))
rowToAdd2 = data.frame(as.integer(563),as.factor('Subject'),as.logical('TRUE'))
colnames(rowToAdd1) <- colnames(features)
colnames(rowToAdd2) <- colnames(features)
features <- rbind(features, rowToAdd1, rowToAdd2)

#Setting the colNames of totalDataSet based on the features names, and extracting only variables with Mean and Std
colnames(totalDataSet) <- as.character(features$V2)
totalDataSetMeanorStd <- totalDataSet[,features$containsMeanorStd]

#Use desscriptive Names to descriptive Activity and put subject and label first
colnames(activityLabels) <- c("activityID", "activityDesc")
totalNeededData <- merge(totalDataSetMeanorStd, activityLabels, by.x = "activityID", by.y ="activityID")
FinalDataSet <- totalNeededData[,2:ncol(totalNeededData)]
FinalDataSet <- FinalDataSet[,c(ncol(FinalDataSet)-1,ncol(FinalDataSet),1:(ncol(FinalDataSet)-2))]

#Create the independent Dataframe
#Make the Subject and Activity Factors
FinalDataSet$Subject <- as.factor(FinalDataSet$Subject)
FinalDataSet$activityDesc <- as.factor(FinalDataSet$activityDesc)
#Group by and create the summary table
groupedFinalDataSet <- group_by(FinalDataSet, Subject, activityDesc)
summaryTable <- summarize_each(groupedFinalDataSet, funs(mean))
