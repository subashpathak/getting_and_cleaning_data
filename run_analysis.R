setwd("C:/Users/spathak/Desktop")

library(dplyr)
install.packages("data.table")
library(data.table)

#download the data and put in the folder
if (!file.exists("./clean")) {dir.create("./clean")}

#download 
fileurl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

download.file(fileurl,destfile="./clean/Dataset.zip")

#unzip the file
unzip(zipfile="./clean/Dataset.zip",exdir="./clean")

#list of files
path<-file.path("./clean","UCI HAR Dataset")
list<-list.files(path,recursive=TRUE)
list

#read the features and activity
featurenames<-read.table("./clean/UCI HAR Dataset/features.txt")
activities<-read.table("./clean/UCI HAR Dataset/activity_labels.txt")

#reading training data
subjecTrain<-read.table("./clean/UCI HAR Dataset/train/subject_train.txt",header=FALSE)
featureTrain<-read.table("./clean/UCI HAR Dataset/train/x_train.txt",header=FALSE)
activityTrain<-read.table("./clean/UCI HAR Dataset/train/y_train.txt",header=FALSE)

#reading Test data
subjecTest<-read.table("./clean/UCI HAR Dataset/test/subject_test.txt",header=FALSE)
featureTest<-read.table("./clean/UCI HAR Dataset/test/x_test.txt",header=FALSE)
activityTest<-read.table("./clean/UCI HAR Dataset/test/y_test.txt",header=FALSE)

#merge training and test data sets
subject<-rbind(subjecTrain,subjecTest)
features<-rbind(featureTrain,featureTest)
activity<-rbind(activityTrain,activityTest)

colnames(features)<- t(featurenames[2])
colnames(activity)<-"Activity"
colnames(subject)<-"Subject"

#merge training and test data
complete<-cbind(subject,features,activity)

#extract columns with mean or std

extractedata<-grep(".*Mean.*|.*Std.*", names(complete), ignore.case=TRUE)

#add columns to extracted data

addcolumns<-c(extractedata,1,563)


extract<-complete[,addcolumns]
names(extract)

#Uses descriptive activity names to name the activities in the data set
extract$Activity <- as.character(extract$Activity)
for (i in 1:6){
  extract$Activity[extract$Activity == i] <- as.character(activities[i,2])
}

#factor activity variable
extract$Activity<-as.factor(extract$Activity)

#By looking extract, we can say that the following acronyms can be replaced:
  
#Acc can be replaced with Accelerometer

#Gyro can be replaced with Gyroscope

#BodyBody can be replaced with Body

#Mag can be replaced with Magnitude

#Character f can be replaced with Frequency

#Character t can be replaced with Time


names(extract)<-gsub("Acc", "Accelerometer", names(extract))
names(extract)<-gsub("Gyro", "Gyroscope", names(extract))
names(extract)<-gsub("BodyBody", "Body", names(extract))
names(extract)<-gsub("Mag", "Magnitude", names(extract))
names(extract)<-gsub("^t", "Time", names(extract))
names(extract)<-gsub("^f", "Frequency", names(extract))
names(extract)<-gsub("tBody", "TimeBody", names(extract))
names(extract)<-gsub("-mean()", "Mean", names(extract), ignore.case = TRUE)
names(extract)<-gsub("-std()", "STD", names(extract), ignore.case = TRUE)
names(extract)<-gsub("-freq()", "Frequency", names(extract), ignore.case = TRUE)
names(extract)<-gsub("angle", "Angle", names(extract))
names(extract)<-gsub("gravity", "Gravity", names(extract))

#list of modified column names
names(extract)


#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject

#set subject as a factor variable
extract$Subject <- as.factor(extract$Subject)
extract <- data.table(extract)

extract$Activity <- as.factor(extract$Activity)
extract <- data.table(extract)

group<-group_by(extract,Subject,Activity)
tidy<-summarise_each(group,funs(mean(.,na.rm=TRUE)))


#write to text file
write.table(tidy,file="./clean/tidy.txt",row.name=FALSE)
    
    
 



