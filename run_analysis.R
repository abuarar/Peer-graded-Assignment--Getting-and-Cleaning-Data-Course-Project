library(dplyr)
library(downloader)
library(stringr)
#Loading necessary libraries

UrlData<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(UrlData,destfile = "./data.zip")
unzip("./data.zip",exdir = ".")
#Download compressed data from Url ,then unzip it to working directory

activity_labels_raw<-readLines("./UCI HAR Dataset/activity_labels.txt")
features_raw<-readLines("./UCI HAR Dataset/features.txt")
features_info_raw<-readLines("./UCI HAR Dataset/features_info.txt")
subject_train_raw<-readLines("./UCI HAR Dataset/train/subject_train.txt")
X_train_raw<-readLines("./UCI HAR Dataset/train/X_train.txt")
y_train_raw<-readLines("./UCI HAR Dataset/train/y_train.txt")

subject_test_raw<-readLines("./UCI HAR Dataset/test/subject_test.txt")
X_test_raw<-readLines("./UCI HAR Dataset/test/X_test.txt")
y_test_raw<-readLines("./UCI HAR Dataset/test/y_test.txt")
#Reading raw data from *.txt data files

features<-str_split_fixed(features_raw,pattern = "( )+",n=2)
features<-features[,2] 
#Extract the feature names vector from features.txt to apply it to column names in X set

cleanX = function(x){
  x<-str_split_fixed(x,pattern = "( )+",n=562) #561 feature vector
  x<-x[,2:562]
  r<-nrow(x)
  c<-ncol(x)
  x<-as.numeric(x)
  x<-matrix(x,nrow = r,ncol = c)
  x<-as.data.frame(x)
  names(x)<-features
  x
}
#Function to clean raw data of both train set (X_train_raw) & test set (X_test_raw)
X_train_tidy<-cleanX(X_train_raw)
X_test_tidy<-cleanX(X_test_raw)

selectMeanStd<-grepl(pattern = "[Mm][Ee][Aa][Nn]|[Ss][Tt][Dd]",features)
TrainSetOnlyMeanStd<-X_train_tidy[,selectMeanStd]
TestSetOnlyMeanStd<-X_test_tidy[,selectMeanStd]
# Extracts only the measurements on the mean and standard deviation for each measurement (Point-2 in Project requirements). 

activity_labels_tidy<-str_split_fixed(activity_labels_raw,pattern = " ",n=2)
activity_labels_tidy<-as.data.frame(activity_labels_tidy)
names(activity_labels_tidy)<-c("activity_number","activity_labels")
# Uses descriptive activity names to name the activities in the data set (Point-3 in Project requirements).

y_train_tidy<-as.numeric(as.character(y_train_raw))
y_train_tidy<-as.data.frame(y_train_tidy)
names(y_train_tidy)<-"activity_number"

y_test_tidy<-as.numeric(as.character(y_test_raw))
y_test_tidy<-as.data.frame(y_test_tidy)
names(y_test_tidy)<-"activity_number"

subject_train_tidy<-as.numeric(as.character(subject_train_raw))
subject_train_tidy<-as.data.frame(subject_train_tidy)
names(subject_train_tidy)<-"subject_number"

subject_test_tidy<-as.numeric(as.character(subject_test_raw))
subject_test_tidy<-as.data.frame(subject_test_tidy)
names(subject_test_tidy)<-"subject_number"
#Appropriately labels the data set with descriptive variable names (Point-4 in Project requirements)


TrainData<-cbind(subject_train_tidy,y_train_tidy,TrainSetOnlyMeanStd)
#Merge all Train data 
TestData<-cbind(subject_test_tidy,y_test_tidy,TestSetOnlyMeanStd)
#Merge all Test data
TidyData<-rbind(TrainData,TestData)
TidyData<-merge(activity_labels_tidy,TidyData,by.x="activity_number",by.y="activity_number")
#Merges the training and the test sets to create one data set (Point-1 in Project requirements), And add activity label to TidyData

TidyData_Point5<-aggregate(TidyData[4:89],by=TidyData[c("activity_number","activity_labels","subject_number")], FUN=mean)
TidyData_Point5<-arrange(TidyData_Point5,activity_number)
names(TidyData_Point5)[4:89]<-paste("AVG:",names(TidyData_Point5)[4:89])
#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject ((Point-5 in Project requirements))

write.table(TidyData_Point5,file = "./TidyData_Point5.txt",row.names = FALSE)
#Tidy data set created in step 5 of the instructions, this will be uploaded to repo as requested in the Project