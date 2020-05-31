
#downloading and unzipping file, changing working directory
temp= tempfile()
url = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, "temp.zip")
unzip("temp.zip")
list.files()
setwd("./UCI HAR Dataset")

#importing datasets
list.files()
list.files("./train")
xtrain= read.table("./train/X_train.txt")
ytrain= read.table('./train/y_train.txt')
xtest= read.table("./test/X_test.txt")
ytest= read.table("./test/y_test.txt")

#combining train and test
x= rbind(xtrain, xtest)
y=rbind(ytrain,ytest)

#assigning rownames to x 
features= read.table("features.txt")
xnames= as.vector(features[,2] )
colnames(x)= xnames

#filtering mean and std variables
column_mean= grep("mean", xnames)
column_std= grep("std", xnames)
x2 = x[,c(column_mean,column_std)]

#adding subject and y to form data
subject_train = read.table("./train/subject_train.txt")
subject_test = read.table("./test/subject_test.txt")
subject= rbind(subject_train, subject_test)

data1 = cbind(y,subject,x2)

#changing colnames of newly added columns
names(data1)[c(1,2)]<- c("y", "subject")

#replacing with activity name
activity= read.table("activity_labels.txt")
names(activity)=c("y","activity")
data1$y = as.factor(data1$y)
levels(data1$y)
data2= merge(data1, activity, by="y", all.x= TRUE)

#loading dplyr
if ("dplyr" %in% rownames(installed.packages())){
  library(dplyr)
} else{
  install.packages("dplyr")
  library(dplyr)
}

#mean of each subject and each activity
data3= data2[,-1]  #dropping y as activity var exists
data4= data3 %>% 
  group_by(activity,subject) %>%
  summarize_all(mean)

write.table(data2, "dataset_1.txt", row.names=FALSE)
write.table(data4, "dataset_2.txt", row.names= FALSE)
  

