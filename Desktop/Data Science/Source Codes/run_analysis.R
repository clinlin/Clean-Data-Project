run_analysis <- function {
  ## Merge Training & Test Data Sets
  
  ## Assume "UCI HAR Dataset" folder is in "./data" directory
  
  ## Read Test Subject ID, Activity, Data & form a new data frame testDataSet
  testSubjectID<-read.table("./data/UCI HAR Dataset/test/subject_test.txt")
  testActivity<-read.table("./data/UCI HAR Dataset/test/y_test.txt")
  testData<-read.table("./data/UCI HAR Dataset/test/x_test.txt")
  testDataSet<-data.frame(c(testSubjectID,testActivity))
  
  ## Assign names to first & second columns
  colnames(testDataSet)[1]<-"Subject.ID"
  colnames(testDataSet)[2]<-"Activity"
  
  ## Read Training Subject ID, Activity, Data & form a new data frame trainDataSet
  trainSubjectID<-read.table("./data/UCI HAR Dataset/train/subject_train.txt")
  trainActivity<-read.table("./data/UCI HAR Dataset/train/y_train.txt")
  trainData<-read.table("./data/UCI HAR Dataset/train/x_train.txt")
  trainDataSet<-data.frame(c(trainSubjectID,trainActivity,trainData))
  
  ## Assign names to first & second columns
  colnames(trainDataSet)[1]<-"Subject.ID"
  colnames(trainDataSet)[2]<-"Activity"
  
  ## Merge the two data sets by first & second columns
  HARDataSet<-merge(testDataSet, trainDataSet, by=c("Subject.ID","Activity"), all=T)
  
  ## Extract the mean & standdard deviation for each measurement
  extractedDataSet<-data.frame(HARDataSet[,1:8],HARDataSet[,43:48],
                               HARDataSet[,123:128],HARDataSet[,564:569],
                               HARDataSet[,604:609],HARDataSet[,684:689])
  
  ## Replace Activity with descriptive activity names
  
  ## Label the variable with descriptive variable names
  colnames(extractedDataSet)[3]<-"test.tBodyAcc.X.mean"
  colnames(extractedDataSet)[4]<-"test.tBodyAcc.Y.mean"
  colnames(extractedDataSet)[5]<-"test.tBodyAcc.Z.mean"
  colnames(extractedDataSet)[6]<-"test.tBodyAcc.X.std"
  colnames(extractedDataSet)[7]<-"test.tBodyAcc.Y.std"
  colnames(extractedDataSet)[8]<-"test.tBodyAcc.Z.std"
  colnames(extractedDataSet)[9]<-"test.tGravityAcc.X.mean"
  colnames(extractedDataSet)[10]<-"test.tGravityAcc.Y.mean"
  colnames(extractedDataSet)[11]<-"test.tGravityAcc.Z.mean"
  colnames(extractedDataSet)[12]<-"test.tGravityAcc.X.std"
  colnames(extractedDataSet)[13]<-"test.tGravityAcc.Y.std"
  colnames(extractedDataSet)[14]<-"test.tGravityAcc.Z.std"
  colnames(extractedDataSet)[15]<-"test.tBodyGyro.X.mean"
  colnames(extractedDataSet)[16]<-"test.tBodyGyro.Y.mean"
  colnames(extractedDataSet)[17]<-"test.tBodyGyro.Z.mean"
  colnames(extractedDataSet)[18]<-"test.tBodyGyro.X.std"
  colnames(extractedDataSet)[19]<-"test.tBodyGyro.Y.std"
  colnames(extractedDataSet)[20]<-"test.tBodyGyro.Z.std"
  colnames(extractedDataSet)[21]<-"train.tBodyAcc.X.mean"
  colnames(extractedDataSet)[22]<-"train.tBodyAcc.Y.mean"
  colnames(extractedDataSet)[23]<-"train.tBodyAcc.Z.mean"
  colnames(extractedDataSet)[24]<-"train.tBodyAcc.X.std"
  colnames(extractedDataSet)[25]<-"train.tBodyAcc.Y.std"
  colnames(extractedDataSet)[26]<-"train.tBodyAcc.Z.std"
  colnames(extractedDataSet)[27]<-"train.tGravityAcc.X.mean"
  colnames(extractedDataSet)[28]<-"train.tGravityAcc.Y.mean"
  colnames(extractedDataSet)[29]<-"train.tGravityAcc.Z.mean"
  colnames(extractedDataSet)[30]<-"train.tGravityAcc.X.std"
  colnames(extractedDataSet)[31]<-"train.tGravityAcc.Y.std"
  colnames(extractedDataSet)[32]<-"train.tGravityAcc.Z.std"
  colnames(extractedDataSet)[33]<-"train.tBodyGyro.X.mean"
  colnames(extractedDataSet)[34]<-"train.tBodyGyro.Y.mean"
  colnames(extractedDataSet)[35]<-"train.tBodyGyro.Z.mean"
  colnames(extractedDataSet)[36]<-"train.tBodyGyro.X.std"
  colnames(extractedDataSet)[37]<-"train.tBodyGyro.Y.std"
  colnames(extractedDataSet)[38]<-"train.tBodyGyro.Z.std"
  
  ## Create a independent tidy data set with average of each variable for each
  ## activity and each subject
  extractedDataSet2<-melt(extractedDataSet,id=c("Subject.ID","Activity"))
  extractedDataSet3<-dcast(extractedDataSet2,Subject.ID+Activity ~ variable, mean)
  newfile<-file.path("./data","cleanDataSet.txt")
  write.table(extractedDataSet3,clean.file.name,row.names=F,quote=F)
}