#You should create one R script called run_analysis.R that does the following.

#Merges the training and the test sets to create one data set.
#Extracts only the measurements on the mean and standard deviation for each measurement.
#Uses descriptive activity names to name the activities in the data set
#Appropriately labels the data set with descriptive variable names.
#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.




# Download and unzip the data set 
DownloadDataSet = function(url) {
  if (!file.exists("data")) {
    #  Check for existing data directory or create it
    message("create data folder...")
    dir.create("data")
  }
  if (!file.exists("data/UCI HAR Dataset")) {

    zipfile="data/UCI_HAR_data.zip"
    message("data download...")
    download.file(url, destfile=zipfile, method="auto")
    unzip(zipfile, exdir="data")
  }
}



# load the train and test files 
LoadMergeData = function() {
  message("load data...")

  path<<-paste(getwd(),"/data/UCI HAR Dataset/", sep = "")
  

  message("  read X_train.txt...")
  train.dat = read.csv(paste(path,"train/X_train.txt",sep=""), sep="", header=FALSE)
  

  message("  read Y_train.txt...")
  train.dat[,ncol(train.dat)+1] = read.csv(paste(path,"train/Y_train.txt",sep=""), sep="", header=FALSE)
  

  message("  read subject_train.txt...")
  train.dat[,ncol(train.dat)+1] = read.csv(paste(path,"train/subject_train.txt",sep=""), sep="", header=FALSE)
  

  message("  read X_test.txt...")
  test.dat = read.csv(paste(path,"test/X_test.txt",sep=""), sep="", header=FALSE)
  

  message("  read Y_test.txt...")
  test.dat[,ncol(test.dat)+1] = read.csv(paste(path,"test/Y_test.txt",sep=""), sep="", header=FALSE)
  

  message("  read subject_test.txt...")
  test.dat[,ncol(test.dat)+1] = read.csv(paste(path,"test/subject_test.txt",sep=""), sep="", header=FALSE)
  
  
  # Merges the training and the test sets to create one data set.
  message("merge data...")
  rbind(train.dat, test.dat)
}





ExtractData=function(df){
  # Extracts only the measurements on the mean and standard deviation for each measurement. 
  message("extract data...")

  features <- read.csv(paste(path,"features.txt", sep=""), sep="", header=FALSE)
  

  cols.in.scope <<- grep(".*-mean.*|.*-std.*", features[,2])
  
  
  features <<- features[cols.in.scope,]
  
  var.count = ncol(df)

  cols.in.scope <<- c(cols.in.scope, var.count-1, var.count)
  

  df<-df[,cols.in.scope]
  df
}




# Uses descriptive activity names to name the activities in the data set
SetActivityNames = function(df){
  message("set activity labels...")
 
  activity.Labels = read.csv(paste(path,"activity_labels.txt", sep=""), sep="", header=FALSE)
  

  activity.ID = 1
  for (ActivityLabel in activity.Labels$V2) {
    df$activity <- gsub(activity.ID, ActivityLabel, df$activity)
    activity.ID <- activity.ID + 1
  }
  
  df
}




# Appropriately labels the data set with descriptive variable names following Google's Rguide 
DescriptiveVariables = function(df){
  message("make descriptive variable names...")

  features[,2] <- gsub('-meanFreq()', '.mean.freq', features[,2]) # substitutes "-meanFreq()" with ".mean.freq"
  features[,2] <- gsub('-mean()', '.mean', features[,2]) # substitutes "-mean" with ".mean"
  features[,2] <- gsub('-std()', '.std', features[,2]) # substitutes "-std" with ".std"
  features[,2] <- gsub('[-]', '.', features[,2]) # substitutes "-" with "."
  features[,2] <- gsub('[()]', '', features[,2]) # removes "()"
  

  colnames(df) <- c(features$V2, "Activity", "Subject")

  colnames(df) <- tolower(colnames(df))
  
  df
}

# Creates a second, independent DF tidy.data with the mean of each variable for each activity and each subject. 
MakeTidy = function(df){
  message("tidy data...")

  df$activity <- as.factor(df$activity)
  df$subject <- as.factor(df$subject)
  

  countnndc = ncol(df)-2 
  nndc = c(1:countnndc) 
  

  tidy <- aggregate(df[,nndc], by=list(activity = df$activity, subject=df$subject), mean, na.rm=TRUE)
  tidy
}



DownloadDataSet("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip")

Data <- LoadMergeData()

Data <- ExtractData(Data)

Data <- DescriptiveVariables(Data)

Data <- SetActivityNames(Data) 

Tidy.Data <- MakeTidy(Data)

write.table(Tidy.Data, "tidy.txt", sep="\t",row.names = F)

write(names(Data), file = "variables.txt", ncolumns = 1)
