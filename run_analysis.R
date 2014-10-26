library(reshape)
tidyUCIHAR <- function(dir, outputFile) {
  
  # declare meaningful feature names
  featureNames <- c("Time.Body.Acc.Mean.X", "Time.Body.Acc.Mean.Y", "Time.Body.Acc.Mean.Z", "Time.Body.Acc.Std.X", "Time.Body.Acc.Std.Y", "Time.Body.Acc.Std.Z",  
                    "Time.Gravity.Acc.Mean.X", "Time.Gravity.Acc.Mean.Y", "Time.Gravity.Acc.Mean.Z", "Time.Gravity.Acc.Std.X", "Time.Gravity.Acc.Std.Y", "Time.Gravity.Acc.Std.Z", 
                    "Time.Body.Acc.Jerk.Mean.X", "Time.Body.Acc.Jerk.Mean.Y", "Time.Body.Acc.Jerk.Mean.Z", "Time.Body.Acc.Jerk.Std.X", "Time.Body.Acc.Jerk.Std.Y", "Time.Body.Acc.Jerk.Std.Z", 
                    "Time.Body.Gyro.Mean.X", "Time.Body.Gyro.Mean.Y", "Time.Body.Gyro.Mean.Z", "Time.Body.Gyro.Std.X", "Time.Body.Gyro.Std.Y", "Time.Body.Gyro.Std.Z", 
                    "Time.Body.Gyro.Jerk.Mean.X", "Time.Body.Gyro.Jerk.Mean.Y", "Time.Body.Gyro.Jerk.Mean.Z", "Time.Body.Gyro.Jerk.Std.X", "Time.Body.Gyro.Jerk.Std.Y", "Time.Body.Gyro.Jerk.Std.Z", 
                    "Time.Body.Acc.Mag.Mean", "Time.Body.Acc.Mag.Std", 
                    "Time.Gravity.Acc.Mag.Mean", "Time.Gravity.Acc.Mag.Std", 
                    "Time.Body.Acc.Jerk.Mag.Mean", "Time.Body.Acc.Jerk.Mag.Std", 
                    "Time.Body.Gyro.Mag.Mean", "Time.Body.Gyro.Mag.Std", 
                    "Time.Body.Gyro.Jerk.Mag.Mean", "Time.Body.Gyro.Jerk.Mag.Std", 
                    "Freq.Body.Acc.Mean.X", "Freq.Body.Acc.Mean.Y", "Freq.Body.Acc.Mean.Z", "Freq.Body.Acc.Std.X", "Freq.Body.Acc.Std.Y", "Freq.Body.Acc.Std.Z",  
                    "Freq.Body.Acc.Jerk.Mean.X", "Freq.Body.Acc.Jerk.Mean.Y", "Freq.Body.Acc.Jerk.Mean.Z", "Freq.Body.Acc.Jerk.Std.X", "Freq.Body.Acc.Jerk.Std.Y", "Freq.Body.Acc.Jerk.Std.Z", 
                    "Freq.Body.Gyro.Mean.X", "Freq.Body.Gyro.Mean.Y", "Freq.Body.Gyro.Mean.Z", "Freq.Body.Gyro.Std.X", "Freq.Body.Gyro.Std.Y", "Freq.Body.Gyro.Std.Z", 
                    "Freq.Body.Acc.Mag.Mean", "Freq.Body.Acc.Mag.Std", 
                    "Freq.Body.Acc.Jerk.Mag.Mean", "Freq.Body.Acc.Jerk.Mag.Std", 
                    "Freq.Body.Gyro.Mag.Mean", "Freq.Body.Gyro.Mag.Std", 
                    "Freq.Body.Gyro.Jerk.Mag.Mean", "Freq.Body.Gyro.Jerk.Mag.Std")
  
  # get required features
  fReqLog <- grepl(".*mean\\(.*|.*std\\(.*", features$Measurement.Name)
  fReq <- features[fReqLog,]
  
  # create colClass vector to read only required features
  detColClass <- function(bool) { ifelse(bool, "numeric", "NULL") }
  fReqClasses <- sapply(fReqLog, detColClass)
  
  # read all files needed
  testVals <- read.table(paste0(dir,"/test/X_test.txt"), colClasses=fReqClasses, comment.char="")
  testType <- read.table(paste0(dir,"/test/y_test.txt"), colClasses=c("numeric"), comment.char="", col.names="Activity")
  testSubj <- read.table(paste0(dir,"/test/subject_test.txt"), colClasses=c("numeric"), comment.char="", col.names="Subject")
  
  trainVals <- read.table(paste0(dir,"/train/X_train.txt"), colClasses=fReqClasses, comment.char="")
  trainType <- read.table(paste0(dir,"/train/y_train.txt"), colClasses=c("numeric"), comment.char="", col.names="Activity")
  trainSubj <- read.table(paste0(dir,"/train/subject_train.txt"), colClasses=c("numeric"), comment.char="", col.names="Subject")
  
  features <- read.table(paste0(dir,"/features.txt"), colClasses=c("numeric", "character"), comment.char="", sep=" ", col.names=c("Measurement.ID","Measurement.Name"))
  activities <- read.table(paste0(dir,"/activity_labels.txt"), colClasses=c("numeric", "character"), comment.char="", sep=" ", col.names=c("Activity.ID","Activity.Name"))
  
  # rename to meaningful feature and activity names
  names(testVals) <- featureNames
  names(trainVals) <-featureNames
  
  # merge and order data
  combinedVals <- rbind(testVals, trainVals)
  combinedType <- rbind(testType, trainType)
  combinedSubj <- rbind(testSubj, trainSubj)
  
  combined <- cbind(combinedSubj, combinedType, combinedVals)
  
  # rename to meaningful activity names
  combined$Activity <- as.factor(combined$Activity)
  levels(combined$Activity) <- activities$Activity.Name
  
  # create averages
  melted <- melt(combined, id=names(combined)[1:2], measure.vars=names(combined)[3:68])
  output <- cast(melted, fun.aggregate=mean)
  
  # output
  write.table(output, file=outputFile,row.name=FALSE, quote=FALSE)
}