library(dplyr)
testX <- read.delim("UCI HAR Dataset/test/X_test.txt", sep="", header = FALSE)
testY <- read.delim("UCI HAR Dataset/test/Y_test.txt", sep="", header = FALSE)
testSub <- read.delim("UCI HAR Dataset/test/subject_test.txt", sep="", header = FALSE)
trainX <- read.delim("UCI HAR Dataset/train/X_train.txt", sep="", header = FALSE)
trainY <- read.delim("UCI HAR Dataset/train/Y_train.txt", sep="", header = FALSE)
trainSub <- read.delim("UCI HAR Dataset/train/subject_train.txt", sep="", header = FALSE)
activityLabels <- read.delim("UCI HAR Dataset/activity_labels.txt", sep="", header = FALSE)
features <- read.delim("UCI HAR Dataset/features.txt", sep="", header=FALSE)

#merge training set and test set
observationsX <- rbind (trainX,testX)
#prepare to have mean and std
meanIndex <- grep("[mM]ean", features$V2)
meanNames <- grep("[mM]ean", features$V2, value=TRUE)
stdIndex <- grep ("std", features$V2)
stdNames <- grep ("std", features$V2, value=TRUE)
indecies <- c(meanIndex, stdIndex)
variableNames <- c(meanNames, stdNames)
outputX <- observationsX[indecies]
colnames(outputX) <- variableNames
# we now have mean and std in outputX, with correct variable names

act <- rbind(trainY, testY)
Activity <- activityLabels$V2[act$V1]
subjects <- rbind (trainSub, testSub)
colnames(subjects) <- "Subject"
output <- cbind (outputX, Activity, subjects)

outBySubj <- split(output, output$Subject)
allSubjects <- unique(subjects)
finalAnswer <- data.frame()
for (s in allSubjects[[1]]){
  print (s)
  outbyActivity <- split(outBySubj[[s]], outBySubj[[s]]$Activity)
  for (a in unique(Activity))
  {
    vec <- select(outbyActivity[[a]], -Activity, -Subject)
    answer <- lapply(vec,mean)
    answer$Activity <- a
    answer$Subject <- s
    aDF <- as.data.frame(answer)
    finalAnswer <- rbind(aDF,finalAnswer)
  }
}
finalAnswer <- arrange(finalAnswer, Subject, Activity)
write.table(finalAnswer, file = "file.txt", row.name=FALSE)
