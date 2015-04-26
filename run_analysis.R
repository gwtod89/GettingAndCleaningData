## Getting and Cleaning Data Course Project
#	  Instructions: 
#     You should create one R script called run_analysis.R that does the following. 
#     Merges the training and the test sets to create one data set.
#     Extracts only the measurements on the mean and standard deviation for each measurement. 
#     Uses descriptive activity names to name the activities in the data set
#     Appropriately labels the data set with descriptive variable names. 
#     From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


## STEP 1
# Read all files from the corresponding folders, assuming the working 
# directory is set to the default unzipped folder. 
trainingData<-read.table("train/subject_train.txt")
testingData<-read.table("test/Subject_test.txt")
trainingLabel<-read.table("train/y_train.txt")
testingLabel<-read.table("test/y_test.txt")
xTraining<-read.table("train/X_train.txt")
xTesting<-read.table("test/X_test.txt")

# Basic combinations of the data files 
datComp <- rbind(trainingData,testingData) ##combine train and test subject files
labComp<-rbind(trainingLabel,testingLabel) ##combine train and test activity files
xComp<-rbind(xTraining,xTesting) ##combine train and test measurement files

## STEP 2 
# read the features and select only the ones using STD and MEAN from 
# the entire list. Grep used to find the matches. Relevant features 
# are stored using logical indexing. 
featuresList <-read.table("features.txt")
selecFeatures<-grep("mean\\(\\)|std\\(\\)",featuresList[,2]) 
xComp<-xComp[,selecFeatures]  
## STEP 3 
# activities are read from the  file and some minor modifications are done. 
actList<-read.table("activity_labels.txt")
actList[,2]<-tolower(gsub("_","",actList[,2])) 
substr(actList[2, 2], 8, 8) <- toupper(substr(actList[2, 2], 8, 8)) 
substr(actList[3, 2], 8, 8) <- toupper(substr(actList[3, 2], 8, 8))
activityLabel <- actList[labComp[,1],2]
labComp[,1]<-activityLabel 


## STEP 4 
# Change the names of features and add them from the list in previous set. 
names(labComp) <- "Activity" 
names(datComp) <-"SubjectID" 
colnames(xComp) = featuresList[selecFeatures,2]

## STEP 5 
# Use the logical indexing of activity and subject, finds means, and saves 
# to predetermined data frame. 
firstComp<-cbind(datComp,xComp,labComp)
lengthS <- length(table(datComp)) 
lengthA <- dim(actList)[1] 
lengthC <- dim(firstComp)[2]
meanComp <- as.data.frame(matrix(NA, nrow=lengthS*lengthA, ncol=lengthC)) 
colnames(meanComp) <- colnames(firstComp)
tempR <- 1
for(i in 1:lengthS) { 
        for(j in 1:lengthA) {
                meanComp[tempR, 1] <- sort(unique(datComp)[, 1])[i]
                meanComp[tempR, 2] <- actList[j, 2]
                bool1 <- i == firstComp$SubjectID
                bool2 <- actList[j, 2] == firstComp$Activity
                meanComp[tempR, 3:lengthC-1] <- colMeans(firstComp[bool1&bool2, 3:lengthC-1])
                tempR <- tempR + 1
        } 
}
# Basic reordering to facilitate reading and output to a file. 
meanComp <- meanComp[c(1,68,2:67)]
meanComp$Activity <- actList[c(5,4,6,1,3,2),2]
write.table(meanComp, "meansTable.txt")