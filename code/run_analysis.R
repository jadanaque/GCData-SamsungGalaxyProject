# Merge X_train and X_test. First read data into R, add the variables names (features.txt), and then row bind X_train and X_test.
trainDF <- read.table("./data/train/X_train.txt")
testDF <- read.table("./data/test/X_test.txt")
varNames <- readLines("./data/features.txt")

ncol(trainDF) == length(varNames)
ncol(testDF)  == length(varNames)

names(trainDF) <- varNames
names(testDF) <- varNames

fullDF <- rbind(trainDF, testDF)

# Extract only the measurements on the mean and standard deviation (std) for each measurement. Use 'grep()' and this expression "mean\\(|std\\("
fullDFsubset <- fullDF[, grep("mean\\(|std\\(", names(fullDF))]

# Read subject_train and subject_test using 'readLines()' and bind them (using 'c()') into one vector, then, cbind this vector with the previous DF. 
subjectTrain <- readLines("./data/train/subject_train.txt")
subjectTest <- readLines("./data/test/subject_test.txt")
subject <- c(subjectTrain, subjectTest)

fullDFsubset <- cbind(subject, fullDFsubset)

# Read y_train and y_test. Bind y_train and y_test into one vector with a descriptive name. Then, coerce it to a factor using 'factor()' and its arguments levels and labels (use "activity_labels.txt" for this purpose)
trainActivity <- readLines("./data/train/y_train.txt")
testActivity <- readLines("./data/test/y_test.txt")
activity <- c(trainActivity, testActivity)

activityLabels <- readLines("./data/activity_labels.txt")
activityLabels <- tolower(gsub("[^A-Z]", "", activityLabels))

activity <- factor(activity, labels = activityLabels)  ## I could add the argument 'levels = 1:6', but it's not necessary because the values of 'activity' are in ascending order

# Cbind this vector (activity) with the data frame you created in the previous step.
fullDFsubset <- cbind(activity, fullDFsubset)

# Clean the variable names
names(fullDFsubset)
names(fullDFsubset) <- gsub("[^a-zA-Z]", "", names(fullDFsubset))
names(fullDFsubset) <- gsub("mean", "Mean", names(fullDFsubset))
names(fullDFsubset) <- gsub("std", "Std", names(fullDFsubset))
names(fullDFsubset) <- gsub("BodyBody", "Body", names(fullDFsubset))

# Creating a tidy data set with the average of each variable for each activity and each subject.
library(reshape2)
fullDFsubsetMelt <- melt(fullDFsubset, id.vars=c("subject", "activity"))
averageBySubjectActivity <- dcast(fullDFsubsetMelt, subject + activity ~ variable, mean, na.rm=T, value.var="value")

dim(averageBySubjectActivity)

# Just testing other ways to compute the same
library(dplyr)
averageDFtest <- summarize(group_by(fullDFsubset, subject, activity), tBodyAccMeanX=mean(tBodyAccMeanX, na.rm=T), tBodyAccMeanY=mean(tBodyAccMeanY, na.rm=T))  # I would need insert the name-value pair for each variable
head(averageDFtest, 10)

# Writing the resulting data set into a text file
write.table(averageBySubjectActivity, "./output/averageBySubjectActivity.txt", row.name=F)
