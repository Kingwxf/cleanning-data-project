# Read data
  data.path<- file.path(getwd(), "UCI HAR Dataset")
  library(data.table)
  features<- fread(file = file.path(data.path, "features.txt"))
  x_test<- fread(file = file.path(data.path, "test/x_test.txt"))
  y_test<- fread(file = file.path(data.path, "test/y_test.txt"))  
  subject_test<- fread(file = file.path(data.path, "test/subject_test.txt"))
  x_train<- fread(file = file.path(data.path, "train/x_train.txt"))
  y_train<- fread(file = file.path(data.path, "train/y_train.txt"))  
  subject_train<- fread(file = file.path(data.path, "train/subject_train.txt"))
  
## 1. Merges the training and the test sets to create one data set
   # merge the training set
   train_set <- cbind(subject_train, y_train, x_train)
   # merge the test set
   test_set <- cbind(subject_test, y_test, x_test)
   # combine into one set named data.merged
   data.merged<- rbind(train_set, test_set)   
   
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
   # take out the column name from features dataset
   fea.col<- as.character(features$V2)
   # search "mean" "std" pattern and subject/ activity infomation in column names
   mean_std_index <- c(TRUE, TRUE, grepl("mean" , fea.col) | grepl("std" , fea.col))
   # subset the measurements according to the index
   data.merged<- as.data.frame(data.merged)
   mean_std_set <- data.merged[,mean_std_index]
   test<- mean_std_set [1:10, 1:10]
   
## 3. Uses descriptive activity names to name the activities in the data set
   # read in the activity labels and names the colname
   activity_labels <- read.table(file = file.path(data.path, "activity_labels.txt"))
   colnames(activity_labels) <- c("activityID", "labels")
   colnames(mean_std_set)[1:2] <- c("subjectID", "activityID")   
   # merge the labels file and extracted dataset
   actnames.set<- merge(mean_std_set, activity_labels, by="activityID")
   # a variable named 'labels' is used to describe the activities
   
## 4. Label the data set with decriptive variable names
   # look at the present colnames
   names(actnames.set)
   # extract the subset of names containing 'mean' and 'std' from fea.col to name the columns
   colnames(actnames.set)[3:81]<- fea.col[grepl("mean" , fea.col) | grepl("std" , fea.col)]
   # check the colnames
   names(actnames.set)
   
## 5. From the data set in step 4, creates a second, independent tidy data set with the average
##    of each variable for each activity and each subject.
   final.set <- aggregate(actnames.set, by= list(actnames.set$activityID, actnames.set$subjectID, actnames.set$labels), FUN = mean)
   # remove the extra columns
   final.set<- final.set[,3:84]
   colnames(final.set)[1] = "activityLabels"
   # arrange the cleaned dataset
   library(dplyr)
   final.set<- final.set %>% arrange(activityID, subjectID)

## Save the tidy data
   write.table(final.set, file = "tidydata.txt", row.names = FALSE)
   