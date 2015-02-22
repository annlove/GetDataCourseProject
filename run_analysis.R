## The script run_analysis.R does the following. 
## Merges the training and the test sets to create one data set.
## Extracts only the measurements on the mean and standard deviation for each measurement. 
## Uses descriptive activity names to name the activities in the data set
## Appropriately labels the data set with descriptive variable names. 
## From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

run_analysis <- function() {
	## install.packages("dplyr")
	test_s<- read.table("test/subject_test.txt")
	test_x <- read.table("test/X_test.txt")
	test_y <- read.table("test/y_test.txt")
	train_s<- read.table("train/subject_train.txt")
	train_x <- read.table("train/X_train.txt")
	train_y <- read.table("train/y_train.txt")

	## Merges the training and the test sets to create one data set.
	m_test <- cbind(test_y, test_s)
	m_test <- cbind(m_test,test_x)
	
	m_train <- cbind(train_y,train_s)
	m_train <- cbind(m_train,train_x)
	m <- rbind(m_test,m_train)
	
	## Appropriately labels the data set with descriptive variable names. 
	f <- read.table("features.txt")
	names(m) <- c("activity","subject", as.character(f$V2))
	
	## Uses descriptive activity names to name the activities in the data set
	a <- read.table("activity_labels.txt")
	m$activity <- a[match(m$activity,a$V1),2]
	
	## Extracts only the measurements on the mean and standard deviation for each measurement.
	r <- m[, grep('activity|mean()|std()', names(m))]
	
	## From the data set in step 4, creates a second, independent tidy data set with the average of each variable 	for each activity and each subject.
	s <- m[, grep('activity|subject|mean()|std()', names(m))]
	y<- group_by(s, activity, subject)
	z<- summarise_each(y, funs(mean))
	write.table(z,"result.txt", row.names=FALSE)
}
