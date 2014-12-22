run_analysis<- function() {
## This script merges data from a number of .txt files and produces 
## a tidy data set which may be used for further analysis.  Changed 12/20/2014

##check for required packages and provide installation alert when necessary
if (!("reshape2" %in% rownames(installed.packages())) ) {
  print("Please install required package \"reshape2\" before proceeding")
} else {
        ## Open required libraries
        library(reshape2)

        ## 1) Read all required .txt files and label the datasets
        
        ## 2) Read all activities and their names and label the aproppriate columns 
        activity_labels <- read.table("./activity_labels.txt",col.names=c("activity_id","activity_name"))

        ## 3) Read the dataframe's column names
        features <- read.table("features.txt")
        feature_names <-  features[,2]
        
        ## 4) Read the test data and label the dataframe's columns
        testdata <- read.table("./test/X_test.txt")
        colnames(testdata) <- feature_names
        
        ## 5) Read the training data and label the dataframe's columns
        traindata <- read.table("./train/X_train.txt")
        colnames(traindata) <- feature_names
        
        ## 6) Read the ids of the test subjects and label the the dataframe's columns
        test_subject_id <- read.table("./test/subject_test.txt")
        colnames(test_subject_id) <- "subject_id"
        
        ## 7) Read the activity id's of the test data and label the the dataframe's columns
        test_activity_id <- read.table("./test/y_test.txt")
        colnames(test_activity_id) <- "activity_id"
        
        ## 8) Read the ids of the test subjects and label the the dataframe's columns
        train_subject_id <- read.table("./train/subject_train.txt")
        colnames(train_subject_id) <- "subject_id"
        
        ## 9) Read the activity id's of the training data and label the dataframe's columns
        train_activity_id <- read.table("./train/y_train.txt")
        colnames(train_activity_id) <- "activity_id"
        
        ## 10) Merge the test subject id's, the test activity id's and the test data into one dataframe
        test_data <- cbind(test_subject_id , test_activity_id , testdata)
        
        ## 11) Merge the test subject id's, the test activity id's and the test data into one dataframe
        train_data <- cbind(train_subject_id , train_activity_id , traindata)
        
        ## 12) Merge the test data and the train data into one dataframe
        all_data <- rbind(train_data,test_data)
        
        ## Retain the columns for the mean() and std() values
        mean_col_idx <- grep("mean",names(all_data),ignore.case=TRUE)
        mean_col_names <- names(all_data)[mean_col_idx]
        std_col_idx <- grep("std",names(all_data),ignore.case=TRUE)
        std_col_names <- names(all_data)[std_col_idx]
        meanstddata <-all_data[,c("subject_id","activity_id",mean_col_names,std_col_names)]
        
        ## 13) Merge the activities file with the mean/std values file to get one file with descriptive activity names
        descrnames <- merge(activity_labels,meanstddata,by.x="activity_id",by.y="activity_id",all=TRUE)
        
        ## 14) Melt the dataset with the descriptive activity names for better handling
        data_melt <- melt(descrnames,id=c("activity_id","activity_name","subject_id"))
        
        ## 15) Cast the melted dataset according to the average of each variable for each activity and each subject
        mean_data <- dcast(data_melt,activity_id + activity_name + subject_id ~ variable,mean)
       
        ## 16) Create a file with the new tidy dataset
        write.table(mean_data,"./tidy_movement_data.txt")

}
}
## Change 2