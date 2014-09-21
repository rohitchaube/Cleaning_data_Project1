##########################################
## Course - Getting and Cleaning Data
## Project 1 
## Author - Rohit Chaube 
##########################################

run_analysis <- function() { 
   
  ## Define variables 
  read_x_test_data  <- data.frame(matrix(,,ncol=561))
  read_x_train_data <- data.frame(matrix(,,ncol=561))
  combine_data  	  <- data.frame(matrix(,,ncol=561))
  mean_table  	  <- data.frame(matrix(,,ncol=561))



  ## Step 1 - Reading Data  
  ## Reading Test Data
  read_x_test_data <- read.table("./UCI HAR Dataset/test/X_test.txt")

  ## Reading Train Data 
  read_x_train_data <- read.table("./UCI HAR Dataset/train/X_train.txt")

  ## Combine data from train_data and test_data
  combine_data <- rbind(train_data, test_data)

  ##return(head(combine_data, n=2))  -- TEST complete 



  ## Renaming combine_data column names from features.txt file 
  read_features_data <- read.table("./UCI HAR Dataset/features.txt")

  ## Save Features data in feature_vec vector 
  feature_vec <- read_features_data[,2]
  
  ## Update the all_data column names with the feature_vec 
  colnames(combine_data) <- feature_vec

  ## Step 2 - Extract Only the measurements on the mean and standard deviation for each measurement 
  ## Extracting - Mean 
  subset_mean_data <- combine_data[, grep("mean", colnames(combine_data), value=TRUE)]

  ## Extracting - Standard Deviation 
  subset_std_data <- combine_data[, grep("std", colnames(combine_data), value=TRUE)]

  
  ## Combining - Mean and Standard Deviation 
  combine_mean_std_data <- cbind(subset_mean_data, subset_std_data)

  ## return(dim(combine_mean_std_data))  -- TEST complete 


  ## Add Subject and Activities columns to the combine - mean and std data 
  read_activities_test_data <- read.table("./UCI HAR Dataset/test/y_test.txt")
  read_subject_test_data <- read.table("./UCI HAR Dataset/test/subject_test.txt")

  read_activities_train_data <- read.table("./UCI HAR Dataset/train/y_train.txt")
  read_subject_train_data <- read.table("./UCI HAR Dataset/train/subject_train.txt")


   ## combine Data   
   act_sub_test_data  <- cbind(read_activities_test_data, read_subject_test_data)
   act_sub_train_data <- cbind(read_activities_train_data, read_subject_train_data)

   act_sub_combine_data <- rbind(act_sub_test_data, act_sub_train_data) 

   ## return(dim(act_sub_combine_data))  -- TEST complete 

   ## Adding Activities and Subject columns to - Mean and Standard Deviation data  
   combine_mean_std_data <- cbind(combine_mean_std_data, act_sub_combine_data)

   
   ## Update columns 562 and 563 with values  
   names(combine_mean_std_data)[80]<-"Activity"
   names(combine_mean_std_data)[81]<-"Subject"

   ## return(names(combine_mean_std_data))  
   ## return(tail(combine_mean_std_data, n=1))  -- TEST Complete 



   ## Uses descriptive activity names to name the activities in the data set
   combine_mean_std_data$Activity[combine_mean_std_data$Activity=="1"] <- "WALKING"
   combine_mean_std_data$Activity[combine_mean_std_data$Activity=="2"] <- "WALKING_UPSTAIRS"
   combine_mean_std_data$Activity[combine_mean_std_data$Activity=="3"] <- "WALKING_DOWNSTAIRS"
   combine_mean_std_data$Activity[combine_mean_std_data$Activity=="4"] <- "SITTING"
   combine_mean_std_data$Activity[combine_mean_std_data$Activity=="5"] <- "STANDING"
   combine_mean_std_data$Activity[combine_mean_std_data$Activity=="6"] <- "LAYING" 

   ## return(combine_mean_std_data[,80])  ## -- TEST Complete 



   ## Step 5 - independent tidy data set with the average of each variable for each activity and each subject.
   no_cols <- ncol(combine_mean_std_data) 

   for(i in 1:(no_cols-2)) {
    col_name <- colnames(combine_mean_std_data[i])
	# Following generates Average Ass desired 
	mean_var$col_name <- ddply(combine_mean_std_data, c("Subject", "Activity"), function(df) mean(df[,i]))
	
      if (i ==1) {
	   mean_table <- mean_var$col_name
	   col_name <- paste ("avg_",col_name)
	   names(mean_table)[3]<-col_name
	} else {
	   ext_data <- mean_var$col_name[,3]	
	   mean_table <- cbind(mean_table, ext_data)
	   col_name <- paste ("avg_",col_name)
	   names(mean_table)[i+2]<-col_name
	}
  
   }

   
   write.table(mean_table, file = "./proj1_tidy_data.txt", sep = ",", row.name=FALSE)
   return(head(mean_table, n=10))
   

}