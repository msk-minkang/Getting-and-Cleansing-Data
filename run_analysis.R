
#main_dir <- "Project/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/"

run_analysis <- function(main_dir) {
  
  
  main_dir <- gsub("\\/$", "", main_dir)
  
  ### File Names
  filename_test <- paste(main_dir, "/test/X_test.txt", sep = "")
  filename_test_lab <- paste(main_dir, "/test/y_test.txt", sep = "")
  filename_test_sub <- paste(main_dir, "/test/subject_test.txt", sep = "")
  
  filename_train <- paste(main_dir, "/train/X_train.txt", sep = "")
  filename_train_lab <- paste(main_dir, "/train/y_train.txt", sep = "")
  filename_train_sub <- paste(main_dir, "/train/subject_train.txt", sep = "")
  
  filename_features <- paste(main_dir, "/features.txt", sep = "")
  filename_activity_lab <- paste(main_dir, "/activity_labels.txt", sep = "")
  
  
  
  ### Import Txt files
  test <- read.table(file = filename_test)
  test_lab <- read.table(file = filename_test_lab)
  test_sub <- read.table(file = filename_test_sub)
  
  train <- read.table(file = filename_train)
  train_lab <- read.table(file = filename_train_lab)
  train_sub <- read.table(file = filename_train_sub)
  
  
  features <- read.table(file = filename_features)
  activity_lab <- read.table(file = filename_activity_lab)
  
  ### Give Proper name
  names(test) <- features$V2
  names(train) <- features$V2
  
  ### Assign Activity Code
  test$activity_cd <- test_lab$V1
  train$activity_cd <- train_lab$V1
  
  ### Get Subject
  test$subject <- test_sub$V1
  train$subject <- train_sub$V1
  
  merged_df <- rbind(test, train)
  merged_df$activity_lab <- activity_lab[match(merged_df$activity_cd, activity_lab$V1), "V2"]
  
  ### Only extract coluns with mean() and std()
  extract_cols <- names(merged_df)[grepl("(mean|std)\\(\\)", names(merged_df))]
  extract_df <- merged_df[, c("subject", "activity_lab", extract_cols)]
  
  ### Take Average by Subject by Activity
  avg_by_sub_act <- aggregate(. ~ subject + activity_lab, extract_df, mean)
  
  return(avg_by_sub_act)
  
  
}

