
# Pull original titanic data files and 
# prepare samples for analysis
setwd("/home/a_friend/data_analysis/projects/titanic/")

orig_train <- read.csv("titantic_data/original_files/train.csv")
View(orig_train)
orig_test <- read.csv("titantic_data/original_files/test.csv")
View(orig_test)
gender_sub <- read.csv("titantic_data/original_files/gender_submission.csv")
View(gender_sub)





