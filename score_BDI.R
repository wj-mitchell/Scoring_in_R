# Scoring the Beck Depression Inventory - II Short Version (BDI)
# last updated: Feb. 2025
# author: Billy Mitchell

# PURPOSE: This scale measures depression.

# STRUCTURE: This script assumes that each survey item is a separate column and each subject is a separate row in a dataframe named 'df'. It also assumes that each BDI-related column has the column naming structure listed below (i.e., 'BDI_##')

score_BDI <- function(df, keep_items = F){
  
  # Identify which columns are STAI responses
  BDI_Cols <- grep("BDI", colnames(df))
  
  # Assign a Numeric Value for each BDI Responses
  df$BDI_01[df$BDI_01 == "I do not feel sad."] <- 0
  df$BDI_01[df$BDI_01 == "I feel sad."] <- 1
  df$BDI_01[df$BDI_01 == "I am sad all the time and I can't snap out of it."] <- 2
  df$BDI_01[df$BDI_01 == "I am so sad and unhappy that I can't stand it."] <- 3
  df$BDI_02[df$BDI_02 == "I am not particularly discouraged about the future."] <- 0
  df$BDI_02[df$BDI_02 == "I feel discouraged about the future."]  <- 1
  df$BDI_02[df$BDI_02 == "I feel I have nothing to look forward to."]  <- 2
  df$BDI_02[df$BDI_02 == "I feel the future is hopeless and that things cannot improve."]  <- 3
  df$BDI_03[df$BDI_03 == "I do not feel like a failure."] <- 0
  df$BDI_03[df$BDI_03 == "I feel I have failed more than the average person."]  <- 1
  df$BDI_03[df$BDI_03 == "As I look back on my life, all I can see is a lot of failures."]  <- 2
  df$BDI_03[df$BDI_03 == "I feel I am a complete failure as a person."] <- 3
  df$BDI_04[df$BDI_04 == "I am not particularly dissatisfied."] <- 0
  df$BDI_04[df$BDI_04 == "I don't enjoy things the way I used to."] <- 1
  df$BDI_04[df$BDI_04 == "I don't get satisfaction out of anything anymore."] <- 2
  df$BDI_04[df$BDI_04 == "I am dissatisfied with everything."] <- 3
  df$BDI_05[df$BDI_05 == "I don't feel particularly guilty."] <- 0
  df$BDI_05[df$BDI_05 == "I feel guilty a good part of the time."] <- 1
  df$BDI_05[df$BDI_05 == "I feel quite guilty most of the time."] <- 2
  df$BDI_05[df$BDI_05 == "I feel guilty all of the time."] <- 3
  df$BDI_06[df$BDI_06 == "I don't feel disappointed in myself."] <- 0
  df$BDI_06[df$BDI_06 == "I am disappointed in myself."] <- 1
  df$BDI_06[df$BDI_06 == "I am disgusted with myself."] <- 2
  df$BDI_06[df$BDI_06 == "I hate myself."] <- 3
  df$BDI_07[df$BDI_07 == "I have not lost interest in other people."] <- 0
  df$BDI_07[df$BDI_07 == "I am less interested in other people than I used to be."] <- 1
  df$BDI_07[df$BDI_07 == "I have lost most of my interest in other people."] <- 2
  df$BDI_07[df$BDI_07 == "I have lost all of my interest in other people."] <- 3
  df$BDI_08[df$BDI_08 == "I make decisions about as well as I ever could."] <- 0
  df$BDI_08[df$BDI_08 == "I put off making decisions more than I used to."] <- 1
  df$BDI_08[df$BDI_08 == "I have greater difficulty in making decisions more than I used to."] <- 2
  df$BDI_08[df$BDI_08 == "I can't make any decisions at all anymore."] <- 3
  df$BDI_09[df$BDI_09 == "I don't feel that I look any worse than I used to."] <- 0
  df$BDI_09[df$BDI_09 == "I am worried that I am looking old or unattractive."] <- 1
  df$BDI_09[df$BDI_09 == "I feel that there are permanent changes in my appearance and they make me look unattractive."] <- 2
  df$BDI_09[df$BDI_09 == "I believe that I look ugly."] <- 3
  df$BDI_10[df$BDI_10 == "I can work about as well as before."] <- 0
  df$BDI_10[df$BDI_10 == "It takes extra effort to get started at doing something."] <- 1
  df$BDI_10[df$BDI_10 == "I have to push myself very hard to do anything."] <- 2
  df$BDI_10[df$BDI_10 == "I can't do any work at all."] <- 3
  df$BDI_11[df$BDI_11 == "I don't get more tired than usual."] <- 0
  df$BDI_11[df$BDI_11 == "I get tired more easily than I used to."] <- 1
  df$BDI_11[df$BDI_11 == "I get tired from doing almost anything."] <- 2
  df$BDI_11[df$BDI_11 == "I get too tired to do anything."] <- 3
  df$BDI_12[df$BDI_12 == "My appetite is no worse than usual."] <- 0
  df$BDI_12[df$BDI_12 == "My appetite is not as good as it used to be."] <- 1
  df$BDI_12[df$BDI_12 == "My appetite is much worse now."] <- 2
  df$BDI_12[df$BDI_12 == "I have no appetite at all anymore."] <- 3
  
  # Restructuring STAI Variables as Numeric
  BDI_Cols <- grep("BDI", colnames(df))
  for (i in BDI_Cols){
    df[,i] <- as.numeric(df[,i])
  }
  
  # Calculating BDI composite scores
  df <- df %>%
    mutate(
      BDI = rowSums(across(all_of(BDI_Cols)), na.rm = TRUE)
    )
  
  # If we don't want individual responses
  if (keep_items == F){
    
    # Remove Individual Difference Responses
    df <- df[,-c(BDI_Cols)]
  }
  
  return(df)
}