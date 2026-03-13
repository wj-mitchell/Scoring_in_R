# Scoring the Interpersonal Reactivity Index (IRI)
# last updated: Feb. 2025
# author: Billy Mitchell

# PURPOSE: This scale measures individual differences in empathy across four composite measures: Perspective Taking, Fantasy, Empathic Concern, and Personal Distress.

# STRUCTURE: This script assumes that each survey item is a separate column and each subject is a separate row in a dataframe named 'df'. It also assumes that each IRI-related column has 'IRI_' somewhere in the header

# For scoring information, see original Interpersonal Reactivity Index materials

# Four sub-scales that are each made up of 7 different items. 
# 1. Perspective taking (PT) - the tendency to spontaneously adopt the psychological point of view of others [3 8 11 15 21 25 28] 
# 2. Fantasy (FS) - taps respondents' tendencies to transpose themselves imaginatively into the feelings and actions of fictitious characters in books, movies, and plays [1 5 7 12 16 23 26] 
# 3. Empathetic Concern (EC) - assesses "other-oriented" feelings of sympathy and concern for unfortunate others [2 4 9 14 18 20 22] 
# 4. Personal Distress (PD) - measures "self-oriented" feelings of personal anxiety and unease in tense interpersonal settings [6 10 13 17 19 24 27]

# This script makes the following assumptions about your data structure: 
# 1) Your data frame is called iri. 
# 2) You have separate columns for each IRI questions (e.g., IRI_1, IRI_2, etc.) 

score_IRI <- function(df, keep_items = F) {
  
  # Identify which columns are IRI responses
  IRI_Cols <- grep("^IRI_", colnames(df))
  
  # Assign a Numeric Value for each IRI Response
  for (i in IRI_Cols){
    for (j in 1:length(rownames(df))){
      if (df[j,i] == "A - Does not describe me well" & !is.na(df[j,i]))
        df[j,i] <- 0
      if (df[j,i] == "B" & !is.na(df[j,i]))
        df[j,i] <- 1
      if (df[j,i] == "C" & !is.na(df[j,i]))
        df[j,i] <- 2
      if (df[j,i] == "D" & !is.na(df[j,i]))
        df[j,i] <- 3
      if (df[j,i] == "E - Describes very well" & !is.na(df[j,i]))
        df[j,i] <- 4
    }
  }
  
  # Restructuring IRI Variables as Numeric
  for (i in IRI_Cols){
    df[,i] <- as.numeric(df[,i])
  }
  
  # Identify reverse-scored items
  IRI_Reverse_items <- c(3, 4, 7, 12, 13, 14, 15, 18, 19)
  IRI_Cols_Reverse <- IRI_Cols[IRI_Reverse_items]
  
  # Reverse score designated IRI items
  for (i in IRI_Cols_Reverse){
    for (j in 1:length(rownames(df))){
      if (df[j,i] == 0 & !is.na(df[j,i]))
        df[j,i] <- 4
      else if (df[j,i] == 1 & !is.na(df[j,i]))
        df[j,i] <- 3
      else if (df[j,i] == 2 & !is.na(df[j,i]))
        df[j,i] <- 2
      else if (df[j,i] == 3 & !is.na(df[j,i]))
        df[j,i] <- 1
      else if (df[j,i] == 4 & !is.na(df[j,i]))
        df[j,i] <- 0
    }
  }
  
  # Identify subscales
  IRI_PT_items <- c(3, 8, 11, 15, 21, 25, 28)
  IRI_FS_items <- c(1, 5, 7, 12, 16, 23, 26)
  IRI_EC_items <- c(2, 4, 9, 14, 18, 20, 22)
  IRI_PD_items <- c(6, 10, 13, 17, 19, 24, 27)
  
  IRI_Cols_PT <- IRI_Cols[IRI_PT_items]
  IRI_Cols_FS <- IRI_Cols[IRI_FS_items]
  IRI_Cols_EC <- IRI_Cols[IRI_EC_items]
  IRI_Cols_PD <- IRI_Cols[IRI_PD_items]
  
  # Score IRI
  df <- df %>%
    mutate(
      IRI_PT = rowSums(select(., all_of(IRI_Cols_PT)), na.rm = TRUE),
      IRI_FS = rowSums(select(., all_of(IRI_Cols_FS)), na.rm = TRUE),
      IRI_EC = rowSums(select(., all_of(IRI_Cols_EC)), na.rm = TRUE),
      IRI_PD = rowSums(select(., all_of(IRI_Cols_PD)), na.rm = TRUE)
    )
  
  # If we don't want individual responses
  if (keep_items == F){
    
    # Remove Individual Difference Responses
    df <- df[,-c(IRI_Cols)]
  }
  
  return(df)
}