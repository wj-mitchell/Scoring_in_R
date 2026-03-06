# Scoring the Difficulty in Emotion Regulation Scale (DERS)
# last updated: Feb. 2025
# author: Billy Mitchell

# PURPOSE: This scale measures difficulties in emotion regulation 

# STRUCTURE: This script assumes that each survey item is a separate column and each subject is a separate row in a dataframe named 'df'. It also assumes that each DERS-related column has 'DERS' somewhere in the header

# For scoring information, see: https://www.researchgate.net/profile/W-Williams/publication/323019722_Interpersonal_Regulation_Questionnaire_DERS/data/5a7cced4a6fdccc013f51ad1/DERS-01-2018.pdf?origin=publication_list

# Identify which columns are DERS responses
DERS_Cols <- grep("DERS", colnames(df))

# Assign a Numeric Value for each DERS Responses
for (i in DERS_Cols){
  for (j in 1:length(rownames(df))){
    if (!is.na(df[i,j]) & df[i,j] == "Almost never (00 - 10 %)")
      df[i,j] <- 1
    if (!is.na(df[i,j]) & df[i,j] == "Sometimes (11 – 35%) ")
      df[i,j] <- 2
    if (!is.na(df[i,j]) & df[i,j] == "About half the time (36 – 65%)")
      df[i,j] <- 3
    if (!is.na(df[i,j]) & df[i,j] == "Most of the time (66 – 90%)")
      df[i,j] <- 4
    if (!is.na(df[i,j]) & df[i,j] == "Almost always (91 – 100%)")
      df[i,j] <- 5
  }
}

# Restructuring DERS Variables as Numeric
for (i in DERS_Cols){
  df[,i] <- as.numeric(df[,i])
}

# Reverse scoring specific items
DERS_Reverse <- c(1,2,6,7,8,10,17,20,22,24,34)
for (i in DERS_Reverse){
  for (j in 1:length(rownames(df))){
    df[j,i] <- sum(6 - df[j,i])
  }  
}

# Calculating DERS composite scores
for (i in 1:length(rownames(df))){
  df$DERS[i] <- sum(df[i, DERS_Cols[c(1:36)]])
  df$DERS_Nonaccept[i] <- sum(df[i, DERS_Cols[c(11, 12, 21, 23, 25, 29)]])
  df$DERS_DiffGoal[i] <- sum(df[i, DERS_Cols[c(13, 18, 20, 26, 33)]])
  df$DERS_Impulse[i] <- sum(df[i, DERS_Cols[c(03, 14, 19, 24, 27, 32)]])
  df$DERS_LackAware[i] <- sum(df[i, DERS_Cols[c(02, 06, 08, 10, 17, 34)]])
  df$DERS_LimitAccess[i] <- sum(df[i, DERS_Cols[c(15, 16, 22, 28, 30, 31, 35,36)]])
  df$DERS_LackClarity[i] <- sum(df[i, DERS_Cols[c(01, 04, 05, 07, 09)]])
}

# Removing DERS We No Longer Need}
rm(DERS_Cols, DERS_Reverse, i, j)