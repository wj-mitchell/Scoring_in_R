# Scoring the Interpersonal Regulation Questionnaire (IRQ)
# last updated: Feb. 2025
# author: Billy Mitchell

# PURPOSE: This scale measures interpersonal regulation on four dimensions: positive (P) and negative (N) efficacy (E) and tendency (T) 

# STRUCTURE: This script assumes that each survey item is a separate column and each subject is a separate row in a dataframe named 'df'. It also assumes that each IRQ-related column has 'IRQ' somewhere in the header

# For scoring information, see: https://www.researchgate.net/profile/W-Williams/publication/323019722_Interpersonal_Regulation_Questionnaire_IRQ/data/5a7cced4a6fdccc013f51ad1/IRQ-01-2018.pdf?origin=publication_list

# Identify which columns are IRQ responses
IRQ_Cols <- grep("IRQ", colnames(df))

# Assign a Numeric Value for each IRQ Responses
for (i in IRQ_Cols){
  for (j in 1:length(rownames(df_measures))){
    if (df_measures[j,i] == "Strongly disagree" & !is.na(df_measures[j,i]))
      df_measures[j,i] <- 1
    if (df_measures[j,i] == "Disagree" & !is.na(df_measures[j,i]))
      df_measures[j,i] <- 2
    if (df_measures[j,i] == "Somewhat disagree" & !is.na(df_measures[j,i]))
      df_measures[j,i] <- 3
    if (df_measures[j,i] == "Neither agree nor disagree" & !is.na(df_measures[j,i]))
      df_measures[j,i] <- 4
    if (df_measures[j,i] == "Somewhat agree" & !is.na(df_measures[j,i]))
      df_measures[j,i] <- 5
    if (df_measures[j,i] == "Agree" & !is.na(df_measures[j,i]))
      df_measures[j,i] <- 6
    if (df_measures[j,i] == "Strongly agree" & !is.na(df_measures[j,i]))
      df_measures[j,i] <- 7   
  }
}

# Restructuring IRQ Variables as Numeric
for (i in IRQ_Cols){
  df[,i] <- as.numeric(df[,i])
}

# Calculating IRQ composite scores
for (i in 1:length(rownames(df_measures))){
  df_measures$IRQ_NT[i] <- sum(df_measures[i, IRQ_Cols[c(1:4)]])
  df_measures$IRQ_NE[i] <- sum(df_measures[i, IRQ_Cols[c(5:8)]])
  df_measures$IRQ_PT[i] <- sum(df_measures[i, IRQ_Cols[c(9:12)]])
  df_measures$IRQ_PE[i] <- sum(df_measures[i, IRQ_Cols[c(13:16)]])
}

# Removing IRQ We No Longer Need}
rm(IRQ_Cols, i, j)