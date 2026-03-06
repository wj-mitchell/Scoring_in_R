# Scoring the Intolerance of Uncertainty Scale (IUS)
# last updated: Feb. 2025
# author: Billy Mitchell

# PURPOSE: This scale measures responses to uncertainty in two composite measures: Prospective and Inhibitory Anxiety.

# STRUCTURE: This script assumes that each survey item is a separate column and each subject is a separate row in a dataframe named 'df'. It also assumes that each IUS-related column has 'IUS' somewhere in the header

# For scoring information, see: https://arc.psych.wisc.edu/self-report/intolerance-of-uncertainty-scale-ius/

score_IUS <- function(df, keep_items = F) {
  
  # Identify which columns are IUS responses
  IUS_Cols <- match(paste0("IUS_", 1:27), colnames(df))
  
  # Assign a Numeric Value for each IUS Responses
  for (i in IUS_Cols){
    for (j in 1:length(rownames(df))){
      if (df[j,i] == "Not at all characteristic of me" & !is.na(df[j,i]))
        df[j,i] <- 1
      if (df[j,i] == "A little characteristic of me" & !is.na(df[j,i]))
        df[j,i] <- 2
      if (df[j,i] == "Somewhat characteristic of me" & !is.na(df[j,i]))
        df[j,i] <- 3
      if (df[j,i] == "Very characteristic of me" & !is.na(df[j,i]))
        df[j,i] <- 4
      if (df[j,i] == "Entirely characteristic of me" & !is.na(df[j,i]))
        df[j,i] <- 5
    }
  }
  
  # Restructuring IUS Variables as Numeric
  for (i in IUS_Cols){
    df[,i] <- as.numeric(df[,i])
  }
  
  # Identify subscales
  IUS_F2_items <- c(4, 5, 6, 7, 8, 10, 11, 18, 19, 21, 26, 27)
  IUS_Cols_F2 <- IUS_Cols[IUS_F2_items]
  IUS_Cols_F1 <- IUS_Cols[-IUS_F2_items]
  
  # Score IUS
  df <- df %>%
    mutate(
      IUS_Total = rowSums(select(., all_of(IUS_Cols)), na.rm = TRUE),
      IUS_F1 = rowSums(select(., all_of(IUS_Cols_F1)), na.rm = TRUE),
      IUS_F2 = rowSums(select(., all_of(IUS_Cols_F2)), na.rm = TRUE)
    )
  
  # If we don't want individual responses
  if (keep_items == F){
    
    # Remove Individual Difference Responses
    df <- df[,-c(IUS_Cols)]
  }
  
  return(df)
}

