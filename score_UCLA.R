# Scoring the UCLA Loneliness Scale
# last updated: Feb. 2025
# author: Billy Mitchell

# PURPOSE: This scale measures subjective feelings of loneliness and social isolation in a single composite measure.

# STRUCTURE: This script assumes that each survey item is a separate column and each subject is a separate row in a dataframe named 'df'. It also assumes that each UCLA-related column has 'loneliness_' somewhere in the header

# For scoring information, see original UCLA Loneliness Scale materials

score_UCLA <- function(df, keep_items = F) {
  
  # Identify which columns are UCLA responses
  UCLA_Cols <- grep("UCLA_", colnames(df))
  
  # Assign a Numeric Value for each UCLA Response
  for (i in UCLA_Cols){
    for (j in 1:length(rownames(df))){
      if (df[j,i] == "Never" & !is.na(df[j,i]))
        df[j,i] <- 1
      if (df[j,i] == "Rarely" & !is.na(df[j,i]))
        df[j,i] <- 2
      if (df[j,i] == "Sometimes" & !is.na(df[j,i]))
        df[j,i] <- 3
      if (df[j,i] == "Often" & !is.na(df[j,i]))
        df[j,i] <- 4
    }
  }
  
  # Restructuring UCLA Variables as Numeric
  for (i in UCLA_Cols){
    df[,i] <- as.numeric(df[,i])
  }
  
  # Identify reverse-scored items
  UCLA_Reverse_items <- c(1, 4, 5, 6, 9, 10, 15, 16, 18, 20)
  UCLA_Cols_Reverse <- UCLA_Cols[UCLA_Reverse_items]
  
  # Reverse score designated UCLA items
  for (i in UCLA_Cols_Reverse){
    for (j in 1:length(rownames(df))){
      if (df[j,i] == 1 & !is.na(df[j,i]))
        df[j,i] <- 4
      else if (df[j,i] == 2 & !is.na(df[j,i]))
        df[j,i] <- 3
      else if (df[j,i] == 3 & !is.na(df[j,i]))
        df[j,i] <- 2
      else if (df[j,i] == 4 & !is.na(df[j,i]))
        df[j,i] <- 1
    }
  }
  
  # Score UCLA
  df <- df %>%
    mutate(
      UCLA_Total = rowSums(select(., all_of(UCLA_Cols)), na.rm = TRUE)
    )
  
  # If we don't want individual responses
  if (keep_items == F){
    
    # Remove Individual Difference Responses
    df <- df[,-c(UCLA_Cols)]
  }
  
  return(df)
}