# Scoring the State-Trait Anxiety Inventory
# last updated: Feb. 2025
# author: Billy Mitchell

# PURPOSE: This scale measures current and long-term anxiety differences.

# STRUCTURE: This script assumes that each survey item is a separate column and each subject is a separate row in a dataframe named 'df'. It also assumes that each STAI-related column has 'STAI' somewhere in the header

score_STAI <- function(df, type = c("State", "Trait"), keep_items = F){

# Identify which columns are STAI responses
STAI_Cols <- grep("STAI", colnames(df))

# Assign a Numeric Value for each STAI Responses
for (i in STAI_Cols){
  for (j in 1:length(rownames(df))){
    if (df[j,i] == "Not at all" & !is.na(df[j,i]))
      df[j,i] <- 1
    if (df[j,i] == "Somewhat" & !is.na(df[j,i]))
      df[j,i] <- 2
    if (df[j,i] == "Moderately so" & !is.na(df[j,i]))
      df[j,i] <- 3
    if (df[j,i] == "Very much so" & !is.na(df[j,i]))
      df[j,i] <- 4
  }
}

# Restructuring STAI Variables as Numeric
for (i in STAI_Cols){
  df[,i] <- as.numeric(df[,i])
}

# Reverse Scoring STAI- Please note: it's important that questions appear in the same order, or these indices may not be correct
if (type == "State"){
  STAI_Cols.r <- STAI_Cols[c(1,2,5,8,10,11,15,16,19,20)] 
}
if (type == "Trait"){
  STAI_Cols.r <- STAI_Cols[c(1,6,7,10,11,15,16,19)]
}
for (i in STAI_Cols.r){
  df[,i] <- 5 - df[,i]
}

# Score STAI
df <- df %>%
  mutate(
    STAI = rowSums(select(., all_of(STAI_Cols)), na.rm = TRUE)
  )

# Changing variable name
if (type == "State"){
  names(df)[nrow(df)] = "STAI_State"
}
if (type == "Trait"){
  names(df)[nrow(df)] = "STAI_Trait"
}

# If we don't want individual responses
if (keep_items == F){
  
  # Remove Individual Difference Responses
  df <- df[,-c(STAI_Cols)]
}

return(df)
}