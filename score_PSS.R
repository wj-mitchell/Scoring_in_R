compute_pss <- function(df,
                        item_cols,
                        new_col = "PSS_total",
                        reverse_items = c("PSS4", "PSS5", "PSS7", "PSS8"),
                        method = c("sum", "mean"),
                        na_rm = TRUE,
                        drop_items = FALSE) {
  method <- match.arg(method)
  
  # Safety checks
  missing_items <- setdiff(item_cols, names(df))
  if (length(missing_items) > 0) {
    stop("These item_cols are not in df: ", paste(missing_items, collapse = ", "))
  }
  
  # Keep only reverse items that are actually in item_cols
  reverse_items <- intersect(reverse_items, item_cols)
  
  # Helper: pull leading number from "2 Sometimes" etc.
  parse_item <- function(x) {
    if (is.numeric(x)) return(as.numeric(x))
    x <- as.character(x)
    # extract leading integer (handles "2 Sometimes", "0 Never", etc.)
    out <- suppressWarnings(as.numeric(sub("^\\s*([0-9]+).*$", "\\1", x)))
    # if it didn't start with a number, set NA
    out[!grepl("^\\s*[0-9]+", x)] <- NA_real_
    out
  }
  
  # Parse all items to numeric in a temp matrix
  X <- as.data.frame(lapply(df[item_cols], parse_item))
  
  # Reverse-score selected items (PSS 0-4 scale => reverse = 4 - x)
  if (length(reverse_items) > 0) {
    X[reverse_items] <- lapply(X[reverse_items], function(v) ifelse(is.na(v), NA_real_, 4 - v))
  }
  
  # Composite
  comp <- if (method == "sum") {
    rowSums(X, na.rm = na_rm)
  } else {
    rowMeans(X, na.rm = na_rm)
  }
  
  # If na_rm = FALSE, rowSums/Means will produce NA when any NA present;
  # but rowMeans doesn't do that by default unless na.rm=FALSE is passed (we do).
  df[[new_col]] <- comp
  
  if (drop_items) {
    df[item_cols] <- NULL
  }
  
  df
}
