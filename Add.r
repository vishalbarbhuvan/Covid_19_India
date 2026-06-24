# Add.r
# Diagnostic script to check missing values, duplicate rows,
# and non-numeric values in TotalSamples, Negative, and Positive.

# 1. Load the dataset (adjust filename as needed)
file_name <- "StatewiseTestingDetails.csv" 

cat("Loading dataset:", file_name, "...\n")
if (!file.exists(file_name)) {
  stop(paste("Error: File", file_name, "not found."))
}
df <- read.csv(file_name, stringsAsFactors = FALSE)

# 2. Check Duplicated Rows
cat("\n=================== DUPLICATE ROWS CHECK ===================\n")
# Check exact row duplicates
total_dups <- sum(duplicated(df))
cat("Total exact duplicate rows:", total_dups, "\n")
if (total_dups > 0) {
  print(head(df[duplicated(df), ], 5))
}

# Check duplicate Date-State combinations (primary key violation)
date_state_dups <- sum(duplicated(df[, c("Date", "State")]))
cat("Duplicate Date-State combinations:", date_state_dups, "\n")
if (date_state_dups > 0) {
  cat("Examples of duplicate Date-State rows:\n")
  dup_rows <- df[duplicated(df[, c("Date", "State")]) | duplicated(df[, c("Date", "State")], fromLast = TRUE), ]
  print(head(dup_rows, 6))
}

# 3. Check Missing Values (NAs or empty strings)
cat("\n==================== MISSING VALUES CHECK ====================\n")
target_cols <- c("TotalSamples", "Negative", "Positive")

for (col in target_cols) {
  vals <- df[[col]]
  na_count <- sum(is.na(vals))
  empty_count <- sum(as.character(vals) == "", na.rm = TRUE)
  total_missing <- na_count + empty_count
  cat(sprintf("Column '%s':\n", col))
  cat(sprintf("  - NA values: %d\n", na_count))
  cat(sprintf("  - Empty strings: %d\n", empty_count))
  cat(sprintf("  - Total missing: %d (%.2f%% of total rows)\n", total_missing, (total_missing / nrow(df)) * 100))
}

# 4. Check Non-Numeric Values
cat("\n=================== NON-NUMERIC VALUES CHECK ===================\n")
check_non_numeric <- function(col_name, vec) {
  # Convert to character vector
  char_vec <- as.character(vec)
  
  # Identify indices that are not empty and not NA
  non_empty_idx <- !is.na(char_vec) & char_vec != ""
  
  # Attempt conversion to numeric
  num_vec <- suppressWarnings(as.numeric(char_vec))
  
  # Any value that becomes NA but wasn't NA originally is non-numeric
  failed_conversion <- non_empty_idx & is.na(num_vec)
  failed_vals <- unique(char_vec[failed_conversion])
  failed_count <- sum(failed_conversion)
  
  cat(sprintf("Column '%s':\n", col_name))
  cat(sprintf("  - Total non-numeric entries: %d\n", failed_count))
  if (failed_count > 0) {
    cat("  - Distinct non-numeric values found:\n")
    print(failed_vals)
    cat("  - Example rows with non-numeric values:\n")
    print(head(df[failed_conversion, c("Date", "State", col_name)], 5))
  } else {
    cat("  - All non-missing values are numeric.\n")
  }
}

for (col in target_cols) {
  check_non_numeric(col, df[[col]])
}