# clean_data.R
# Script to clean StatewiseTestingDetails.csv and output a cleaned version.

# Load raw dataset
raw_file <- "StatewiseTestingDetails.csv"
output_file <- "StatewiseTestingDetails_Cleaned.csv"

cat("Loading raw dataset...\n")
if (!file.exists(raw_file)) {
  stop("Error: Raw dataset StatewiseTestingDetails.csv not found in the current directory.")
}
df <- read.csv(raw_file, stringsAsFactors = FALSE)
initial_rows <- nrow(df)
cat("Initial row count:", initial_rows, "\n")

# 1. Convert columns to appropriate types
df$TotalSamples <- as.numeric(df$TotalSamples)
df$Negative <- as.numeric(df$Negative)
df$Positive <- as.numeric(df$Positive)

# 2. Remove exact duplicate rows
cat("\nRemoving duplicate rows...\n")
dup_count <- sum(duplicated(df))
df <- unique(df)
cat("Removed", dup_count, "duplicate row(s). Current row count:", nrow(df), "\n")

# 3. Correct known typographical errors
cat("\nCorrecting typographical errors...\n")

# Jammu and Kashmir (2021-06-02)
jk_idx <- which(df$State == "Jammu and Kashmir" & df$Date == "2021-06-02" & df$Negative == 83561026)
if (length(jk_idx) > 0) {
  df$Negative[jk_idx] <- 8356026
  cat("- Corrected Jammu & Kashmir (2021-06-02) Negative count from 83561026 to 8356026.\n")
}

# Tripura (2020-05-24)
tr_idx <- which(df$State == "Tripura" & df$Date == "2020-05-24" & df$Negative == 19537)
if (length(tr_idx) > 0) {
  df$Negative[tr_idx] <- 19087
  cat("- Corrected Tripura (2020-05-24) Negative count from 19537 to 19087 (TotalSamples - Positive).\n")
}

# Jharkhand (2021-01-28)
jh_idx <- which(df$State == "Jharkhand" & df$Date == "2021-01-28" & df$Negative == 5185675)
if (length(jh_idx) > 0) {
  df$Negative[jh_idx] <- 5067118
  cat("- Corrected Jharkhand (2021-01-28) Negative count from 5185675 to 5067118 (TotalSamples - Positive).\n")
}

# 4. Remove rows where Negative + Positive > TotalSamples
cat("\nFiltering out mathematically inconsistent rows (Negative + Positive > TotalSamples)...\n")
inconsistent_idx <- which(!is.na(df$Negative) & !is.na(df$Positive) & (df$Negative + df$Positive) > df$TotalSamples)
inconsistent_count <- length(inconsistent_idx)

if (inconsistent_count > 0) {
  cat("- Removing", inconsistent_count, "inconsistent row(s).\n")
  # Print the rows being removed for transparency
  print(df[inconsistent_idx, c("Date", "State", "TotalSamples", "Negative", "Positive")])
  df <- df[-inconsistent_idx, ]
} else {
  cat("- No inconsistent rows found.\n")
}

# 5. Type cast counts to integer type
df$TotalSamples <- as.integer(df$TotalSamples)
df$Negative <- as.integer(df$Negative)
df$Positive <- as.integer(df$Positive)

# 6. Save the cleaned dataset
cat("\nSaving cleaned dataset to:", output_file, "\n")
write.csv(df, output_file, row.names = FALSE, na = "")

final_rows <- nrow(df)
cat("Final row count:", final_rows, "\n")
cat("Total rows removed/filtered:", initial_rows - final_rows, "\n")
cat("Cleaning complete!\n")
