# ==========================================
# MODULE 1 - DATA CLEANING
# ==========================================

library(dplyr)

# Load original dataset
data <- read.csv("data/bank_Churn.csv")

# Display original dataset
print(head(data))

# Check dimensions
print(dim(data))

# Check missing values
print(colSums(is.na(data)))

# Remove duplicate records
data <- distinct(data)

# Remove unnecessary columns
data <- data %>%
  select(-any_of(c("RowNumber", "Surname")))

# Handle missing values
data <- na.omit(data)

# Save cleaned dataset
write.csv(
  data,
  "data/cleaned_bank_churn.csv",
  row.names = FALSE
)

# Display cleaned dataset
print(head(data))

cat("Data Cleaning Completed Successfully!")