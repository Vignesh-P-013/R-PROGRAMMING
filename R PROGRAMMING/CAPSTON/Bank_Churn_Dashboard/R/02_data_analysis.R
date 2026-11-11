# ==========================================
# MODULE 1 - CUSTOMER DATA ANALYSIS
# ==========================================

library(ggplot2)
library(dplyr)

# Load cleaned dataset
data <- read.csv("data/cleaned_bank_churn.csv")

# Customer statistics
total_customers <- nrow(data)

churned_customers <- sum(data$Exited == 1)

active_customers <- sum(data$Exited == 0)

churn_rate <- round(
  (churned_customers / total_customers) * 100, 2
)

# Display statistics
cat("Total Customers:", total_customers, "\n")
cat("Churned Customers:", churned_customers, "\n")
cat("Active Customers:", active_customers, "\n")
cat("Churn Rate:", churn_rate, "%\n")

# 1. Churn Distribution
p1 <- ggplot(data, aes(x = factor(Exited))) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Customer Churn Distribution",
    x = "Churn Status",
    y = "Customer Count"
  ) +
  theme_minimal()

print(p1)

# 2. Age Distribution
p2 <- ggplot(data, aes(x = Age)) +
  geom_histogram(
    fill = "skyblue",
    color = "black",
    bins = 20
  ) +
  labs(
    title = "Customer Age Distribution",
    x = "Age",
    y = "Customer Count"
  ) +
  theme_minimal()

print(p2)

# 3. Geography-wise Churn
p3 <- ggplot(
  data,
  aes(x = Geography, fill = factor(Exited))
) +
  geom_bar(position = "dodge") +
  labs(
    title = "Geography-wise Customer Churn",
    x = "Geography",
    y = "Customer Count",
    fill = "Churn"
  ) +
  theme_minimal()

print(p3)

cat("Customer Analysis Completed!")