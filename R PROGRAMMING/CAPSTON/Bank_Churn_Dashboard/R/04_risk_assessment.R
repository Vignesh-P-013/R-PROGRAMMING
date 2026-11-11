# ==========================================
# MODULE 3 - CUSTOMER RISK ASSESSMENT
# ==========================================

library(ggplot2)
library(dplyr)

# Load cleaned dataset
data <- read.csv("data/cleaned_bank_churn.csv")

# Load trained Random Forest model
rf_model <- readRDS(
  "models/random_forest_model.rds"
)

# Prepare customer data for prediction
predictor_data <- data[
  , !names(data) %in% c("CustomerId", "Exited")
]

# Calculate churn probability
probabilities <- predict(
  rf_model,
  newdata = predictor_data,
  type = "prob"
)

# Add churn probability
data$Churn_Probability <-
  probabilities$Churned * 100

# Classify customer risk
data$Risk_Level <- ifelse(
  data$Churn_Probability >= 60,
  "High Risk",
  ifelse(
    data$Churn_Probability >= 30,
    "Medium Risk",
    "Low Risk"
  )
)

# Display risk distribution
print(table(data$Risk_Level))

# Risk visualization
p <- ggplot(
  data,
  aes(x = Risk_Level, fill = Risk_Level)
) +
  geom_bar() +
  labs(
    title = "Customer Churn Risk Assessment",
    x = "Risk Level",
    y = "Customer Count"
  ) +
  theme_minimal()

print(p)

# Save risk assessment results
write.csv(
  data,
  "data/customer_risk_analysis.csv",
  row.names = FALSE
)

cat("Customer Risk Assessment Completed!")