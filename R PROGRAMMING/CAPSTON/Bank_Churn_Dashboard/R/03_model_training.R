# ==========================================================
# BANK CUSTOMER CHURN - MODEL TRAINING AND EVALUATION
# Algorithms: Logistic Regression and Random Forest
# ==========================================================


# 1. LOAD REQUIRED LIBRARIES

library(caret)
library(dplyr)
library(randomForest)
library(pROC)


# 2. LOAD CLEANED DATASET

data <- read.csv("data/cleaned_bank_churn.csv")

cat("Dataset Loaded Successfully\n")
cat("Total Records:", nrow(data), "\n")
cat("Total Columns:", ncol(data), "\n")


# 3. DATA PREPROCESSING

# Convert categorical columns into factors

data$Geography <- as.factor(data$Geography)
data$Gender <- as.factor(data$Gender)

# Convert target variable into factor

data$Exited <- factor(
  ifelse(data$Exited == 1, "Churned", "Stayed"),
  levels = c("Churned", "Stayed")
)

# Remove CustomerId from model predictors

model_data <- data %>%
  select(-any_of("CustomerId"))

cat("\nData Preprocessing Completed\n")


# 4. SPLIT DATA INTO TRAINING AND TESTING

set.seed(123)

train_index <- createDataPartition(
  model_data$Exited,
  p = 0.80,
  list = FALSE
)

train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

cat("\nTraining Records:", nrow(train_data), "\n")
cat("Testing Records:", nrow(test_data), "\n")


# 5. MODEL TRAINING CONTROL

train_control <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE
)


# 6. TRAIN LOGISTIC REGRESSION MODEL

set.seed(123)

log_model <- train(
  Exited ~ .,
  data = train_data,
  method = "glm",
  family = binomial(),
  trControl = train_control
)

cat("\nLogistic Regression Model Trained Successfully\n")


# 7. TRAIN RANDOM FOREST MODEL

set.seed(123)

rf_model <- train(
  Exited ~ .,
  data = train_data,
  method = "rf",
  ntree = 200,
  trControl = train_control
)

cat("\nRandom Forest Model Trained Successfully\n")


# 8. PREDICTIONS ON TEST DATA

# Logistic Regression

log_predictions <- predict(
  log_model,
  newdata = test_data
)

log_probabilities <- predict(
  log_model,
  newdata = test_data,
  type = "prob"
)


# Random Forest

rf_predictions <- predict(
  rf_model,
  newdata = test_data
)

rf_probabilities <- predict(
  rf_model,
  newdata = test_data,
  type = "prob"
)


# 9. CONFUSION MATRICES

log_results <- confusionMatrix(
  log_predictions,
  test_data$Exited,
  positive = "Churned"
)

rf_results <- confusionMatrix(
  rf_predictions,
  test_data$Exited,
  positive = "Churned"
)


# 10. FUNCTION TO CALCULATE EVALUATION METRICS

calculate_metrics <- function(confusion_result,
                              probabilities,
                              actual_values) {
  
  roc_result <- pROC::roc(
    response = as.integer(actual_values == "Churned"),
    predictor = probabilities$Churned,
    quiet = TRUE
  )
  
  data.frame(
    Accuracy = as.numeric(
      confusion_result$overall["Accuracy"]
    ),
    
    Precision = as.numeric(
      confusion_result$byClass["Pos Pred Value"]
    ),
    
    Recall = as.numeric(
      confusion_result$byClass["Sensitivity"]
    ),
    
    F1_Score = as.numeric(
      confusion_result$byClass["F1"]
    ),
    
    ROC_AUC = as.numeric(
      pROC::auc(roc_result)
    )
  )
}


# 11. CALCULATE METRICS FOR BOTH MODELS

log_metrics <- calculate_metrics(
  log_results,
  log_probabilities,
  test_data$Exited
)

rf_metrics <- calculate_metrics(
  rf_results,
  rf_probabilities,
  test_data$Exited
)


# 12. CREATE MODEL COMPARISON TABLE

model_metrics <- rbind(
  
  cbind(
    Model = "Logistic Regression",
    log_metrics
  ),
  
  cbind(
    Model = "Random Forest",
    rf_metrics
  )
  
)

cat("\n========== MODEL PERFORMANCE ==========\n")

print(model_metrics)


# 13. DISPLAY CONFUSION MATRICES

cat("\n========== LOGISTIC REGRESSION ==========\n")

print(log_results)

cat("\n========== RANDOM FOREST ==========\n")

print(rf_results)


# 14. SAVE MODEL PERFORMANCE RESULTS

if (!dir.exists("models")) {
  dir.create("models")
}

write.csv(
  model_metrics,
  "models/model_metrics.csv",
  row.names = FALSE
)


# 15. SAVE TRAINED MODELS

saveRDS(
  log_model,
  "models/logistic_model.rds"
)

saveRDS(
  rf_model,
  "models/random_forest_model.rds"
)


# 16. FINAL OUTPUT

cat("\n========================================\n")
cat("MODEL TRAINING COMPLETED SUCCESSFULLY\n")
cat("========================================\n")

cat("\nGenerated Files:\n")
cat("1. models/logistic_model.rds\n")
cat("2. models/random_forest_model.rds\n")
cat("3. models/model_metrics.csv\n")

cat("\nEvaluation Metrics:\n")
cat("Accuracy, Precision, Recall, F1-Score, ROC-AUC\n")

cat("\nAll Operations Completed!\n")