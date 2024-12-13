# Load necessary libraries
library(dplyr)
library(caret)
library(randomForest)
library(e1071)
library(rpart)
library(rpart.plot)

# Load data
data <- read.csv("nhanesBetter.csv")

# Convert categorical variables to factors
data$subject_biological_sex <- as.factor(data$subject_biological_sex)
data$subject_high_blood_pressure <- as.factor(data$subject_high_blood_pressure)
data$subject_race_ethnicity <- as.factor(data$subject_race_ethnicity)
data$diagnosis <- as.factor(data$diagnosis)

# Scale numerical variables
numerical_vars <- c("blood_cholesterol_mg_per_dc", 
                    "blood_glycosylated_hemoglobin_volume_percentage",
                    "household_ref_person_age_year", 
                    "subject_age_year_screening_time", 
                    "subject_2_year_weight_screening_time", 
                    "subject_2_year_mec_weight_mec_time")

data[numerical_vars] <- scale(data[numerical_vars])

# Split into training and test sets
set.seed(123)
trainIndex <- createDataPartition(data$diagnosis, p = 0.8, list = FALSE)
train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]


# Random Forest for feature importance
set.seed(123)
rf_model <- randomForest(diagnosis ~ ., data = train_data, importance = TRUE)
importance <- as.data.frame(varImpPlot(rf_model, type = 1))

# Select top features
top_features <- rownames(importance)[order(-importance$MeanDecreaseAccuracy)][1:5]
print(top_features)



# Logistic Regression Model
log_model <- glm(diagnosis ~ ., data = train_data[, c(top_features, "diagnosis")], family = binomial)

# Predict and Evaluate
log_preds <- predict(log_model, newdata = test_data[, c(top_features, "diagnosis")], type = "response")
log_preds_class <- ifelse(log_preds > 0.5, "TRUE", "FALSE")

confusionMatrix(factor(log_preds_class, levels = levels(test_data$diagnosis)), test_data$diagnosis)




# Decision Tree Model
tree_model <- rpart(diagnosis ~ ., data = train_data[, c(top_features, "diagnosis")], method = "class")

# Plot Tree
rpart.plot(tree_model, type = 3, extra = 102)

# Predict and Evaluate
tree_preds <- predict(tree_model, newdata = test_data[, c(top_features, "diagnosis")], type = "class")
confusionMatrix(tree_preds, test_data$diagnosis)




# SVM Model
svm_model <- svm(diagnosis ~ ., data = train_data[, c(top_features, "diagnosis")], kernel = "linear")

# Predict and Evaluate
svm_preds <- predict(svm_model, newdata = test_data[, c(top_features, "diagnosis")])
confusionMatrix(svm_preds, test_data$diagnosis)





physical.activity <- merge(diabetes, physical.activity, by = "SEQN")
set.seed(123)
physical.activity$DIQ010 <- factor(physical.activity$DIQ010)


na_counts_base <- colSums(is.na(physical.activity))
print(na_counts_base)



rf_model <- randomForest(DIQ010 ~ ., data = physical.activity, importance = TRUE)
importance <- as.data.frame(varImpPlot(rf_model, type = 1))

# Select top features
top_features <- rownames(importance)[order(-importance$MeanDecreaseAccuracy)][1:5]
print(top_features)