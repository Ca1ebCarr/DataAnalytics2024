library(foreign)
library(randomForest)
library(datasets)
library(caret)
library(ggraph)
library(igraph)
library(dplyr)
library(e1071)


##FUNCTIONS####
stratified_sample <- function(data, strat_column, size_per_stratum) {
  strata <- unique(data[[strat_column]])
  sampled_data <- do.call(rbind, lapply(strata, function(stratum) {
    subset_data <- data[data[[strat_column]] == stratum, ]
    subset_data[sample(nrow(subset_data), size_per_stratum), ]
  }))
  return(sampled_data)
}



####################
# IMPORT AND CLEAN #
####################

threshold <- 0.33

diabetes <- read.xport("./P_DIQ.XPT")
diabetes <- diabetes[, c("SEQN", "DIQ010")]
diabetes <-diabetes[diabetes$DIQ010==1 | diabetes$DIQ010==2, ]


### Demographics ###
demographics <- read.xport("./P_DEMO.XPT")
demographics <- merge(diabetes, demographics, by = "SEQN")
demographics <- demographics[,c(2, 5, 6, 7, 8,9,10,11,12,13,15,16,19,30)]
demographics <- demographics[, colMeans(is.na(demographics)) <= threshold]
demographics$DIQ010 <- as.factor(demographics$DIQ010)
cols_to_convert <- setdiff(1:ncol(demographics), c(3, 10))
demographics[cols_to_convert] <- lapply(demographics[cols_to_convert], as.factor)
demographics_clean <- na.omit(demographics)
rf_model <- randomForest(DIQ010 ~ ., data = demographics_clean, importance = TRUE)
importance <- as.data.frame(varImpPlot(rf_model, type = 1))

# Select top features
top_features <- rownames(importance)[order(-importance$MeanDecreaseAccuracy)][1:5]
print(top_features)

set.seed(42)
demographics_clean_sample <- stratified_sample(demographics_clean, "DIQ010", 1000)
#trainIndex <- createDataPartition(demographics_clean_sample$DIQ010, p = 0.8, list = FALSE, times = 1)
#train_data <- demographics_clean_sample[trainIndex, ]
#test_data <- demographics_clean_sample[-trainIndex, ]
trainIndex <- createDataPartition(demographics_clean$DIQ010, p = 0.8, list = FALSE, times = 1)
train_data <- demographics_clean[trainIndex, ]
test_data <- demographics_clean[-trainIndex, ]


# Logistic Regression Model
log_model <- glm(DIQ010 ~ ., data = train_data[, c(top_features, "DIQ010")], family = binomial)

log_preds <- predict(log_model, newdata = test_data[, c(top_features, "DIQ010")], type = "response")
log_preds_class <- ifelse(log_preds > 0.5, "TRUE", "FALSE")


log_preds_class <- factor(log_preds_class, levels = c(FALSE, TRUE), labels = c(1, 2))

test_data$DIQ010 <- factor(test_data$DIQ010, levels = c(1, 2))


confusionMatrix(factor(log_preds_class, levels = levels(test_data$DIQ010)), test_data$DIQ010)



train_control <- trainControl(method = "cv", number = 5)  # 5-fold cross-validation
rf.mod <- train(DIQ010~ .,data=train_data, method = "rf", prox = TRUE, trControl = train_control, ntree = 100)
rf.mod
getTree(rf.mod$finalModel, k=2)
pred <- predict(rf.mod,test_data)
test_data$predRight <- pred==test_data$DIQ010
table(pred,test_data$DIQ010)





diet <- read.xport("./P_DR1TOT.XPT")
diet <- diet[, colMeans(is.na(diet)) <= threshold]

body.measures <- read.xport("./P_BMX.XPT")
body.measures <- body.measures[, colMeans(is.na(body.measures)) <= threshold]

insulin <- read.xport("./P_INS.XPT")
insulin <- insulin[, colMeans(is.na(insulin)) <= threshold]

alcohol.use <- read.xport("./P_ALQ.XPT")
alcohol.use <- alcohol.use[, colMeans(is.na(alcohol.use)) <= threshold]

diet.behaviour <- read.xport("./P_DBQ.XPT")
diet.behaviour <- diet.behaviour[, colMeans(is.na(diet.behaviour)) <= threshold]

early.childhood <- read.xport("./P_ECQ.XPT")
early.childhood <- early.childhood[, colMeans(is.na(early.childhood)) <= threshold]

income <- read.xport("./P_INQ.XPT")
income <- income[, colMeans(is.na(income)) <= threshold]

physical.activity <- read.xport("./P_PAQ.XPT")
physical.activity <- physical.activity[, colMeans(is.na(physical.activity)) <= threshold]
physical.activity <- merge(diabetes, physical.activity, by = "SEQN")
physical.activity <- physical.activity[,2:8]
physical.activity$DIQ010 <- as.factor(physical.activity$DIQ010)
cols_to_convert <- setdiff(1:ncol(physical.activity), c(7))
physical.activity[cols_to_convert] <- lapply(physical.activity[cols_to_convert], as.factor)
physical.activity_clean <- na.omit(physical.activity)
rf_model <- randomForest(DIQ010 ~ ., data = physical.activity_clean, importance = TRUE)
importance <- as.data.frame(varImpPlot(rf_model, type = 1))

# Select top features
top_features <- rownames(importance)[order(-importance$MeanDecreaseAccuracy)][1:5]
print(top_features)

set.seed(42)
physical.activity_clean_sample <- stratified_sample(physical.activity_clean, "DIQ010", 1000)
trainIndex <- createDataPartition(physical.activity_clean_sample$DIQ010, p = 0.8, list = FALSE, times = 1)
train_data <- physical.activity_clean_sample[trainIndex, ]
test_data <- physical.activity_clean_sample[-trainIndex, ]


# Logistic Regression Model
log_model <- glm(DIQ010 ~ ., data = train_data[, c(top_features, "DIQ010")], family = binomial)

# Predict and Evaluate
log_preds <- predict(log_model, newdata = test_data[, c(top_features, "DIQ010")], type = "response")
log_preds_class <- ifelse(log_preds > 0.5, "TRUE", "FALSE")


# Convert log_preds_class to factor with levels 1 and 2
log_preds_class <- factor(log_preds_class, levels = c(FALSE, TRUE), labels = c(1, 2))

# Ensure that test_data$DIQ010 is also a factor with the same levels
test_data$DIQ010 <- factor(test_data$DIQ010, levels = c(1, 2))


confusionMatrix(factor(log_preds_class, levels = levels(test_data$DIQ010)), test_data$DIQ010)



train_control <- trainControl(method = "cv", number = 5)  # 5-fold cross-validation
rf.mod <- train(DIQ010~ .,data=train_data, method = "rf", prox = TRUE, trControl = train_control, ntree = 100)
rf.mod
getTree(rf.mod$finalModel, k=2)
pred <- predict(rf.mod,test_data)
test_data$predRight <- pred==test_data$DIQ010
table(pred,test_data$DIQ010)










weight.history <- read.xport("./P_WHQ.XPT")
weight.history <- weight.history[, colMeans(is.na(weight.history)) <= threshold]









#Numeric Data/
diabetes <- read.xport("./P_DIQ.XPT")
diabetes <- diabetes[,c("SEQN", "DIQ010")]

demographics <- read.xport("./P_DEMO.XPT")
demographics <- demographics[, c("SEQN", "RIDAGEYR", "DMDEDUC2", "INDFMPIR")]
data <- merge(diabetes, demographics, by = "SEQN")


diet <- read.xport("./P_DR1TOT.XPT")
diet <- diet[, c("SEQN", "DR1TNUMF", "DR1TKCAL","DR1TPROT", "DR1TCARB", "DR1TSUGR", "DR1TFIBE", "DR1TTFAT", "DR1TSFAT", "DR1TMFAT", "DR1TPFAT", "DR1TCHOL", "DR1TATOC", "DR1TATOA", "DR1TRET", "DR1TVARA", "DR1TACAR", "DR1TBCAR", "DR1TCRYP", "DR1TLYCO", "DR1TLZ", "DR1TVB1", "DR1TVB2", "DR1TNIAC", "DR1TVB6", "DR1TFOLA", "DR1TFA", "DR1TFF", "DR1TFDFE", "DR1TCHL", "DR1TVB12", "DR1TB12A", "DR1TVC", "DR1TVD", "DR1TVK", "DR1TCALC", "DR1TPHOS", "DR1TMAGN", "DR1TIRON", "DR1TZINC", "DR1TCOPP", "DR1TSODI", "DR1TPOTA", "DR1TSELE", "DR1TCAFF", "DR1TTHEO", "DR1TALCO", "DR1TMOIS", "DR1TS040", "DR1TS060", "DR1TS080", "DR1TS100", "DR1TS120", "DR1TS140", "DR1TS160", "DR1TS180", "DR1TM161", "DR1TM181", "DR1TM201", "DR1TM221", "DR1TP182", "DR1TP183", "DR1TP184", "DR1TP204", "DR1TP205", "DR1TP225", "DR1TP226", "DR1_320Z", "DR1_330Z", "DR1BWATZ")]
data <- merge(data, diet, by = "SEQN")

blood.pressure <- read.xport("./P_BPQ.XPT")
blood.pressure <- blood.pressure[, c("SEQN", "BPQ020", "BPQ080")]
blood.pressure <-blood.pressure[(blood.pressure$BPQ020==1 | blood.pressure$BPQ020==2) & (blood.pressure$BPQ080==1 | blood.pressure$BPQ080==2), ]
data <- merge(data, blood.pressure, by = "SEQN")



body.measures <- read.xport("./P_BMX.XPT")
body.measures <- body.measures[, c("SEQN", "BMXWT", "BMXHT", "BMXLEG", "BMXARML", "BMXARMC", "BMXWAIST", "BMXHIP")]
data <- merge(data, body.measures, by = "SEQN")



diet.behaviour <- read.xport("./P_DBQ.XPT")
diet.behaviour <- diet.behaviour[, c(1, 36, 37, 38, 39)]
diet.behaviour <-diet.behaviour[(diet.behaviour$DBD895<=21 & diet.behaviour$DBD900<=21 & diet.behaviour$DBD905<=90 & diet.behaviour$DBD910<=90), ]
data <- merge(data, diet.behaviour, by = "SEQN")


income <- read.xport("./P_INQ.XPT")
income <- income[, c("SEQN", "INDFMMPI")]
data <- merge(data, income, by = "SEQN")

physical.activity <- read.xport("./P_PAQ.XPT")
physical.activity <- physical.activity[, c("SEQN", "PAD680")]
physical.activity <-physical.activity[(physical.activity$PAD680<=1400), ]
data <- merge(data, physical.activity, by = "SEQN")

  

weight.history <- read.xport("./P_WHQ.XPT")
weight.history <- weight.history[, c("SEQN", "WHD050", "WHQ225")]
weight.history$WHQ225[weight.history$WHQ225 == 5] <- 0
data <- merge(data, weight.history, by = "SEQN")

glycohemoglobin <- read.xport("./P_GHB.XPT")
data <- merge(data, glycohemoglobin, by = "SEQN")

data <- na.omit(data)
data$DIQ010 <- as.factor(data$DIQ010)

normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# normalize
data[, 3:ncol(data)] <- lapply(data[, 3:ncol(data)], normalize)
data <- data[data$DIQ010==1 | data$DIQ010==2, ]





# Split data 
set.seed(42)  
trainIndex <- createDataPartition(data$DIQ010, p = 0.8, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]


trainX <- trainData[, 3:ncol(data)] 
testX <- testData[, 3:ncol(data)]
trainY <- trainData[, 2]            
testY <- testData[, 2]

# Standardize 
preProc <- preProcess(trainX, method = c("center", "scale"))
trainX <- predict(preProc, trainX)
testX <- predict(preProc, testX)


# knn
k <- 5  
knnPred <- knn(train = trainX, test = testX, cl = trainY, k = k)

# Evaluate 
confMatrix <- confusionMatrix(knnPred, testY)
print(confMatrix)







#kmeans
clustering_data <- trainData[, -c(1, 2)]

kmeans_result <- kmeans(clustering_data, centers = 2, nstart = 25)


cluster_mapping <- table(kmeans_result$cluster, trainData$DIQ010)
print(cluster_mapping)
cluster_to_label <- ifelse(cluster_mapping[1, 1] > cluster_mapping[1, 2], 1, 2)
predicted_labels <- ifelse(kmeans_result$cluster == 1, cluster_to_label, 3 - cluster_to_label)
confusion <- table(Predicted = predicted_labels, Actual = trainData$DIQ010)
print(confusion)

train_data$Cluster <- kmeans_result$cluster

table(train_data$Cluster)








#trees 
library(rpart)
tree <- rpart(DIQ010 ~ ., data = trainData, method = "class")
print(tree)

# treeporedict
treePred <- predict(tree, newdata = testData, type = "class")
confusion <- table(Predicted = treePred, Actual = testData$DIQ010)
print(confusion)

plot(tree)
text(tree, use.n = TRUE, all = TRUE, cex = 0.8)




mod_train_data <- trainData[, -c(92)]
mod_test_data <- testData[, -c(92)]
tree <- rpart(DIQ010 ~ ., data = mod_train_data, method = "class")
print(tree)

# Predict tree again
treePred <- predict(tree, newdata = mod_test_data, type = "class")
confusion <- table(Predicted = treePred, Actual = mod_test_data$DIQ010)
print(confusion)

plot(tree)
text(tree, use.n = TRUE, all = TRUE, cex = 0.8)





##SVM

svmModel <- svm(DIQ010 ~ ., data = trainData, kernel = "linear")
summary(svmModel)


svmPred <- predict(svmModel, newdata = testData)
confusion <- table(Predicted = svmPred, Actual = testData$DIQ010)
print(confusion)


svmModel <- svm(DIQ010 ~ ., data = trainData, kernel = "sigmoid")
summary(svmModel)

svmPred <- predict(svmModel, newdata = testData)
confusion <- table(Predicted = svmPred, Actual = testData$DIQ010)
print(confusion)




data <- droplevels(data)
ggplot(data, aes(x = factor(DIQ010), y = LBXGH)) +
  geom_boxplot()


barplot(table(data$DIQ010), main="Frequency of Diabetes Diagnosis")
table(data$DIQ010)

table(filtered_data$DIQ010)


ggplot(data, aes(x = factor(DIQ010), y = RIDAGEYR)) +
  labs(
    title = "Age vs Diagnosis",
    x = "Diagnosis",
    y = "Age"
  ) +
  geom_boxplot()


#https://statsandr.com/blog/wilcoxon-test-in-r-how-to-compare-2-groups-under-the-non-normality-assumption/
test_result <- wilcox.test(
  RIDAGEYR ~ DIQ010,
  data = data
)
# Display the results
print(test_result)


ggplot(data, aes(x = factor(DIQ010), y = INDFMPIR)) +
  labs(
    title = "Poverty Level vs Diagnosis",
    x = "Diagnosis",
    y = "Income / Poverty Level"
  ) +
  geom_boxplot()


test_result <- wilcox.test(
  INDFMPIR ~ DIQ010,
  data = data
)
print(test_result)



ggplot(data, aes(x = factor(DIQ010), y = PAD680)) +
  labs(
    title = "Minutes of Sedentary Activity vs Diagnosis",
    x = "Diagnosis",
    y = "Minutes of Sedentary Activity Per Day"
  ) +
  geom_boxplot()


test_result <- wilcox.test(
  PAD680 ~ DIQ010,
  data = data
)
print(test_result)


by(data$PAD680, data$DIQ010, summary)




ggplot(data, aes(x = factor(DIQ010), y = DR1TCARB)) +
  labs(
    title = "Carbs Eatern Per Day vs Diagnosis",
    x = "Diagnosis",
    y = "Carbs Eaten per Day"
  ) +
  geom_boxplot()


test_result <- wilcox.test(
  DR1TCARB ~ DIQ010,
  data = data
)
print(test_result)


by(data$DR1TCARB, data$DIQ010, summary)



ggplot(data, aes(x = factor(DIQ010), y = DR1TSUGR)) +
  labs(
    title = "Sugar Eatern Per Day vs Diagnosis",
    x = "Diagnosis",
    y = "Sugar Eaten per Day"
  ) +
  geom_boxplot()


test_result <- wilcox.test(
  DR1TSUGR ~ DIQ010,
  data = data
)
print(test_result)


by(data$DR1TSUGR, data$DIQ010, summary)



ggplot(data, aes(x = factor(DIQ010), y = DR1TKCAL)) +
  labs(
    title = "Calories Eatern Per Day vs Diagnosis",
    x = "Diagnosis",
    y = "Calories Eaten per Day"
  ) +
  geom_boxplot()


test_result <- wilcox.test(
  DR1TKCAL ~ DIQ010,
  data = data
)
print(test_result)


by(data$DR1TKCAL, data$DIQ010, summary)



data_num <- data[, -c(1, 2)]
data_num <- na.omit(data_num)

principal_components <- princomp(data_num, cor = TRUE, score = TRUE)

summary(principal_components)
plot(principal_components)

plot(principal_components, type = "l")

biplot(principal_components)

biplot(principal_components, xlabs = rep(".", nrow(data_num)), scale = 0)

autoplot(principal_components, data = data_num, 
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

str(principal_components)




data_clean <- na.omit(data)
data_clean <-droplevels(data_clean)
rf_model <- randomForest(DIQ010 ~ ., data = data_clean, importance = TRUE, ntree=10000)
importance <- as.data.frame(varImpPlot(rf_model, type = 1))




