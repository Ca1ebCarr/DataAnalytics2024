library(ggfortify)
library(e1071)
library(class)


#############
### LAB 5 ###
#############


###########################################
## Support Vector Machine Classification ##
###########################################

#############################################################
# Train 2 SVM classifiers to predict the type of wine using # 
#  a subset of the other 13 variables. You may choose the   #
#  subset based on previous analysis. One using a linear    #
#  kernel and another of your choice. Use tune.svm to find  #
#            the optimum C and gamma values.                #
#############################################################

#Read in the Data
wine <- read.csv("C:/Users/carrc4/Documents/ITWS4600/DataAnalytics2024/lab5/wine.csv", header=TRUE)
wine$Class <- as.factor(wine$Class)

#Split the data
train.indexes <- sample(178,0.7*178)
train <- wine[train.indexes,]
test <- wine[-train.indexes,]

#Train and test the linear model
linear.tuned.svm <- tune.svm(Class~., data = train, kernel = 'linear',gamma = seq(1/2^nrow(wine),1, .01), cost = 2^seq(-6, 4, 2))
linear.tuned.svm

#Best Params: 
#        gamma     cost
# 2.610122e-54 0.015625

wine.svm.mod0 <- svm(Class ~ ., data = train, kernel = 'linear', gamma = 2.610122e-54, cost=0.015625)
test.pred <- predict(wine.svm.mod0, test)
cm = as.matrix(table(Actual = test$Class, Predicted = test.pred))
cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 
recall = diag / rowsums 
precision = diag / colsums
f1 = 2 * precision * recall / (precision + recall) 
data.frame(precision, recall, f1)

#Train and test the polynomial model
polynomial.tuned.svm <- tune.svm(Class~., data = train, kernel = 'polynomial',gamma = seq(1/2^nrow(wine),1, .01), cost = 2^seq(-6, 4, 2))
polynomial.tuned.svm

#Best Params: 
#        gamma     cost
#         0.63 0.015625

wine.svm.mod1 <- svm(Class ~ ., data = train, kernel = 'polynomial', gamma = 0.63, cost = 0.015625)
test.pred <- predict(wine.svm.mod1, test)
cm = as.matrix(table(Actual = test$Class, Predicted = test.pred))
cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 
recall = diag / rowsums 
precision = diag / colsums
f1 = 2 * precision * recall / (precision + recall) 
data.frame(precision, recall, f1)

############################################################
#  Choose another classification method (kNN, NaiveBayes,  #
# etc.) and train a classifier based on the same features. #
############################################################

#Train and evaluate knn
KNNpred <- knn(train = train[,-1], test = test[,-1], cl =
                 train$Class, k = sqrt(178))
cm = as.matrix(table(Actual = test$Class, Predicted = KNNpred))
cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 
recall = diag / rowsums 
precision = diag / colsums
f1 = 2 * precision * recall / (precision + recall) 
data.frame(precision, recall, f1)

#######################################################
# Compare the performance of the 2 models (Precision, #
#                 Recall, F1)                         #
#######################################################

#Linear about as good as Polynomial. Both far better than KNN


#######################################
## Support Vector Machine Regression ##
#######################################

###########################################################
# Train a SVM regression model to predict PRICE based on  #
# Square Footage and plot predicted price vs. real price. #
###########################################################

#Read in the Data
nyc <- read.csv("C:/Users/carrc4/Documents/ITWS4600/DataAnalytics2024/lab5/NY-House-Dataset.csv", header=TRUE)

#Regression with SVM
modelsvm = svm(nyc$PRICE~nyc$PROPERTYSQFT,nyc)

#Predict using SVM regression
predYsvm = predict(modelsvm, nyc)

plot(nyc$PROPERTYSQFT, nyc$PRICE, col = "red", pch=16)
points(nyc$PROPERTYSQFT, predYsvm, col = "blue", pch=16)


###########################################################
#  Train a linear model using the same formula and plot   #
#           predicted price vs. real price.               #
###########################################################

model.ln = lm(nyc$PRICE~nyc$PROPERTYSQFT)

#Predict using linear regression
predYln= predict(model.ln, nyc)

plot(nyc$PROPERTYSQFT, nyc$PRICE, col = "red", pch=16)
points(nyc$PROPERTYSQFT, predYln, col = "blue", pch=16)
