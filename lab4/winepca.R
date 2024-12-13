#################################################
### Lab 4: Principal Component Analysis (PCA) ###
#################################################

library(ggfortify)
library(class)
library("caret")
library(e1071)


##########################################################
# Compute the PCs and plot the dataset using the 1st and # 
#                           2nd PC.                      #
##########################################################

#Load the wine dataset
wines <- read.csv("C:/Users/carrc4/Documents/ITWS4600/DataAnalytics2024/lab4/wine/wine.csv", header=TRUE)

#Do Principal Components on the original wine dataset
principal_components <- princomp(wines[,2:14], cor = TRUE, score = TRUE)
summary(principal_components)

#Plot the principal components
plot(principal_components)
plot(principal_components, type = "l")
biplot(principal_components)
## using autoplot() function to plot the components
autoplot(principal_components, data = wines, 
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

principal_components$loadings


##########################################################
# Identify the variables that contribute the most to the #
#                         1st PC.                        #
##########################################################

#Flavanoids, Total.Phenols, OD280.OD315.of.diluted.wines, proanthocyanins



###########################################################
# Train a classifier model to predict wine type using the #
#                        13 attributes.                   #
###########################################################

train.indexes <- sample(178,0.7*178)
train <- wines[train.indexes,]
test <- wines[-train.indexes,]
train$Class <- as.factor(train$Class)
test$Class <- as.factor(test$Class)

## separate x (features) & y (class labels)
x <- wines[,2:14] 
y <- wines[,1]
histogram(y)


## train SVM model - linear kernel
svm.mod0 <- svm(Class ~ ., data = train, kernel = 'linear')
svm.mod0

test.pred <- predict(svm.mod0, test)
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


###########################################################
# Train a classifier model to predict wine type using the #
#          data projected into the first 3PCs             #
###########################################################

####### SVM Classifier on 3 Components ####### 
train.indexes <- sample(178,0.7*178)
wines3 <- principal_components$scores[,c(1,2,3)]
train <- wines3[train.indexes,]
test <- wines3[-train.indexes,]
labels <- as.matrix(y)
trainClass <- as.factor(labels[train.indexes,])
testClass <- as.factor(labels[-train.indexes,])


## train SVM model - linear kernel
svm.mod0 <- svm(trainClass ~ ., data = train, kernel = 'linear')

svm.mod0

test.pred <- predict(svm.mod0, test)

cm = as.matrix(table(Actual = testClass, Predicted = test.pred))

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





####### Drop the variables least contributing to the 1st PC and rerun PCA  #########


newWines <- wines[-c(2,4,6,11)]


principal_components <- princomp(newWines[,2:10], cor = TRUE, score = TRUE)

summary(principal_components)

plot(principal_components)
plot(principal_components, type = "l")
biplot(principal_components)
## using autoplot() function to plot the components
autoplot(principal_components, data = newWines, 
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)




#######  Train a classifier model to predict wine type using the data projected into the first 3 PCs after rerunning PCA   #########

train.indexes <- sample(178,0.7*178)
wines3 <- principal_components$scores[,c(1,2,3)]
train <- wines3[train.indexes,]
test <- wines3[-train.indexes,]
labels <- as.matrix(y)
trainClass <- as.factor(labels[train.indexes,])
testClass <- as.factor(labels[-train.indexes,])


## train SVM model - linear kernel
svm.mod0 <- svm(trainClass ~ ., data = train, kernel = 'linear')

svm.mod0

test.pred <- predict(svm.mod0, test)

cm = as.matrix(table(Actual = testClass, Predicted = test.pred))

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

