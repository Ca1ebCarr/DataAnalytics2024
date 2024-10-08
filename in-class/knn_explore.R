####     KNN- Abalone     ####


# abalone dataset from UCI repository
# reading the dataset from UCI repository URL
abalone <- read.csv("abalone/abalone.data", header = FALSE, sep = ",")


# Column names
#c -combine values into a vector or list
colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght',
                       'viscera_wieght', 'shell_weight', 'rings' )
# summary on abalone
summary(abalone)
# structure of the abalone data
str(abalone)
# summary of the abalone rings column
summary(abalone$rings)


# As shown above, the “rings” variable has a range between 1-29.
# This is the variable that we want to predict, and predicting this many levels
# might not give us the insight we’re looking for.
# For now, we’ll break the rings variable
# into 3 levels" “young” for abalones less than 8, “adult” for abalones between 8-11,
# and “old” for abalones older than 11.
abalone$rings <- as.numeric(abalone$rings)

#Divide the data into intervals -1-8yo   8-11yo   11-35yo
#note: -1 is younger than the yougest and 35 is older than the oldest. They are somewhat arbitrary
abalone$rings <- cut(abalone$rings, br=c(-1,8,11,35), labels = c("young", 'adult', 'old'))

#Turn the numerical cuts into different factors (categories). Young, Adult, and Old
abalone$rings <- as.factor(abalone$rings)

summary(abalone$rings)
# remove the "sex" variable in abalone, because KNN requires all numeric variables for prediction #
z <- abalone
aba <- abalone
aba$sex <- NULL


# normalize the data using min max normalization
normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x))) }
aba[1:7] <- as.data.frame(lapply(aba[1:7], normalize))
summary(aba$shucked_wieght)
# After Normalization, each variable has a min of 0 and a max of 1.
# in other words, values are in the range from 0 to 1.


# We’ll now split the data into training and testing sets.
ind <- sample(2, nrow(aba), replace=TRUE, prob=c(0.7, 0.3))  #Make 4177 samples of 1 or 2
KNNtrain <- aba[ind==1,]  #the 1s become training
KNNtest <- aba[ind==2,]   #the 2s become the test data
sqrt(2918)


# make k equal to the square root of 2918, the number of observations in the training set.
# sqrt(2918) ~= 54.01852 round it to 55 and use k = 55
# We usually take an Odd number for k value,
# knn model
# knn() is in the "class" library. Make sure to install it first on your RStudio.
library(class)
help("knn") # Read the knn documentation on RStudio.
KNNpred <- knn(train = KNNtrain[1:7], test = KNNtest[1:7], cl =
                 KNNtrain$rings, k = 55)
KNNpred
table(KNNpred)

