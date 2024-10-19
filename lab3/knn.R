#Required libraries
library(class)

epi2024results <- read.csv("epi2024results_DA_F24_lab03.csv", header=TRUE)
epi <- epi2024results
attach(epi)

#Select 3 regions to train on. Picked the ones with the most
epi_sub <- epi[epi$region == 'Sub-Saharan Africa' | 
                 epi$region == 'Latin America & Caribbean' | 
                 epi$region == 'Global West', ]

#Pick a random subset of training indices
n = nrow(epi_sub)
train.indexes <- sample(n,n*.7)

#split into train and test observations
epi.train <-epi_sub[train.indexes,]
epi.test <-epi_sub[-train.indexes,]

#Set k to the number of regions selected
k <- 3

na.indexes <- is.na(epi.train$EPI) | is.na(epi.train$ECO) | is.na(epi.train$BDH) | is.na(epi.train$MKP) | is.na(epi.train$MHP)
epi.train.subset <- epi.train[!na.indexes,]
na.indexes <- is.na(epi.test$EPI) | is.na(epi.test$ECO) | is.na(epi.test$BDH) | is.na(epi.test$MKP) | is.na(epi.test$MHP)
epi.test <- epi.test[!na.indexes,]


#Get the predictions
KNNpred <- knn(train = epi.train.subset[6:10], test = epi.test[6:10], cl = epi.train.subset$region, k = k)

#make and print the contingency table
contingency.table <- table(Actual=KNNpred, Predicted = epi.test$region, dnn=list('predicted','actual'))

print(contingency.table)





#Select 3 regions to train on. Picked the ones with the most
epi_sub2 <- epi[epi$region == 'Sub-Saharan Africa' | 
                 epi$region == 'Asia-Pacific' | 
                 epi$region == 'Global West', ]

#Pick a random subset of training indices
n = nrow(epi_sub2)
train.indexes <- sample(n,n*.7)

#split into train and test observations
epi.train <-epi_sub2[train.indexes,]
epi.test <-epi_sub2[-train.indexes,]

#Set k to the number of regions selected
k <- 3

na.indexes <- is.na(epi.train$EPI) | is.na(epi.train$ECO) | is.na(epi.train$BDH) | is.na(epi.train$MKP) | is.na(epi.train$MHP)
epi.train.subset <- epi.train[!na.indexes,]
na.indexes <- is.na(epi.test$EPI) | is.na(epi.test$ECO) | is.na(epi.test$BDH) | is.na(epi.test$MKP) | is.na(epi.test$MHP)
epi.test <- epi.test[!na.indexes,]


#Get the predictions
KNNpred <- knn(train = epi.train.subset[6:10], test = epi.test[6:10], cl = epi.train.subset$region, k = k)

#make and print the contingency table
contingency.table <- table(Actual=KNNpred, Predicted = epi.test$region, dnn=list('predicted','actual'))

print(contingency.table)


##The second model has a much better confusion matrix. I think this is becauseThe regions are more dissimilar on a global scale, so it makes sense that their environmental data would also be able to be clustered better. 