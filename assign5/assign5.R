library(class)


nyc <- read.csv("C:/Users/carrc4/Documents/ITWS4600/DataAnalytics2024/assign5/NYC_Citywide_Annualized_Calendar_Sales_Update_20241112.csv", header = TRUE)
manhattan <- nyc[nyc$BOROUGH==1 | nyc$BOROUGH=="MANHATTAN",]
bronx <- nyc[nyc$BOROUGH==2 | nyc$BOROUGH=="BRONX",]
brooklyn <- nyc[nyc$BOROUGH==3 | nyc$BOROUGH=="BROOKLYN",]
queens <- nyc[nyc$BOROUGH==4 | nyc$BOROUGH=="QUEENS",]
staten.island <- nyc[nyc$BOROUGH==5 | nyc$BOROUGH=="STATEN ISLAND",]

## Choose to work with Queens because it has the most observations


plot(queens$Longitude[!is.na(queens$Longitude) & !is.na(queens$Latitude)],
     queens$Latitude[!is.na(queens$Longitude) & !is.na(queens$Latitude)],
     xlab = "Longitude", ylab = "Latitude", asp = 1)

plot(staten.island$Longitude[!is.na(staten.island$Longitude) & !is.na(staten.island$Latitude)],
     staten.island$Latitude[!is.na(staten.island$Longitude) & !is.na(staten.island$Latitude)],
     xlab = "Longitude", ylab = "Latitude", asp = 1)

plot(brooklyn$Longitude[!is.na(brooklyn$Longitude) & !is.na(brooklyn$Latitude)],
     brooklyn$Latitude[!is.na(brooklyn$Longitude) & !is.na(brooklyn$Latitude)],
     xlab = "Longitude", ylab = "Latitude", asp = 1)

plot(manhattan$Longitude[!is.na(manhattan$Longitude) & !is.na(manhattan$Latitude)],
     manhattan$Latitude[!is.na(manhattan$Longitude) & !is.na(manhattan$Latitude)],
     xlab = "Longitude", ylab = "Latitude", asp = 1)

plot(bronx$Longitude[!is.na(bronx$Longitude) & !is.na(bronx$Latitude)],
     bronx$Latitude[!is.na(bronx$Longitude) & !is.na(bronx$Latitude)],
     xlab = "Longitude", ylab = "Latitude", asp = 1)

##########################################
queens <- queens[!is.na(queens$SALE.PRICE),]
queens <- queens[!is.na(queens$RESIDENTIAL.UNITS),]
queens <- queens[!is.na(queens$COMMERCIAL.UNITS),]
queens <- queens[!is.na(queens$LAND.SQUARE.FEET),]
queens <- queens[!is.na(queens$GROSS.SQUARE.FEET),]
queens <- queens[!is.na(queens$YEAR.BUILT),]
queens <- queens[!is.na(queens$TAX.CLASS.AT.TIME.OF.SALE),]
queens <- queens[!is.na(queens$ZIP.CODE),]
queens <- queens[!is.na(queens$Longitude),]
queens <- queens[!is.na(queens$Latitude),]

hist(queens$RESIDENTIAL.UNITS)
hist(queens$COMMERCIAL.UNITS)
hist(queens$TOTAL.UNITS)
hist(queens$YEAR.BUILT)
hist(queens$SALE.PRICE)



queens <- queens[queens$SALE.PRICE >=500, ]
queens <- queens[queens$YEAR.BUILT >=1900, ]
queens$LAND.SQUARE.FEET <- as.numeric(gsub(",", "", queens$LAND.SQUARE.FEET))
queens$GROSS.SQUARE.FEET <- as.numeric(gsub(",", "", queens$GROSS.SQUARE.FEET))
queens <- queens[!is.na(queens$LAND.SQUARE.FEET),]
queens <- queens[!is.na(queens$GROSS.SQUARE.FEET),]
queens <- queens[queens$LAND.SQUARE.FEET >0 & queens$GROSS.SQUARE.FEET >0, ]
hist(queens$YEAR.BUILT)
hist(queens$SALE.PRICE)

summary(queens$SALE.PRICE)

iqr <- 975000 -520000
up <-  975000 +1.5*iqr # Upper Range  
low<- 520000-1.5*iqr # Lower Range

queens.clean <- queens[queens$SALE.PRICE>low & queens$SALE.PRICE<up, ]

summary(queens.clean$SALE.PRICE)

hist(queens.clean$SALE.PRICE)
boxplot(queens$SALE.PRICE, queens.clean$SALE.PRICE, log = "y",
        main = "Boxplot with Logarithmic Scale",
        ylab = "Values (log scale)")

boxplot(queens.clean$SALE.PRICE,
        main = "Boxplot with Logarithmic Scale",
        ylab = "Values (log scale)")
#################################################


lin.mod <- lm(SALE.PRICE ~ BLOCK + LOT + ZIP.CODE + RESIDENTIAL.UNITS + COMMERCIAL.UNITS + LAND.SQUARE.FEET + GROSS.SQUARE.FEET + YEAR.BUILT + TAX.CLASS.AT.TIME.OF.SALE + Latitude + Longitude + Community.Board + Council.District + Census.Tract + BIN + BBL , data = queens.clean)
summary(lin.mod)


lin.mod <- lm(SALE.PRICE ~ RESIDENTIAL.UNITS + COMMERCIAL.UNITS + 
                LAND.SQUARE.FEET + GROSS.SQUARE.FEET + YEAR.BUILT + 
                TAX.CLASS.AT.TIME.OF.SALE + Longitude + Latitude, 
              data = queens.clean)
summary(lin.mod)


queens.old <- queens.clean[queens.clean$YEAR.BUILT<1940 
                           & !is.na(queens.clean$Latitude
                           & !is.na(queens.clean$Longitude)) , ]
sample_index <- sample(seq_len(nrow(queens.old)), size = 4000)
queens.old <- queens.old[sample_index, ]
predictions <- predict(lin.mod, newdata = queens.old)
rmse.old <- sqrt(mean((predictions - queens.old$SALE.PRICE)^2))
rmse.old

queens.new <- queens.clean[queens.clean$YEAR.BUILT>2000, ]
sample_index <- sample(seq_len(nrow(queens.new)), size = 4000)
queens.new <- queens.new[sample_index, ]
predictions <- predict(lin.mod, newdata = queens.new)
rmse.new <- sqrt(mean((predictions - queens.new$SALE.PRICE)^2))
rmse.new
###############################################################

install.packages("dplyr")
library(dplyr)




#https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/ifelse
queens.clean$IS_TAX_CLASS_1 <- factor(ifelse(queens.clean$TAX.CLASS.AS.OF.FINAL.ROLL == "1", 1, 0))

#https://www.geeksforgeeks.org/stratified-sampling-in-r/
queens.clean.strat <-strat_sample <- queens.clean %>% group_by(IS_TAX_CLASS_1) %>% sample_n(size=3000)

n = nrow(queens.clean.strat)
train.indexes <- sample(n,n*.7)

#split into train and test observations
queens.train <-queens.clean.strat[train.indexes,]
queens.test <-queens.clean.strat[-train.indexes,]
k <- 5

# Run k-NN with queens.train as the training data and queens.test as the test data
KNNpred <- knn(train = queens.train[c("SALE.PRICE", "RESIDENTIAL.UNITS", "COMMERCIAL.UNITS", 
                                        "LAND.SQUARE.FEET", "GROSS.SQUARE.FEET", "YEAR.BUILT", 
                                        "TAX.CLASS.AT.TIME.OF.SALE", "Longitude", "Latitude")], 
               test = queens.test[c("SALE.PRICE", "RESIDENTIAL.UNITS", "COMMERCIAL.UNITS", 
                                        "LAND.SQUARE.FEET", "GROSS.SQUARE.FEET", "YEAR.BUILT", 
                                        "TAX.CLASS.AT.TIME.OF.SALE", "Longitude", "Latitude")], 
               cl = queens.train$IS_TAX_CLASS_1, k = k)

# Create the contingency table with actual and predicted values
cm <- table(Actual = queens.test$IS_TAX_CLASS_1, Predicted = KNNpred)
cm
# Print the contingency table
n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 
precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 
data.frame(recall, precision, f1)

table(queens.train$IS_TAX_CLASS_1)


################
library("caret")
library(e1071)
queens.train <- queens.train[c("SALE.PRICE", "RESIDENTIAL.UNITS", "COMMERCIAL.UNITS", 
               "LAND.SQUARE.FEET", "GROSS.SQUARE.FEET", "YEAR.BUILT", 
               "TAX.CLASS.AT.TIME.OF.SALE", "Longitude", "Latitude", "IS_TAX_CLASS_1")]

svm.mod0 <- svm(IS_TAX_CLASS_1 ~ ., data = queens.train, kernel = 'linear')
SVMpred <- predict(svm.mod0, queens.test)
cm = as.matrix(table(Actual = queens.test$IS_TAX_CLASS_1, Predicted = SVMpred))
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








############################## Part II #########################################
#### Cleaning
nyc <- nyc[!is.na(nyc$SALE.PRICE),]
nyc <- nyc[!is.na(nyc$RESIDENTIAL.UNITS),]
nyc <- nyc[!is.na(nyc$COMMERCIAL.UNITS),]
nyc <- nyc[!is.na(nyc$LAND.SQUARE.FEET),]
nyc <- nyc[!is.na(nyc$GROSS.SQUARE.FEET),]
nyc <- nyc[!is.na(nyc$YEAR.BUILT),]
nyc <- nyc[!is.na(nyc$TAX.CLASS.AT.TIME.OF.SALE),]
nyc <- nyc[!is.na(nyc$ZIP.CODE),]
nyc <- nyc[!is.na(nyc$Longitude),]
nyc <- nyc[!is.na(nyc$Latitude),]
nyc <- nyc[nyc$SALE.PRICE >=500, ]
nyc <- nyc[nyc$YEAR.BUILT >=1900, ]
nyc$LAND.SQUARE.FEET <- as.numeric(gsub(",", "", nyc$LAND.SQUARE.FEET))
nyc$GROSS.SQUARE.FEET <- as.numeric(gsub(",", "", nyc$GROSS.SQUARE.FEET))
nyc <- nyc[!is.na(nyc$LAND.SQUARE.FEET),]
nyc <- nyc[!is.na(nyc$GROSS.SQUARE.FEET),]
nyc <- nyc[nyc$LAND.SQUARE.FEET >0 & nyc$GROSS.SQUARE.FEET >0, ]
nyc <- nyc[nyc$SALE.PRICE>low & nyc$SALE.PRICE<up, ]

###

lin.mod <- lm(SALE.PRICE ~ RESIDENTIAL.UNITS + COMMERCIAL.UNITS + 
                LAND.SQUARE.FEET + GROSS.SQUARE.FEET + YEAR.BUILT + 
                TAX.CLASS.AT.TIME.OF.SALE + Longitude + Latitude, 
              data = queens.clean)
predictions <- predict(lin.mod, newdata = nyc)
plot(log(nyc$SALE.PRICE), log(predictions))

valid_indices <- log(nyc$SALE.PRICE) >= 12 & 
                 log(nyc$SALE.PRICE) <= 15 &
                  log(predictions) >= 12 & 
                  log(predictions) <= 15
filtered_sale_price <- log(nyc$SALE.PRICE)[valid_indices]
filtered_predictions <- log(predictions)[valid_indices]
plot(filtered_sale_price, filtered_predictions)






#https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/ifelse
nyc$IS_TAX_CLASS_1 <- factor(ifelse(nyc$TAX.CLASS.AS.OF.FINAL.ROLL == "1", 1, 0))

nyc.strat <- nyc.strat[c("SALE.PRICE", "RESIDENTIAL.UNITS", "COMMERCIAL.UNITS", 
                               "LAND.SQUARE.FEET", "GROSS.SQUARE.FEET", "YEAR.BUILT", 
                               "TAX.CLASS.AT.TIME.OF.SALE", "Longitude", "Latitude", "IS_TAX_CLASS_1")]

#https://www.geeksforgeeks.org/stratified-sampling-in-r/
nyc.strat <-strat_sample <- nyc.strat %>% group_by(IS_TAX_CLASS_1) %>% sample_n(size=10000)

SVMpredNYC <- predict(svm.mod0, nyc.strat)
cm = as.matrix(table(Actual = nyc.strat$IS_TAX_CLASS_1, Predicted = SVMpredNYC))
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
