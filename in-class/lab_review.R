### EPI 2024 for Data Analytics ###

#setwd("~/Courses/Data Analytics/Fall24/labs/lab03/")

### read in data
epi2024results06022024 <- read.csv("epi2024results06022024.csv", header=TRUE)
# epi2024weights <- read.csv("epi2024weights.csv")

## copy variables
epi <- epi2024results06022024


attach(epi)  # Allows for using the $ for columns

## NA values
na.indexes <- is.na(MHP.new)  #List of true false. true is has an na value for MHP new

## drop NAs
epi.subset <- epi[!na.indexes,] # Keep all rows with False values above

# ## convert to numeric
# EPI.new <- as.numeric(EPI.new)

## summary stats
summary(epi.subset$MHP.new)


## histograms
hist(MHP.new)

hist(MHP.new, seq(0., 100., 5.0), prob=TRUE) # Bin size of 5. Gives better sense of local vals

rug(MHP.new) #Density lines underneath

lines(density(MHP.new,na.rm=TRUE,bw=1))  #extremely overfitted
lines(density(MHP.new,na.rm=TRUE,bw=4)) # Smoother
lines(density(MHP.new,na.rm=TRUE,bw="SJ")) #Smoothest and most accurate. Looks about 4

x <- seq(0., 100., 1.0)

plot(dnorm(x, mean=42, sd=10))  #returns actual distribution value
plot(pnorm(x, mean=42, sd=10))  #Gives the CDF of the normal distribution
qnorm(.50, mean=42, sd=10)     
rnorm(1000, mean=42, sd=10)


hist(EPI.new)

hist(EPI.new, seq(20., 80., 5.0), prob=TRUE)

rug(EPI.new)

lines(density(EPI.new,na.rm=TRUE,bw=1))
lines(density(EPI.new,na.rm=TRUE,bw="SJ"))


x <- seq(20., 80., 1.0)

plot(qqnorm(EPI.new))
qqline(EPI.new)   #Copare quantiles

plot(qqnorm(EPI.new[which(EPI.new<=58)]))
qqline(EPI.new[which(EPI.new<=58)])

qqplot(EPI.new, )



################

hist(ECO.new)

summary(ECO.new)

hist(ECO.new, seq(20., 90., 2.0), prob=TRUE)

rug(ECO.new)

lines(density(ECO.new,na.rm=TRUE,bw=1))
lines(density(ECO.new,na.rm=TRUE,bw="SJ"))


################

hist(APO.new)

summary(APO.new)

boxplot(APO.new)

APO.new.high <- APO.new[APO.new>35]

h0 <- hist(APO.new, seq(0., 100., 1.0), freq=FALSE)
h1 <- hist(APO.new.high, seq(0., 100., 1.0), freq=FALSE)

h1$density

rug(APO.new)

lines(density(APO.new,na.rm=TRUE,bw=1))
lines(density(APO.new,na.rm=TRUE,bw="SJ"))

x <- seq(5., 90., 5.0)

qn<- dnorm(x,mean=45, sd=10,log=FALSE)

lines(x,qn)
# lines(x,0.4*qn)

qn<- dnorm(x,mean=65, sd=20,log=FALSE)

lines(x,qn)
# lines(x,0.12*qn)


########################################################################

##### Naive Bayes #####

library("e1071")
library("ggplot2")

## read data
abalone <- read.csv("abalone/abalone.data", header=FALSE)

## rename columns
colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght', 'viscera_wieght', 'shell_weight', 'rings' ) 

## derive age group based in number of rings
## Create breaks and label them youngg adult and old
abalone$age.group <- cut(abalone$rings, br=c(-1,8,11,35), labels = c("young", 'adult', 'old'))

## alternative way
abalone$age.group <- NA      #Create a new column. populate it with all NA
abalone$age.group[abalone$rings<=8] <- "young"
abalone$age.group[abalone$rings>8 & abalone$rings<=11] <- "adult"
abalone$age.group[abalone$rings>11] <- "old"

## convert age group from character to factor
abalone$age.group <- as.factor(abalone$age.group)

## drop sex and number of rings
abalone <- abalone[,-c(1,9)]


# train classifier using all data
classifier<-naiveBayes(abalone[,1:7], abalone[,8])


## predict classes
prediction <- predict(classifier, abalone[,1:7])

## evaluate prediction
contingency.table <- table(prediction, abalone[,8], dnn=list('predicted','actual')) 

print(contingency.table)

## plot whole_weight using means and SDs from model
parameters <- classifier$tables$whole_weight

m1 <- parameters["young",1][[1]]
m2 <- parameters["adult",1][[1]]
m3 <- parameters["old",1][[1]]

sd1 <- parameters["young",2][[1]]
sd2 <- parameters["adult",2][[1]]
sd3 <- parameters["old",2][[1]]


plot(function(x) dnorm(x, m1, sd1), 0, 3, col="red", main="Petal length distribution for the 3 different species") 

curve(dnorm(x, m2, sd2), add=TRUE, col="blue") 
curve(dnorm(x, m3, sd3 ), add=TRUE, col = "green")


contingency.matrix = as.matrix(contingency.table)

sum(diag(contingency.matrix))/length(abalone[,8])


#####################################################################

########## kNN ###########

## number of rows
n = nrow(abalone)

## training set indexes
train.indexes <- sample(n,n*.7)

## create training/test sets
abalone.train <-abalone[train.indexes,]
abalone.test <-abalone[-train.indexes,]

sqrt(2924)

k = 55

## train knn model
KNNpred <- knn(train = abalone.train[1:7], test = abalone.test[1:7], cl = abalone.train$age.group, k = k)

## evaluate
contingency.table <- table(Actual=KNNpred, Predicted = abalone.test$age.group, dnn=list('predicted','actual'))

print(contingency.table)

contingency.matrix = as.matrix(contingency.table)

sum(diag(contingency.matrix))/length(abalone.test$age.group)


## run text with multiple k values
accuracy <- c()
ks <- seq(5,105,10)

for (k in ks) {
  
  KNNpred <- knn(train = abalone.train[1:7], test = abalone.test[1:7], cl = abalone.train$age.group, k = k)
  cm = as.matrix(table(Actual=KNNpred, Predicted = abalone.test$age.group, dnn=list('predicted','actual')))
  
  accuracy <- c(accuracy,sum(diag(cm))/length(abalone.test$age.group)) 
  
}

plot(ks,accuracy,type = "b")


#####################################################################

######## k-Means ###########


## plot dataset colored by class
ggplot(abalone, aes(x = length, y = whole_weight, colour = age.group)) +
  geom_point()

## set random number generator start value
set.seed(123)

## train kmeans
abalone.km <- kmeans(abalone[,-8], centers = 3)

## WCSS: total within cluster sum of squares
abalone.km$tot.withinss

## get and plot clustering output 
assigned.clusters <- as.factor(abalone.km$cluster)

ggplot(abalone, aes(x = length, y = whole_weight, colour = assigned.clusters)) +
  geom_point()


# ## experimental!!!
# labeled.clusters <- as.character(assigned.clusters)
# 
# labeled.clusters[labeled.clusters==1] <- "old"
# labeled.clusters[labeled.clusters==2] <- "adult"
# labeled.clusters[labeled.clusters==3] <- "young"
# 
# 
# table(labeled.clusters, abalone$age.group, dnn=list('predicted','actual'))


## run tests with multiple k values and plot WCSS
wcss <- c()
ks <- c(2,3,4,5)

for (k in ks) {
  
  abalone.km <- kmeans(abalone[,-8], centers = k)
  
  wcss <- c(wss,abalone.km$tot.withinss)
  
}

plot(ks,wcss,type = "b")



#### END ####