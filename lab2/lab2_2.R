## Call the NaiveBayes Classifier Package e1071, which auto calls the Class package ##
library("e1071")
#Train classifier
classifier<-naiveBayes(iris[,1:4], iris[,5])

# evaluate classification

table(predict(classifier, iris[,-5]), iris[,5], dnn=list('predicted','actual'))

# examine class means and standard deviations for petal length
classifier$tables$Petal.Length
# plot normal distributions at the means of the classes
# one class
plot(function(x) dnorm(x, 1.462, 0.1736640), 0, 8, col="red", main="Petal length distribution for the 3 different species")
# another class
curve(dnorm(x, 4.260, 0.4699110), add=TRUE, col="blue")
# the final class
curve(dnorm(x, 5.552, 0.5518947 ), add=TRUE, col = "green")




abalone <- read.csv("abalone/abalone.data", header = FALSE, sep = ",")
colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght',
                       'viscera_wieght', 'shell_weight', 'rings' )
abalone$rings <- as.numeric(abalone$rings)
abalone$rings <- cut(abalone$rings, br=c(-1,8,11,35), labels = c("young", 'adult', 'old'))
abalone$rings <- as.factor(abalone$rings)

#Train classifier on length, diameter, and height
classifier<-naiveBayes(abalone[,2:4], abalone[,9])
table(predict(classifier, abalone[,-5]), abalone[,9], dnn=list('predicted','actual'))
classifier$tables

classifier$tables$diameter

#Young
plot(function(x) dnorm(x, 0.3212758, 0.09029187), 0, 0.7, col="red", main="Diameter length distribution for the 3 different age groups")
#Adult
curve(dnorm(x, 0.4458591, 0.07153798), add=TRUE, col="blue")
#Old
curve(dnorm(x, 0.4632083, 0.06699741 ), add=TRUE, col = "green")




#Train classifier on whole, shucked, viscera, and shell weight
classifier<-naiveBayes(abalone[,5:8], abalone[,9])
table(predict(classifier, abalone[,-5]), abalone[,9], dnn=list('predicted','actual'))
classifier$tables

classifier$tables$shell_weight

#Young
plot(function(x) dnorm(x, 0.1213945 , 0.08096481), 0, 1.1, col="red", main="Shell Weight distribution for the 3 different age groups")
#Adult
curve(dnorm(x, 0.2752133 , 0.10944254), add=TRUE, col="blue")
#Old
curve(dnorm(x, 0.3423526 , 0.13680125 ), add=TRUE, col = "green")





#Train classifier on height, whole weight, shell weight
classifier<-naiveBayes(abalone[,c(3,5,8)], abalone[,9])
classifier$tables
table(predict(classifier, abalone[,-5]), abalone[,9], dnn=list('predicted','actual'))


classifier$tables$whole_weight

#Young
plot(function(x) dnorm(x, 0.4323742  , 0.3060074), 0, 3, col="red", main="Whole Weight distribution for the 3 different age groups")
#Adult
curve(dnorm(x, 0.9850878  , 0.4264315), add=TRUE, col="blue")
#Old
curve(dnorm(x, 1.1148922  , 0.4563715 ), add=TRUE, col = "green")

