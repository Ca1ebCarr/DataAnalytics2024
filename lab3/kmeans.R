#Required libraries
library(class)
library(ggplot2)

epi2024results <- read.csv("epi2024results_DA_F24_lab03.csv", header=TRUE)
epi <- epi2024results
attach(epi)


## plot dataset colored by class
ggplot(epi, aes(x = EPI, y = ECO, colour = region)) +
  geom_point()

## set random number generator start value
set.seed(123)

epi_sub <- epi[epi$region == 'Sub-Saharan Africa' | 
                  epi$region == 'Asia-Pacific' | 
                  epi$region == 'Global West', ]

na.indexes <- is.na(epi_sub$EPI) | is.na(epi_sub$ECO) | is.na(epi_sub$BDH) | is.na(epi_sub$MKP) | is.na(epi_sub$MHP)
epi.sub <- epi_sub[!na.indexes,]

epi.km <- kmeans(epi.sub[6:10], centers = 3)


## WCSS: total within cluster sum of squares
epi.km$tot.withinss

## get and plot clustering output 
assigned.clusters <- as.factor(epi.km$cluster)

ggplot(epi.sub, aes(x = EPI, y = ECO, colour = assigned.clusters)) +
  geom_point()



##SUBSET WSS
set.seed(300)  #random seed generator
k.max <-8
# tot.withinss = Total within-cluster sum of square
# iter.max = the maximum number of iterations allowed
# nstart = if centers is a number, how many random sets should be chosen.
wss<- sapply(1:k.max,function(k){kmeans(epi.sub[6:10],k,nstart = 20,iter.max = 20)$tot.withinss})


wss/nrow(epi.sub) # within sum of squares.
plot(1:k.max,wss/nrow(epi.sub), type= "b", xlab = "Number of clusters(k)", ylab = "Within cluster
sum of squares") 




epi_sub <- epi[epi$region == 'Former Soviet States' | 
                 epi$region == 'Eastern Europe' | 
                 epi$region == 'Greater Middle East', ]

na.indexes <- is.na(epi_sub$EPI) | is.na(epi_sub$ECO) | is.na(epi_sub$BDH) | is.na(epi_sub$MKP) | is.na(epi_sub$MHP)
epi.sub <- epi_sub[!na.indexes,]

epi.km <- kmeans(epi.sub[6:10], centers = 3)


## WCSS: total within cluster sum of squares
epi.km$tot.withinss  ##MUCH BETTER

## get and plot clustering output 
assigned.clusters <- as.factor(epi.km$cluster)

ggplot(epi.sub, aes(x = EPI, y = ECO, colour = assigned.clusters)) +
  geom_point()






##SUBSET2 WSS
set.seed(300)  #random seed generator
k.max <-8
# tot.withinss = Total within-cluster sum of square
# iter.max = the maximum number of iterations allowed
# nstart = if centers is a number, how many random sets should be chosen.
wss<- sapply(1:k.max,function(k){kmeans(epi.sub[6:10],k,nstart = 20,iter.max = 20)$tot.withinss})


wss # within sum of squares.
plot(1:k.max,wss, type= "b", xlab = "Number of clusters(k)", ylab = "Within cluster
sum of squares") 


#The second has the lower sum of squares, but this is just because there are less values. I think it would be good to look at the sum of squares sclaed by the number of points in the dataset. 

wss/nrow(epi.sub) # within sum of squares.
plot(1:k.max,wss/nrow(epi.sub), type= "b", xlab = "Number of clusters(k)", ylab = "Within cluster
sum of squares") 


#After doing scaling, it can be seen that as the cluster increase the WSS is lower for the second model, so it is better.

