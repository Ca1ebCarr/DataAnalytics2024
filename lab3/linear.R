#Required libraries
library(class)
library(ggplot2)

epi2024results <- read.csv("epi2024results_DA_F24_lab03.csv", header=TRUE)
epi <- epi2024results
attach(epi)

linear.model <- lm(EPI ~ ECO + BDH + MKP + MHP + MPE, data = epi)
linear.model
summary(linear.model)

ggplot() + geom_point(aes(ECO,EPI)) + 
  geom_smooth(aes(ECO,EPI), method="lm", se=F)


epi_sub <- epi[epi$region == 'Global West', ]
linear.model <- lm(EPI ~ ECO + BDH + MKP + MHP + MPE, data = epi_sub)
linear.model
summary(linear.model)

#The second model has a worse fit. 
#would assume this is because the model is better with more datapoints

