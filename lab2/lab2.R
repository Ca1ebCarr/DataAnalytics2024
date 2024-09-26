###### Exercise 1 Example and Explanation ######

### Load in data ###
epi.results <- read.csv("epi2024results06022024.csv", header=TRUE)
epi.weights <- read.csv("epi2024weights.csv")
epi.results$EPI.new
epi.results[1,5]
attach(epi.results)

### Quantile-Quantile Exploration ###
help("qqnorm") 
qqnorm(EPI.new)  #Makes a normal qq plot

#"adds a line to a “theoretical”, by default normal, quantile-quantile plot 
#which passes through the probs quantiles, by default the first and third 
#quartiles."
qqline(EPI.new)  

### Make a Q-Q plot against the generating distribution ###
x <- seq(20., 80., 1.0)   # The integers 20-80
qqplot(qnorm(ppoints(200)), x) # Plots a normal distribution against the quartiles of x
qqline(x) #Draw the line of best fit. Best case is that all data is on line == data is normal
qqplot(qnorm(ppoints(200)),EPI.new) 
qqline(EPI.new)  #Is almost normal, but not quite.


### Cumulative density function ###
plot(ecdf(EPI.new), do.points=FALSE) # Maked a cdf of the data
plot(ecdf(rnorm(1000, 45, 10)), do.points=FALSE) # ecdf of normal distr with mean=45, sd= 10
lines(ecdf(EPI.new)) #Checks data against the normal cdf


###### Exercise 1: Fitting a Distribution ######
epi.results$TBN.old
epi.results[1,5]
attach(epi.results)

### Quantile-Quantile Exploration ###
qqnorm(TBN.old)  #Makes a normal qq plot

#"adds a line to a “theoretical”, by default normal, quantile-quantile plot 
#which passes through the probs quantiles, by default the first and third 
#quartiles."
qqline(TBN.old)  

### Make a Q-Q plot against the generating distribution ###
x <- seq(20., 80., 1.0)   # The integers 20-80
qqplot(qnorm(ppoints(200)), x) # Plots a normal distribution against the quartiles of x
qqline(x) #Draw the line of best fit. Best case is that all data is on line == data is normal
qqplot(qnorm(ppoints(200)),TBN.old) 
qqline(TBN.old)  #Is almost normal, but not quite.


### Cumulative density function ###
mean(TBN.old)
sd(TBN.old)
plot(ecdf(TBN.old), do.points=FALSE) # Maked a cdf of the data
# ecdf of near perfect normal distribution basedon TBN data
plot(ecdf(rnorm(1000, mean(TBN.old),sd(TBN.old))), do.points=FALSE) 
lines(ecdf(TBN.old)) #Checks data against the normal cdf



epi.results$ECO.new
epi.results[1,5]
attach(epi.results)

### Quantile-Quantile Exploration ###
qqnorm(ECO.new)  #Makes a normal qq plot

#"adds a line to a “theoretical”, by default normal, quantile-quantile plot 
#which passes through the probs quantiles, by default the first and third 
#quartiles."
qqline(ECO.new)  

### Make a Q-Q plot against the generating distribution ###
x <- seq(20., 80., 1.0)   # The integers 20-80
qqplot(qnorm(ppoints(200)), x) # Plots a normal distribution against the quartiles of x
qqline(x) #Draw the line of best fit. Best case is that all data is on line == data is normal
qqplot(qnorm(ppoints(200)),ECO.new) 
qqline(ECO.new)  #Is almost normal, but not quite.


### Cumulative density function ###
mean(ECO.new)
sd(ECO.new)
plot(ecdf(ECO.new), do.points=FALSE) # Maked a cdf of the data
# ecdf of near perfect normal distribution basedon ECO.new data
plot(ecdf(rnorm(1000, mean(ECO.new),sd(ECO.new))), do.points=FALSE) 
lines(ecdf(ECO.new)) #Checks data against the normal cdf









####### Exercise 2: Example and Explanation #######
# read data
populations_2023 <- read.csv("countries_populations_2023.csv")
# drop countries not in epi results
populations <- populations_2023[-which(!populations_2023$Country %in% epi.results$country),]
# sort populations by country
populations <- populations[order(populations$Country),]
# drop countries not in populations
epi.results.sub <- epi.results[-which(!epi.results$country %in% populations$Country),]
# sort epi results by country
epi.results.sub <- epi.results.sub[order(epi.results.sub$country),]
# only keep necessary columns
epi.results.sub <- epi.results.sub[,c("country","EPI.old","EPI.new")]
# convert population to numeric
epi.results.sub$population <- as.numeric(populations$Population)
# compute population log base 10
epi.results.sub$population_log <- log10(epi.results.sub$population)

### Linear Model ###
lin.mod.epinew <- lm(EPI.new~population_log,epi.results.sub) #Create a linear model object
plot(epi.results.sub$population_log)
abline(lin.mod.epinew)
summary(lin.mod.epinew)
plot(lin.mod.epinew)
ggplot(epi.results.sub, aes(x = population_log, y = EPI.new)) +
  geom_point() +
  stat_smooth(method = "lm")
ggplot(lin.mod.epinew, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')




####### Exercise 2  #######
eco <- read.csv("epi2024results06022024.csv", header=TRUE)
eco.weights <- read.csv("epi2024weights.csv")
eco.results$ECO.new
eco.results[1,5]
attach(eco.results)


populations_2023 <- read.csv("countries_populations_2023.csv")
populations <- populations_2023[-which(!populations_2023$Country %in% eco.results$country),]
populations <- populations[order(populations$Country),]
eco.results.sub <- eco.results[-which(!eco.results$country %in% populations$Country),]
eco.results.sub <- eco.results.sub[order(eco.results.sub$country),]
eco.results.sub <- eco.results.sub[,c("country","ECO.old","ECO.new")]
eco.results.sub$population <- as.numeric(populations$Population)
eco.results.sub$population_log <- log10(eco.results.sub$population)

### Linear Model ###
lin.mod.econew <- lm(ECO.new~population_log,eco.results.sub) #Create a linear model object
plot(eco.results.sub$population_log)
abline(lin.mod.econew)
summary(lin.mod.econew)
plot(lin.mod.econew)
ggplot(eco.results.sub, aes(x = population_log, y = ECO.new)) +
  geom_point() +
  stat_smooth(method = "lm")
ggplot(lin.mod.econew, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')










mkp.results <- read.csv("epi2024results06022024.csv", header=TRUE)
mkp.weights <- read.csv("epi2024weights.csv")
mkp.results$MKP.new
mkp.results[1,5]
attach(mkp.results)


populations_2023 <- read.csv("countries_populations_2023.csv")
populations <- populations_2023[-which(!populations_2023$Country %in% mkp.results$country),]
populations <- populations[order(populations$Country),]
mkp.results.sub <- mkp.results[-which(!mkp.results$country %in% populations$Country),]
mkp.results.sub <- mkp.results.sub[order(mkp.results.sub$country),]
mkp.results.sub <- mkp.results.sub[,c("country","MKP.old","MKP.new")]
mkp.results.sub$population <- as.numeric(populations$Population)
mkp.results.sub$population_log <- log10(mkp.results.sub$population)

### Linear Model ###
lin.mod.mkpnew <- lm(MKP.new~population_log,mkp.results.sub) #Create a linear model object
plot(mkp.results.sub$population_log)
abline(lin.mod.mkpnew)
summary(lin.mod.mkpnew)
plot(lin.mod.mkpnew)
ggplot(mkp.results.sub, aes(x = population_log, y = MKP.new)) +
  geom_point() +
  stat_smooth(method = "lm")
ggplot(lin.mod.mkpnew, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')
