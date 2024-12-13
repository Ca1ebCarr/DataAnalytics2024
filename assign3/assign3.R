library(ggplot2)

covid2020 <- read.csv("us-counties-2020.csv", header=TRUE)
covid2021 <- read.csv("us-counties-2021.csv", header=TRUE)
boxplot(list(CovidDeaths2020 = covid2020$deaths, CovidDeaths2021 = covid2021$deaths),
        main = "2020/2021 COVID Deaths by County", ylab = "Number of Deaths")
summary(covid2020$deaths)
summary(covid2021$deaths)
boxplot(list(CovidCases2020 = covid2020$cases, CovidCases2021 = covid2021$cases),
        main = "2020/2021 COVID Cases by County", ylab = "Number of Cases")
summary(covid2020$cases)
summary(covid2021$cases)






hist(covid2020$deaths, seq(0., 26000., 1000.0), prob=TRUE)
x_vals <- seq(0, 26000, length.out = 100)
lines(x_vals, dexp(x_vals, rate = 1/53.6), lwd = 2)

x_vals <- seq(0, 36000, length.out = 100)
hist(covid2021$deaths, seq(0., 36000., 1000.0), prob=TRUE)
lines(x_vals, dexp(x_vals, rate = 1/193.6), lwd = 2)

x_vals <- seq(0, 780000., length.out = 1000)
hist(covid2020$cases, seq(0., 780000., 10000.0), prob=TRUE)
lines(x_vals, dexp(x_vals, rate = 1/1952), lwd = 2)

x_vals <- seq(0, 1700000., length.out = 1000)
hist(covid2021$cases, seq(0., 1700000., 10000.0), prob=TRUE)
lines(x_vals, dexp(x_vals, rate = 1/11160), lwd = 2)


ecdf_deaths <- ecdf(covid2020$deaths)
plot(ecdf_deaths,  main = "ECDF of 2020 Covid Deaths",xlab = "2020 Covid Deaths", lwd = 2)

ecdf_deaths <- ecdf(covid2021$deaths)
plot(ecdf_deaths,  main = "ECDF of 2021 Covid Deaths",xlab = "2021 Covid Deaths", lwd = 2)


ecdf_deaths <- ecdf(covid2020$cases)
plot(ecdf_deaths,  main = "ECDF of 2020 Covid Cases",xlab = "2020 Covid Cases", lwd = 2)


ecdf_deaths <- ecdf(covid2021$cases)
plot(ecdf_deaths,  main = "ECDF of 2021 Covid Cases",xlab = "2021 Covid Cases", lwd = 2)

summary(covid2020$deaths)
# Your actual dataset (replace this with your data)
data <- covid2020$deaths
mean(covid2020$deaths)

lambda_hat <- 1 / 53.6
qqplot(qexp(ppoints(length(covid2020$deaths)), rate = lambda_hat), covid2020$deaths, 
       main = "Q-Q Plot for 2020 Covid Deaths Distribution")

lambda_hat <- 1 / 193.6
qqplot(qexp(ppoints(length(covid2021$deaths)), rate = lambda_hat), covid2021$deaths, 
       main = "Q-Q Plot for 2021 Covid Deaths Distribution")

lambda_hat <- 1 / 1952
qqplot(qexp(ppoints(length(covid2020$cases)), rate = lambda_hat), covid2020$cases, 
       main = "Q-Q Plot for 2020 Covid Cases Distribution")

lambda_hat <- 1 / 11160
qqplot(qexp(ppoints(length(covid2021$cases)), rate = lambda_hat), covid2021$cases, 
       main = "Q-Q Plot for 2021 Covid Cases Distribution")





#https://www.r-bloggers.com/2020/01/how-to-remove-outliers-in-r/
nyhouse <- read.csv("NY-House-Dataset.csv", header=TRUE)

lin.mod <- lm(PRICE ~ BEDS + BATH + PROPERTYSQFT, data = nyhouse)
summary(lin.mod)

plot(nyhouse$PROPERTYSQFT, nyhouse$PRICE, main="Property Square Footage vs Price", xlab="Square footage ", ylab="Price", pch=19)
#Line of best fit
oneVar <- lm(PRICE ~ PROPERTYSQFT, data = nyhouse)
summary(oneVar)
abline(a=-837809.4, b=1462.7) 

nyhouse <- nyhouse[-c(2,305),]
lin.mod <- lm(PRICE ~ BEDS + BATH + PROPERTYSQFT, data = nyhouse)
summary(lin.mod)
plot(nyhouse$PROPERTYSQFT, nyhouse$PRICE, main="Property Square Footage vs Price", xlab="Square footage ", ylab="Price", pch=19)
#Line of best fit
oneVar <- lm(PRICE ~ PROPERTYSQFT, data = nyhouse)
summary(oneVar)
abline(a=250170.17, b=743.16) 

nysubset <- nyhouse[nyhouse$BEDS < 5 & nyhouse$BATH < 5 & nyhouse$PROPERTYSQFT < 10000,]
lin.mod <- lm(PRICE ~ BEDS + BATH + PROPERTYSQFT, data = nysubset)
summary(lin.mod)
plot(nysubset$PROPERTYSQFT, nysubset$PRICE, main="Property Square Footage vs Price", xlab="Square footage ", ylab="Price", pch=19)
oneVar <- lm(PRICE ~ PROPERTYSQFT, data = nysubset)
summary(oneVar)
abline(a=-786886.28, b=1172.41) 

