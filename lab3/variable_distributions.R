#Read in data
epi2024results <- read.csv("epi2024results_DA_F24_lab03.csv", header=TRUE)
epi <- epi2024results
attach(epi)

#Create subsets for two regions 
epi_gw <- epi[epi$region == 'Global West',]
epi_ssa <- epi[epi$region == 'Sub-Saharan Africa',]

## Find and drop colums with NA values for SPI
na.indexes <- is.na(epi_gw$SPI)
epi_gw.subset <- epi_gw[!na.indexes,]
na.indexes <- is.na(epi_ssa$SPI)
epi_ssa.subset <- epi_ssa[!na.indexes,]

## histograms with density lines
hist(epi_ssa$SPI, seq(0., 100., 5.0), prob=TRUE)
lines(density(epi_ssa$SPI,na.rm=TRUE,bw="SJ"))
hist(epi_gw$SPI, seq(0., 100., 5.0), prob=TRUE)
lines(density(epi_gw$SPI,na.rm=TRUE,bw="SJ"))


#QQplots of the two vars for the two regions
plot(qqnorm(epi_ssa$SPI))
qqline(epi_ssa$SPI)

plot(qqnorm(epi_gw$SPI))
qqline(epi_gw$SPI)

