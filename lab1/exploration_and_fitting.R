EPI_data <- read.csv("epi2024results06022024.csv") 
View(EPI_data)
attach(EPI_data) 
EPI.new
tf <- is.na(EPI.new)
E <- EPI.new[!tf] 
summary(EPI.new)
fivenum(EPI.new,na.rm=TRUE) 
stem(EPI.new) 
hist(EPI.new)
hist(EPI.new, seq(20., 80., 1.0), prob=TRUE)
lines(density(EPI.new,na.rm=TRUE,bw="SJ"))
rug(EPI.new)
boxplot(EPI.new, APO.new)
x<-seq(20,80,1)
q<- dnorm(x,mean=42, sd=5,log=FALSE) 
lines(x,q)
lines(x,.4*q) 
q<-dnorm(x,mean=65, sd=5,log=FALSE) 
lines(x,.12*q) 
plot(ecdf(EPI.new), do.points=FALSE, verticals=TRUE) 
qqnorm(EPI.new); qqline(EPI.new) 
qqplot(rnorm(250), EPI.new, xlab = "Q-Q plot for norm dsn") 
qqline(EPI.new)
qqplot(rt(250, df = 5), EPI.new, xlab = "Q-Q plot for t dsn")
qqline(EPI.new)


# Exploration of PAR 
summary(PAR.new)
fivenum(PAR.new,na.rm=TRUE)
#initial histogram
hist(PAR.new)
#Add more bars
hist(PAR.new, seq(0., 100., 1.0), prob=TRUE)
#add lines
lines(density(PAR.new,na.rm=TRUE,bw=1))
rug(PAR.new)
#Add some more general lines
lines(density(PAR.new,na.rm=TRUE,bw=2))
lines(density(PAR.new,na.rm=TRUE,bw='SJ'))

#add distribution lines
x<-seq(0,1,0.01)
#https://stat.ethz.ch/R-manual/R-devel/library/stats/html/Beta.html
q<- dbeta(x , 0.5, 0.5, log = FALSE)
lines(x*100,q/75)

#CDF and QQ Plots
plot(ecdf(PAR.new), do.points=FALSE, verticals=TRUE) 
qqnorm(PAR.new); qqline(PAR.new) 
qqplot(rnorm(250), PAR.new, xlab = "Q-Q plot for norm dsn") 
qqline(PAR.new)

qqplot(rbeta(250, 0.5, 0.5), PAR.new, xlab = "Q-Q plot for beta dsn")
qqline(PAR.new)


# Exploration of BER 
summary(BER.new)
fivenum(BER.new,na.rm=TRUE)
#initial histogram
hist(BER.new)
#Add more bars
hist(BER.new, seq(0., 100., 1.0), prob=TRUE)
#add lines
lines(density(BER.new,na.rm=TRUE,bw=1))
rug(BER.new)
#Add some more general lines
lines(density(BER.new,na.rm=TRUE,bw=2))
lines(density(BER.new,na.rm=TRUE,bw='SJ'))

#add distribution lines
x<-seq(0,1,0.01)
#https://stat.ethz.ch/R-manual/R-devel/library/stats/html/Beta.html
q<- dbeta(x , 0.4, 0.4, log = FALSE)
lines(x*100,q/70)

#CDF and QQ Plots
plot(ecdf(BER.new), do.points=FALSE, verticals=TRUE) 
qqnorm(BER.new); qqline(BER.new) 
qqplot(rnorm(250), BER.new, xlab = "Q-Q plot for norm dsn") 
qqline(BER.new)

qqplot(rbeta(250, 0.5, 0.5), BER.new, xlab = "Q-Q plot for beta dsn")
qqline(BER.new)

