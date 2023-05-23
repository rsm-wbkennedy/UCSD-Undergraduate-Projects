# Set Working Directory

setwd("C:/Users/oklah/OneDrive/Spring23/Econ 112/Homework/Writing Assignment 1")

# load all the libraries needed

library(dyn)
library(forecast)
library(sandwich)
library(lmtest)
library(openxlsx)
library(tseries)
library(ggplot2)
library(stats)

# load data into workspace

data.cpi=read.table("CPI.csv",header=T, sep=",")
data.uer=read.table("Unemployment Quarterly.csv",header=T, sep=",")


#_______________________________________________________________________________
#-------------------BOX-JENKINS PROCEDURE: Inflation Rate-----------------------
#_______________________________________________________________________________


#CONSUMER PRICE INDEX

#tell R data is time series

cpindex=ts(data.cpi[,2],start=1960,frequency=12)


# turn it into quarterly data

cpiindex.q<- aggregate(cpindex, nfrequency = 4, FUN='mean')

summary(cpiindex.q)

#compute annualized quarterly inflation rate

Inflation = 100*diff(log(cpiindex.q), lag=4)

#plot growth rate

plot(Inflation, main="Portugal CPI Inflation",xlab="Time",type="l")

#summary statistics

summary(Inflation)

#give data a more convenient notation

y1 <- Inflation

# STEP 1: IDENTIFICATION--------------------------------------------------------

#plot data, ACF and PACF to get an idea of model class

plot(y1, main="Portugal CPI Inflation, Quarterly", xlab="Time", type = "l")

acf(y1,main="Autocorrelation of Inflation, Portugal")

pacf(y1,main="Partial Autocorrelation Inflation, Portugal")

# STEP 2: ESTIMATION------------------------------------------------------------

# compute estimates of models under considerations and AIC and BIC

#Coefficient Test

AR.1.MA.0.y1<-arima(y1,order=c(1,0,0))
coeftest(AR.1.MA.0.y1,vcv=NeweyWest)

AR.2.MA.0.y1<-arima(y1,order=c(2,0,0))
coeftest(AR.2.MA.0.y1,vcv=NeweyWest)

AR.1.MA.1.y1<-arima(y1,order=c(1,0,1))
coeftest(AR.1.MA.1.y1,vcv=NeweyWest)

AR.1.MA.2.y1<-arima(y1,order=c(1,0,2))
coeftest(AR.1.MA.2.y1,vcv=NeweyWest)

AR.1.MA.3.y1<-arima(y1,order=c(1,0,3))
coeftest(AR.1.MA.3.y1,vcv=NeweyWest)

AR.1.MA.4.y1<-arima(y1,order=c(1,0,4))
coeftest(AR.1.MA.4.y1,vcv=NeweyWest)

AR.1.MA.5.y1<-arima(y1,order=c(1,0,5))
coeftest(AR.1.MA.5.y1,vcv=NeweyWest)

AR.2.MA.1.y1<-arima(y1,order=c(2,0,1))
coeftest(AR.2.MA.1.y1,vcv=NeweyWest)

AR.2.MA.2.y1<-arima(y1,order=c(2,0,2))
coeftest(AR.2.MA.2.y1,vcv=NeweyWest)

AR.2.MA.3.y1<-arima(y1,order=c(2,0,3))
coeftest(AR.2.MA.3.y1,vcv=NeweyWest)

AR.2.MA.4.y1<-arima(y1,order=c(2,0,4))
coeftest(AR.2.MA.4.y1,vcv=NeweyWest)

AR.2.MA.5.y1<-arima(y1,order=c(2,0,5))
coeftest(AR.2.MA.5.y1,vcv=NeweyWest)

AIC(AR.1.MA.4.y1,AR.1.MA.5.y1,AR.2.MA.3.y1,AR.2.MA.4.y1)
BIC(AR.1.MA.4.y1,AR.1.MA.5.y1,AR.2.MA.3.y1,AR.2.MA.4.y1)

#Create a Table for Results

models.y1 <- list(
  AR1MA4 = arima(y1, order=c(1,0,4)),
  AR1MA5 = arima(y1, order=c(1,0,5)),
  AR2MA3 = arima(y1, order=c(2,0,3)),
  AR2MA4 = arima(y1, order=c(2,0,4))
)

aics.y1 <- sapply(models.y1, AIC)
bics.y1 <- sapply(models.y1, BIC)

table.y1.aic <- data.frame(Model = names(models.y1), AIC = aics.y1)
table.y1.bic <- data.frame(Model = names(models.y1), BIC = bics.y1)

# STEP 3: DIAGNOSTIC CHECKING---------------------------------------------------

#perform diagnostics of chosen model: ARMA(1,4) 

AR.1.MA.4.eigen<-polyroot(c(-.69,.98, 1))
#If the value of the roots is more than 1 then it is not stationary


AR.1.MA.4.res<-residuals(AR.1.MA.4.y1)
plot(AR.1.MA.4.res, main="Residuals of ARMA(1,4) Estimates", xlab="Time", type = "l", col="blue")

acf(AR.1.MA.4.res, main="Autocorrelation of Residuals for ARMA(1,4)")

# plot histogram of residuals and contrast with Gaussian distribution

x1 <- AR.1.MA.4.res
h1<-hist(x1, breaks=40, col="red", xlab="Inflation Fit Residuals", 
        main="Histogram with Normal Curve") 
x1fit<-seq(min(x1),max(x1),length=40) 
y1fit<-dnorm(x1fit,mean=mean(x1),sd=sd(x1)) 
y1fit <- y1fit*diff(h1$mids[1:2])*length(x1) 
lines(x1fit, y1fit, col="blue", lwd=2)

# Plot Fitted Values
AR.1.MA.4.fit<-fitted(AR.1.MA.4.y1)

plot(y1, main="Fitted ARMA(1,4) Estimates", xlab="Time", type = "l", col="blue")
lines(AR.1.MA.4.fit, col="red")

# forecast using the preferred model

plot(forecast(AR.1.MA.4.y1,h=100))



#_______________________________________________________________________________
#-------------------BOX-JENKINS PROCEDURE: Unemployment Rate--------------------
#_______________________________________________________________________________

#UNEMPLOYMENT RATE

#tell R data is time series

urate=ts(data.uer[,2],start=1998,frequency=4)

#plot data

plot(urate, main="Quarterly Unemployment Rate", xlab="Time", type = "l")

#summary statistics

summary(urate)

#give data a more convenient notation

y2 <- urate

# STEP 1: IDENTIFICATION--------------------------------------------------------

#plot data, ACF and PACF to get an idea of model class

plot(y2, main="Portugal Unemployment Rate, Quarterly", xlab="Time", type = "l")

acf(y2,main="Autocorrelation of Unemployment, Portugal")

pacf(y2,main="Partial Autocorrelation Unemployment, Portugal")

# STEP 2: ESTIMATION------------------------------------------------------------

# compute estimates of models under considerations and AIC and BIC

#Coefficient Test
AR.1.MA.0.y2<-arima(y2,order=c(1,0,0))
coeftest(AR.1.MA.0.y2,vcv=NeweyWest)

AR.2.MA.0.y2<-arima(y2,order=c(2,0,0))
coeftest(AR.2.MA.0.y2,vcv=NeweyWest)

AR.3.MA.0.y2<-arima(y2,order=c(3,0,0))
coeftest(AR.3.MA.0.y2,vcv=NeweyWest)

AR.4.MA.0.y2<-arima(y2,order=c(4,0,0))
coeftest(AR.4.MA.0.y2,vcv=NeweyWest)

AR.3.MA.1.y2<-arima(y2,order=c(3,0,1))
coeftest(AR.3.MA.1.y2,vcv=NeweyWest)

AR.3.MA.2.y2<-arima(y2,order=c(3,0,2))
coeftest(AR.3.MA.2.y2,vcv=NeweyWest)

AR.2.MA.1.y2<-arima(y2,order=c(2,0,1))
coeftest(AR.2.MA.1.y2,vcv=NeweyWest)

AR.2.MA.2.y2<-arima(y2,order=c(2,0,2))
coeftest(AR.3.MA.2.y2,vcv=NeweyWest)

AR.1.MA.1.y2<-arima(y2,order=c(1,0,1))
coeftest(AR.1.MA.1.y2,vcv=NeweyWest)

AR.1.MA.2.y2<-arima(y2,order=c(1,0,2))
coeftest(AR.1.MA.2.y2,vcv=NeweyWest)

AR.1.MA.3.y2<-arima(y2,order=c(1,0,3))
coeftest(AR.1.MA.3.y2,vcv=NeweyWest)

AR.1.MA.4.y2<-arima(y2,order=c(1,0,4))
coeftest(AR.1.MA.4.y2,vcv=NeweyWest)

AIC(AR.2.MA.1.y2,AR.2.MA.2.y2,AR.3.MA.0.y2,AR.3.MA.1.y2,AR.3.MA.2.y2,AR.4.MA.0.y2)
BIC(AR.2.MA.1.y2,AR.2.MA.2.y2,AR.3.MA.0.y2,AR.3.MA.1.y2,AR.3.MA.2.y2,AR.4.MA.0.y2)

#Create a Table for Results

models.y2 <- list(
  AR2MA1 = arima(y2, order=c(2,0,1)),
  AR2MA2 = arima(y2, order=c(2,0,2)),
  AR3MA0 = arima(y2, order=c(3,0,0)),
  AR3MA1 = arima(y2, order=c(3,0,1)),
  AR3MA2 = arima(y2, order=c(3,0,2)),
  AR4MA0 = arima(y2, order=c(4,0,0))
)

aics.y2 <- sapply(models.y2, AIC)
bics.y2 <- sapply(models.y2, BIC)

table.y2.aic <- data.frame(Model = names(models.y2), AIC = aics.y2)
table.y2.bic <- data.frame(Model = names(models.y2), BIC = bics.y2)

# STEP 3: DIAGNOSTIC CHECKING---------------------------------------------------

#perform diagnostics of chosen model: ARMA(2,1) 

AR.2.MA.1.eigen<-polyroot(c(-.89, -.67, 1))

AR.2.MA.1.res<-residuals(AR.2.MA.1.y2)
plot(AR.2.MA.1.res, main="Residuals of ARMA(2,1) Estimates", xlab="Time", type = "l", col="blue")
summary(AR.2.MA.1.res)

#Check for auto correlation
acf(AR.2.MA.1.res, main="Autocorrelation of Residuals for ARMA(2,1)")

# plot histogram of residuals and contrast with Gaussian distribution

x2 <- AR.2.MA.1.res
h2<-hist(x2, breaks=40, col="red", xlab="Unemployment Fit Residuals", 
         main="Histogram with Normal Curve") 
x2fit<-seq(min(x2),max(x2),length=40) 
y2fit<-dnorm(x2fit,mean=mean(x2),sd=sd(x2)) 
y2fit <- y2fit*diff(h2$mids[1:2])*length(x2) 
lines(x2fit, y2fit, col="blue", lwd=2)

# Fit values of model
AR.2.MA.1.fit<-fitted(AR.2.MA.1.y2)

plot(y2, main="Fitted ARMA(2,1) Estimates", xlab="Time", type = "l", col="blue")
lines(AR.2.MA.1.fit, col="red")

# forecast using the preferred model

plot(forecast(AR.2.MA.1.y2,h=100))



#_______________________________________________________________________________
## LIMITATIONS: Data Excluding COVID -------------------------------------------
#_______________________________________________________________________________

# IINFLATION RATE (PRE-COVID)---------------------------------------------------

# Create a new INFLATION time series w/o COVID
inflation.precovid <- window(Inflation, end = c(2020, 1))

#plot growth rate

plot(inflation.precovid, main="Portugal CPI Inflation, Pre-Covid",xlab="Time",type="l")

#summary statistics

summary(inflation.precovid)

#give data a more convenient notation

y1.pc <- inflation.precovid

# STEP 1: IDENTIFICATION--------------------------------------------------------

#plot data, ACF and PACF to get an idea of model class

plot(y1.pc, main="Portugal CPI Inflation, Quarterly, Pre-Covid ", xlab="Time", type = "l")

acf(y1.pc,main="Autocorrelation of Inflation, Pre-Covid ")

pacf(y1.pc,main="Partial Autocorrelation Inflation, Pre-Covid ")

# STEP 2: ESTIMATION------------------------------------------------------------

# compute estimates of models under considerations and AIC and BIC

#Coefficient Test

AR.1.MA.4.y1.pc<-arima(y1.pc,order=c(1,0,4))
coeftest(AR.1.MA.4.y1.pc,vcv=NeweyWest)

AR.1.MA.5.y1.pc<-arima(y1.pc,order=c(1,0,5))
coeftest(AR.1.MA.5.y1.pc,vcv=NeweyWest)

AR.2.MA.3.y1.pc<-arima(y1.pc,order=c(2,0,3))
coeftest(AR.2.MA.3.y1.pc,vcv=NeweyWest)

AR.2.MA.4.y1.pc<-arima(y1.pc,order=c(2,0,4))
coeftest(AR.2.MA.4.y1.pc,vcv=NeweyWest)

AIC(AR.1.MA.4.y1.pc,AR.1.MA.5.y1.pc,AR.2.MA.3.y1.pc,AR.2.MA.4.y1.pc)
BIC(AR.1.MA.4.y1.pc,AR.1.MA.5.y1.pc,AR.2.MA.3.y1.pc,AR.2.MA.4.y1.pc)

#Create a Table for Results

models.y1.pc <- list(
  AR1MA4 = arima(y1.pc, order=c(1,0,4)),
  AR1MA5 = arima(y1.pc, order=c(1,0,5)),
  AR2MA3 = arima(y1.pc, order=c(2,0,3)),
  AR2MA4 = arima(y1.pc, order=c(2,0,4))
)

aics.y1.pc <- sapply(models.y1.pc, AIC)
bics.y1.pc <- sapply(models.y1.pc, BIC)

table.y1.pc.aic <- data.frame(Model = names(models.y1.pc), AIC = aics.y1.pc)
table.y1.pc.bic <- data.frame(Model = names(models.y1.pc), BIC = bics.y1.pc)

# STEP 3: DIAGNOSTIC CHECKING---------------------------------------------------

#perform diagnostics of chosen model: ARMA(2,4) 

AR.2.MA.4.eigen<-polyroot(c(-.67,.21, 1))
#If the value of the roots is more than 1 then it is not stationary


AR.2.MA.4.res<-residuals(AR.2.MA.4.y1.pc)
plot(AR.2.MA.4.res, main="Residuals of ARMA(2,4) Estimates", xlab="Time", type = "l", col="blue")

acf(AR.2.MA.4.res, main="Autocorrelation of Residuals for ARMA(2,4)")

AR.2.MA.4.fit<-fitted(AR.2.MA.4.y1.pc)

plot(y1.pc, main="Fitted ARMA(2,4) Estimates", xlab="Time", type = "l", col="blue")
lines(AR.2.MA.4.fit, col="red")


# plot histogram of residuals and contrast with Gaussian distribution

x1.pc <- AR.2.MA.4.res
h1.pc<-hist(x1.pc, breaks=40, col="red", xlab="Inflation Fit Residuals", 
         main="Histogram with Normal Curve") 
x1fit.pc<-seq(min(x1.pc),max(x1.pc),length=40) 
y1fit.pc<-dnorm(x1fit.pc,mean=mean(x1.pc),sd=sd(x1.pc)) 
y1fit.pc <- y1fit.pc*diff(h1.pc$mids[1:2])*length(x1) 
lines(x1fit.pc, y1fit.pc, col="blue", lwd=2)

# forecast using the preferred model

plot(forecast(AR.2.MA.4.y1.pc,h=100))


# UNEMPLOYMENT RATE (PRE-COVID) ------------------------------------------------

# Create a new UNEMPLOYMENT time series w/o COVID
urate.precovid <- window(urate,  end = c(2020, 1))

#plot data

plot(urate.precovid, main="Quarterly Unemployment Rate, Pre-Covid", xlab="Time", type = "l")

#summary statistics

summary(urate.precovid)

#give data a more convenient notation

y2.pc <- urate.precovid

# STEP 1: IDENTIFICATION--------------------------------------------------------

#plot data, ACF and PACF to get an idea of model class

plot(y2.pc, main="Portugal Unemployment Rate, Quarterly, Pre-Covid", xlab="Time", type = "l")

acf(y2.pc,main="Autocorrelation of Unemployment, Pre-Covid ")

pacf(y2.pc,main="Partial Autocorrelation Unemployment, Pre-Covid ")

# STEP 2: ESTIMATION------------------------------------------------------------

# compute estimates of models under considerations and AIC and BIC

#Coefficient Test

AR.1.MA.2.y2.pc<-arima(y2.pc,order=c(1,0,2))
coeftest(AR.1.MA.2.y2.pc,vcv=NeweyWest)

AR.1.MA.3.y2.pc<-arima(y2.pc,order=c(1,0,3))
coeftest(AR.1.MA.3.y2.pc,vcv=NeweyWest)

AR.1.MA.4.y2.pc<-arima(y2.pc,order=c(1,0,4))
coeftest(AR.1.MA.4.y2.pc,vcv=NeweyWest)

AR.2.MA.0.y2.pc<-arima(y2.pc,order=c(2,0,0))
coeftest(AR.2.MA.0.y2.pc,vcv=NeweyWest)

AR.2.MA.1.y2.pc<-arima(y2.pc,order=c(2,0,1))
coeftest(AR.2.MA.1.y2.pc,vcv=NeweyWest)

AR.3.MA.0.y2.pc<-arima(y2.pc,order=c(3,0,0))
coeftest(AR.3.MA.0.y2.pc,vcv=NeweyWest)

AIC(AR.1.MA.2.y2.pc,AR.1.MA.3.y2.pc,AR.1.MA.4.y2.pc,AR.2.MA.0.y2.pc,AR.2.MA.1.y2.pc,AR.3.MA.0.y2.pc)
BIC(AR.1.MA.2.y2.pc,AR.1.MA.3.y2.pc,AR.1.MA.4.y2.pc,AR.2.MA.0.y2.pc,AR.2.MA.1.y2.pc,AR.3.MA.0.y2.pc)

#Create a Table for Results

models.y2.pc <- list(
  AR1MA2 = arima(y2.pc, order=c(1,0,2)),
  AR1MA3 = arima(y2.pc, order=c(1,0,3)),
  AR1MA4 = arima(y2.pc, order=c(1,0,4)),
  AR2MA0 = arima(y2.pc, order=c(2,0,0)),
  AR2MA1 = arima(y2.pc, order=c(2,0,1)),
  AR3MA0 = arima(y2.pc, order=c(3,0,0))
)

aics.y2.pc <- sapply(models.y2.pc, AIC)
bics.y2.pc <- sapply(models.y2.pc, BIC)

table.y2.pc.aic <- data.frame(Model = names(models.y2.pc), AIC = aics.y2.pc)
table.y2.pc.bic <- data.frame(Model = names(models.y2.pc), BIC = bics.y2.pc)

# STEP 3: DIAGNOSTIC CHECKING---------------------------------------------------

#perform diagnostics of chosen model: ARMA(2,1) 

AR.2.MA.1.eigen.pc<-polyroot(c(-.84, -.49, 1))

AR.2.MA.1.res.pc<-residuals(AR.2.MA.1.y2.pc)
plot(AR.2.MA.1.res.pc, main="Residuals of ARMA(2,1) Pre-Covid Estimates", xlab="Time", type = "l", col="blue")

acf(AR.2.MA.1.res.pc, main="Autocorrelation of Residuals for ARMA(2,1), Pre-Covid")

AR.2.MA.1.fit.pc<-fitted(AR.2.MA.1.y2.pc)

plot(y2.pc, main="Fitted ARMA(2,1) Estimates, Pre-Covid", xlab="Time", type = "l", col="blue")
lines(AR.2.MA.1.fit.pc, col="red")


# plot histogram of residuals and contrast with Gaussian distribution

x2.pc <- AR.2.MA.1.res.pc
h2.pc<-hist(x2.pc, breaks=40, col="red", xlab="Unemployment Fit Residuals", 
         main="Histogram with Normal Curve, Pre-Covid") 
x2fit.pc<-seq(min(x2.pc),max(x2.pc),length=40) 
y2fit.pc<-dnorm(x2fit.pc,mean=mean(x2.pc),sd=sd(x2.pc)) 
y2fit.pc <- y2fit.pc*diff(h2.pc$mids[1:2])*length(x2.pc) 
lines(x2fit.pc, y2fit.pc, col="blue", lwd=2)

# forecast using the preferred model

plot(forecast(AR.2.MA.1.y2.pc,h=100))


#_______________________________________________________________________________
## LIMITATIONS: CPI Data Excluding first 10 years-------------------------------
#_______________________________________________________________________________

# Create a new time series object with the desired window
Inflation.alt <- window(Inflation, start = c(1970, 1))

#plot growth rate

plot(Inflation.alt, main="Portugal CPI Inflation since 1970",xlab="Time",type="l")

#summary statistics

summary(Inflation.alt)

#give data a more convenient notation

y1.alt <- Inflation.alt

# STEP 1: IDENTIFICATION--------------------------------------------------------

#plot data, ACF and PACF to get an idea of model class

plot(y1.alt, main="Portugal CPI Inflation since 1970, Quarterly", xlab="Time", type = "l")

acf(y1.alt,main="Autocorrelation of Inflation since 1970, Portugal")

pacf(y1.alt,main="Partial Autocorrelation Inflation since 1970, Portugal")
