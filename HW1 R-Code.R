# Econ 112 Macroeconomic Data Analysis

# Homework 1  

setwd("C:/Users/oklah/OneDrive/Spring23/Econ 112/Homework 1")

# load data into workspace

data.gdp=read.table("real_gdp_econ_112.csv",header=T, sep=",")
data.cpi=read.table("CPI.csv",header=T, sep=",")
data.gby=read.table("3-Month Interest Rate.csv",header=T, sep=",", na.strings = ".")
data.uer=read.table("Unemployment Quarterly.csv",header=T, sep=",")

#GROSS DOMESTIC PRODUCT--------------------------------------------------------

#tell R data is time series

rgdp=ts(data.gdp[,2],start=1947,frequency=4)

#plot data

plot(rgdp, main="Quarterly GDP", xlab="Time", type = "l")

#compute annualized quarterly growth rate

dlogrgdp = 400*diff(log(rgdp))

#plot growth rate

plot(dlogrgdp, main="Portugal GDP Growth",xlab="Time",type="l")

#summary statistics

summary(dlogrgdp)


#CONSUMER PRICE INDEX-----------------------------------------------------------

#tell R data is time series

cpindex=ts(data.cpi[,2],start=1960,frequency=12)

#summary statistics

summary(cpindex)

#plot data

plot(cpindex, main="Consumer Price Index", xlab="Time", type = "l")

#compute annualized quarterly inflation rate

dlogcpi = 1200*diff(log(cpindex))

#plot growth rate

plot(dlogcpi, main="Portugal CPI Inflation",xlab="Time",type="l")

#summary statistics

summary(dlogrgdp)

#GOVERNMENT BOND YIELD----------------------------------------------------------

#tell R data is time series

gbyield=ts(data.gby[,2],start=1985,frequency=12)

#summary statistics

summary(gbyield)

#plot data

plot(gbyield, main="Government Bond Yield Portugal", xlab="Time", type = "l")


#UNEMPLOYMENT RATE--------------------------------------------------------------

#tell R data is time series

urate=ts(data.uer[,2],start=1998,frequency=4)

#summary statistics

summary(urate)

#plot data

plot(urate, main="Monthly Unemployment Rate", xlab="Time", type = "l")


#REGRESSIONS--------------------------------------------------------------------

#Real GDP growth rate on Unemployment Rate

dlogrgdp.NC=window(dlogrgdp,start=c(1998,1), end=c(2022,3))
model1=lm(urate~dlogrgdp.NC)
stargazer(model1, type = "html", title = "Regression 1 Results")

#CPI Inflation growth rate on Government Bond Yield

dlogcpi.NC=window(dlogcpi,start=c(1985,1), end=c(2022,7))
model2= lm(gbyield~dlogcpi.NC)
stargazer(model2, type = "html", title = "Regression 2 Results")
