# Econ 112 Macroeconomic Data Analysis

# Homework 3  

setwd("C:/Users/oklah/OneDrive/Spring23/Econ 112/Homework")

# INSTALL LIBRARIES-------------------------------------------------------------

library(urca)
library(mFilter)
library(tseries)
library(dyn)

# LOAD DATA---------------------------------------------------------------------

data.gdp=read.table("real_gdp_econ_112.csv",header=T, sep=",")
data.cpi=read.table("CPI.csv",header=T, sep=",")
data.gby=read.table("3-Month Interest Rate.csv",header=T, sep=",", na.strings = ".")
data.uer=read.table("Unemployment Quarterly.csv",header=T, sep=",")

# CREATE TIME SERIES------------------------------------------------------------

# GDP

rgdp=ts(data.gdp[,2],start=1947,frequency=4)
dlogrgdp = 400*diff(log(rgdp))

# CPI Inflation

cpindex=ts(data.cpi[,2],start=1960,frequency=12)
dlogcpi = 1200*diff(log(cpindex))

# 3-Month Interest Rate
data.3mir = na.omit(data.gby)
gbyield=ts(data.3mir[,2],start=1985,frequency=12)

# Unemployment Rate

urate=ts(data.uer[,2],start=1998,frequency=4)

#_______________________________________________________________________________
# QUESTION 1: ADF on Real GDP
#_______________________________________________________________________________

adf.test(dlogrgdp)

#_______________________________________________________________________________
# QUESTION 2: PP/KPSS/ERS on Real GDP
#_______________________________________________________________________________

pp.test(dlogrgdp)
kpss.test(dlogrgdp)
ers.rgdp<-ur.ers(dlogrgdp,type = c("P-test"),model = c("constant"))

summary(ers.rgdp)

#_______________________________________________________________________________
# QUESTION 3: ADF/PP/KPSS/ERS on Unemployment, Inflation, and 3-Month Interest
#_______________________________________________________________________________

# Unemployment

adf.test(urate)
pp.test(urate)
kpss.test(urate)
ers.urate<-ur.ers(urate,type = c("P-test"),model = c("constant"))

summary(ers.urate)

# Inflation

adf.test(dlogcpi)
pp.test(dlogcpi)
kpss.test(dlogcpi)
ers.logcpi<-ur.ers(dlogcpi,type = c("P-test"),model = c("constant"))

summary(ers.logcpi)

# 3-Month Interest

adf.test(gbyield)
pp.test(gbyield)
kpss.test(gbyield)
ers.gby<-ur.ers(gbyield,type = c("P-test"),model = c("constant"))

summary(ers.gby)

#_______________________________________________________________________________
# QUESTION 4: Use HP/BK/Hamilton Filter on Real GDP
#_______________________________________________________________________________

# HP Filter -------------------------------------------------------------------- 

#lambda is smoothing parameter

rgdp.hp<-hpfilter(rgdp,freq=1600,type="lambda")

#plot trend and cycles

plot(rgdp.hp,lwd=2)

# Baxter-King Filter------------------------------------------------------------

#pl and pu are the lower and upper band limit of the filter in quarters

rgdp.bk<-bkfilter(rgdp,pl=6,pu=32)

#plot trend and cycles

plot(rgdp.bk,lwd=2)

# HP and BK together

plot(rgdp.hp$cycle,main="BK and HP filter of Real GDP",col=2)
lines(rgdp.bk$cycle,col=1)

# Hamilton decomposition from Hamilton------------------------------------------

# h is horizon that defines trend in Hamilton filter

h = 8

dy.HAM.1 <- dyn$lm(rgdp ~ lag(rgdp, -h)+lag(rgdp, -(h+1))+lag(rgdp, -(h+2))+lag(rgdp, -(h+3)))
summary(dy.HAM.1)

rgdp.HAM.trend<-fitted(dy.HAM.1)

# note: need to set start 8 quarters later to match timing of cycle

rgdp.HAM.trend<-ts(rgdp.HAM.trend,start=1950,frequency=4)

plot(rgdp.HAM.trend,col="red",lwd=2)
lines(rgdp,col="blue",lwd=2)

rdgp.HAM.cycle = rgdp-rgdp.HAM.trend
plot(rdgp.HAM.cycle,col="red",lwd=2)

#plot Ham, HP, BK Together

plot(rdgp.HAM.cycle, col="black",lwd=2)
lines(rgdp.bk$cycle,col="red",lwd=2)
lines(rgdp.hp$cycle,col="blue",lwd=2)


#_______________________________________________________________________________
# QUESTION 5: Use HP/BK/Hamilton Filter on Unemployment
#_______________________________________________________________________________


# HP Filter -------------------------------------------------------------------- 

#lambda is smoothing parameter

urate.hp<-hpfilter(urate,freq=1600,type="lambda")

#plot trend and cycles

plot(urate.hp,lwd=2)

# Baxter-King Filter------------------------------------------------------------

#pl and pu are the lower and upper band limit of the filter in quarters

urate.bk<-bkfilter(urate,pl=6,pu=32)

#plot trend and cycles

plot(urate.bk,lwd=2)

# HP and BK together

plot(urate.hp$cycle,main="BK and HP filter of Unemployment",col=2)
lines(urate.bk$cycle,col=1)

# Hamilton decomposition from Hamilton------------------------------------------

# h is horizon that defines trend in Hamilton filter

h = 8 

dy.HAM.2 <- dyn$lm(urate ~ lag(urate, -h)+lag(urate, -(h+1))+lag(urate, -(h+2))+lag(urate, -(h+3)))
summary(dy.HAM.2)

urate.HAM.trend<-fitted(dy.HAM.2)

# note: need to set start 8 quarters later to match timing of cycle

urate.HAM.trend<-ts(urate.HAM.trend,start=1998,frequency=4)

plot(urate.HAM.trend,col="red",lwd=2)
lines(urate,col="blue",lwd=2)

urate.HAM.cycle = urate-urate.HAM.trend
plot(urate.HAM.cycle,col="red",lwd=2)

#plot Ham, HP, BK Together

plot(urate.HAM.cycle, col="black",lwd=2)
lines(urate.bk$cycle,col="red",lwd=2)
lines(urate.hp$cycle,col="blue",lwd=2)
