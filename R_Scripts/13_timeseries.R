### Analysis of time series and temporally autocorrelated data ###

library(here)
library(tidyverse)
library(nlme)
library(forecast)

### Fit autocorrelated errors in a predictive model using nlme ###
#load Hawaii bird data
birds <- read.table(here::here("data", "Hawaii_birds.txt"), header=T)

# sqrt transform counts
birds$abund <- sqrt(birds$Moorhen.Kauai)

# plot transformed data
plot(birds$Year, birds$abund, xlab="Year", ylab="sqrt(Moorhen abundance)")

### Linear models ###
## Assume independence (iid)
mod.gls <- gls(abund ~ Rainfall + Year, na.action=na.omit, data=birds)

#ACF plot function assumes no skipped time points. GLS removed missing values. Need to fill in the residual series to account for missing value
E <- residuals(mod.gls, type="normalized")
I1 <- !is.na(birds$abund)
Efull <- vector(length=length(birds$abund))
Efull <- NA
Efull[I1] <- E
par(mfrow=c(2,1))
acf(Efull, na.action=na.pass)
pacf(Efull, na.action=na.pass)

## Compound symmetry
mod.cs <- gls(abund ~ Rainfall + Year, na.action=na.omit, data=birds, correlation=corCompSymm(form=~Year))

Ecs <- residuals(mod.cs, type="normalized")
I1 <- !is.na(birds$abund)
Efullc <- vector(length=length(birds$abund))
Efullc <- NA
Efullc[I1] <- Ecs
acf(Efullc, na.action=na.pass)
pacf(Efullc, na.action=na.pass)

#errors structured as AR1
mod.ar1 <- gls(abund ~ Rainfall + Year, na.action=na.omit, data=birds, correlation=corAR1(form=~Year))

Ear1<-residuals(mod.ar1, type="normalized")
I1<-!is.na(birds$abund)
Efulla<-vector(length=length(birds$abund))
Efulla<-NA
Efulla[I1]<-Ear1
acf(Efulla, na.action=na.pass)
pacf(Efulla, na.action=na.pass)

####################################################
### Time series decomposition ###
####################################################

### Simulate time series with contrasting AR and MA properties using arima.sim ###
## arguments are order (vector of length=3 specifying p,d,q), ar (vector of length p specifying AR coefficients), ma (vecotr of length q specifying MA coefficients), sd (scalar specifying SD of normal errors)

### AR1 models
#parameters for AR(1) with small phi (correlation) coefficient
AR.sm <- list(order=c(1,0,0), ar=0.1, sd=0.1)

#parameters AR(1) with large phi (correlation) coefficient
AR.lg <- list(order=c(1,0,0), ar=0.9, sd=0.1)

#run the simulations
AR1.sm <- arima.sim(n=50, model=AR.sm)
AR1.lg <- arima.sim(n=50, model=AR.lg)

## plot the time series
par(mfrow=c(2,1))
#find y-limits for common plots
ylm<-c(min(AR1.sm, AR1.lg), max(AR1.sm, AR1.lg))
#plot the time series using the time series plotting function
plot.ts(AR1.sm, ylim=ylm, 
        ylab=expression(italic(x)[italic(t)]),
        main=expression(paste(phi, " = 0.1")))
plot.ts(AR1.lg, ylim=ylm,
        ylab=expression(italic(x)[italic(t)]),
        main=expression(paste(phi, " = 0.9")))

## plot the ACF and PACF for the simulated series
par(mfrow=c(2,2))
acf(AR1.sm)
pacf(AR1.sm)
acf(AR1.lg)
pacf(AR1.lg)

### MA models
# parameters for MA(1) with small coefficient (theta, multiplier of previous error term)
MA.sm <-list(order=c(0,0,1), ma=0.2, sd=0.1)

# parameters for MA(1) with large coefficient (theta)
MA.lg <- list(order=c(0,0,1), ma=0.8, sd=0.1)

## run the simulations
MA1.sm <- arima.sim(n=50, model=MA.sm)
MA1.lg <- arima.sim(n=50, model=MA.lg)

## plot the time series
par(mfrow=c(2,1))
#find y-limits for common plots
ylm<-c(min(MA1.sm, MA1.lg), max(MA1.sm, MA1.lg))
#plot the time series using the time series plotting function
plot.ts(MA1.sm, ylim=ylm, 
        ylab=expression(italic(x)[italic(t)]),
        main=expression(paste(theta, " = 0.2")))
plot.ts(MA1.lg, ylim=ylm,
        ylab=expression(italic(x)[italic(t)]),
        main=expression(paste(theta, " = 0.8")))

## plot the ACF and PACF for the simulated series
par(mfrow=c(2,2))
acf(MA1.sm)
pacf(MA1.sm)
acf(MA1.lg)
pacf(MA1.lg)

### Remove a seasonal effect by differencing ###
# diff function requires: x (data), lag (lag at which to difference), differences (order of differencing)

# load CO2 record from Mauna Loa
ww1 <- "ftp://aftp.cmdl.noaa.gov/products/"
ww2 <- "trends/co2/co2_mm_mlo.txt"
CO2 <- read.table(paste(ww1,ww2,sep=""))[,c(1,2,5)]
# assign column names
names(CO2) <- c("year","month","ppm")

# convert the CO2 data frame to a time series object, with frequency=12 (data collected monthly), start date=3/1958
co2 <- ts(data=CO2$ppm, frequency=12,
        start=c(CO2[1,"year"], CO2[1, "month"]))

#plot the co2 time series
par(mfrow=c(1,1))
plot.ts(co2, ylab=expression(paste("CO"[2], " (ppm)")))

## Decomposition of trend, seasonal, and residual variation
co2.decomp <- decompose(co2)

plot(co2.decomp)

#calculate 2nd difference of the co2 time series (non-linear trend)
co2.D2 <- diff(co2, differences=2)

#plot the differenced data
plot(co2.D2, ylab=expression(paste(nabla^2, "CO"[2])))

#difference with lag 12 to remove the seasonal periodicity still apparent
co2.D2D12<-diff(co2.D2, lag=12)
#plot the differenced data
plot(co2.D2D12, ylab=expression(paste(nabla, "(",nabla^2, "CO"[2], ")")))

### Fit an ARMA model ###
# arima parameters are n (number of samples), and include.mean=TRUE if present, or =FALSE if data are de-meaned

#first, simulate an ARMA(2,2) model
ARMA22 <- list(order=c(2,0,2), ar=c(-0.7,0.2), ma=c(0.7, 0.2))
#specify mean
mu <- 5
#simulated data
ARMA.sim <- arima.sim(n=10000, model=ARMA22) + mu

#estimate parameters of the model
arima(x=ARMA.sim, order=c(2,0,2))

## Search for appropriate model over several orders
#set up empty list to store model fits
ARMA.res <- list()
#set counter
cc <- 1
#loop over AR orders 0-3
for(p in 0:3) {
  #loop over MA orders 0-3
  for(q in 0:3) {
    ARMA.res[[cc]]<- arima(x=ARMA.sim, order=c(p,0,q))
    cc<-cc+1
  }
}

#AIC values
ARMA.AIC <- sapply(ARMA.res, function(x) x$aic)
#identify model with lowest AIC and print its output
ARMA.res[[which(ARMA.AIC==min(ARMA.AIC))]]

### Another option for automatically determining the order of an ARIMA model is the auto.arima function in the package "forecast" 

auto.arima(ARMA.sim, start.p=0, max.p=3, start.q=0, max.q=3)
#note: set trace=1 in the call to auto.arima to see the form of each of the models checked

##Once an ARMA model is fitted, it can be used to predict within sample (interpolate missing values), or out of sample (forecast future observations... at your peril)