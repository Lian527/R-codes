### 1. IntroTS
# We have three ways to check the residual is dwn or not
## 1. mean value by t.test
## 2. plot to check the variance in constance or not
## 3. acf of residuals to check autocorrelation

### 1 ploting timeseries and transforming to logaritm
# 1-1
data(AirPassengers)
Air <- AirPassengers
Air
ts.plot(Air) # plot for airpassengers

# 1-2
log_Air<-log(Air)
log_Air # log(response)
plot(log_Air)

### 2 Linear model (quadratic)
# 2-1
## standarize time (time-mean(time))/sd(time)
log_Air.ts <- ts(log_Air,st=c(1949,1),end=c(1960,12),fr=12)
log_Air.ts
time(log_Air.ts) # extract time in ts.
mean(log_Air.ts)
sd(log_Air.ts)
standard.time <- (time(log_Air.ts)-mean(time(log_Air.ts)))/sd(time(log_Air.ts))
standard.time # to prevent huge number of time
## quadratic model
pred1 <- cbind(standard.time, I((standard.time)^2))
colnames(pred1) <- c("time","time2")
pred1
model1 <- lm(log_Air.ts ~ pred1)
summary(model1)
AIC(model1)

# 2-2 residuals of linear model is not whitenoise model, but mean should be 0
model1$residuals
plot(model1$residuals, type="l")
acf(model1$residuals) # residuals are autocorrelated


### 3 Harmonic seasonal model
# 3-1
Cycles <- 6
SIN <- COS <- matrix(nr=length(log_Air.ts),nc=Cycles) # row=number of obs, col=cycles
for(i in 1:Cycles)
{
  COS[,i] <- cos(2*pi*i*time(log_Air.ts)) # do not devide by 12, time is in year
  SIN[,i] <- sin(2*pi*i*time(log_Air.ts)) # do not devide by 12, time is in year
}
pred2 <- cbind(standard.time,I(standard.time^2),COS,SIN)
colnames(pred2) <- c("time", "time2", paste("cos", 1:Cycles, sep=""),paste("sin", 1:Cycles, sep=""))
pred2
model2 <- lm(log_Air.ts ~ pred2)
AIC(model2)

# 3-2
model2$residuals
plot(model2$residuals,type="l")
acf(model2$residuals)

### choosing significant predictors in harmonic seasonal model
# 4-1
summary(model2)
model3 <- lm(log_Air.ts~pred2[,c(1:4,9:13)])
summary(model3)
AIC(model3)
# 4-2
model3$residuals
plot(model3$residuals,type="l") # unequal variance
acf(model3$residuals) # autocorrelated
ar(model3$residuals)$order # do this to fit it in general least square to have right SE
ar(model3$residuals)$ar # always have to choose the model for residuals before using gls

### 4 Generalized least square --> to get right SE value
# 5
library(nlme)
general_ls <- gls(log_Air.ts ~ pred2[,c(1:4,9:13)],cor=corARMA(c(0.6100555),p=1,fixed=TRUE)) # p is the order of AR model of residuals
AIC(general_ls)

## NP1
# Example 1.4.1
# number of international passenger bookings (in thousands) per month
# on an airline (Pan Am) in the United States were obtained from the
# Federal Aviation Administration for the period 1949–1960
data(AirPassengers)
AP <- AirPassengers
AP

class(AP) # time series

start(AP); end(AP); frequency(AP)

summary(AP)

plot(AP, ylab="Passengers (1000's)")  # What do you notice?

layout(1:2)
plot(aggregate(AP)) # series aggregated to annual level
boxplot(AP~cycle(AP)) # passengers by month

aggregate(AP) # passengers per year (removes seasonal effects)
aggregate(AP,FUN=mean) # average passengers per month each year 

window(AP,start=c(1954,1),end=c(1955,12)) 
window(AP,start=c(1954,1),end=c(1955,12),frequency=4) # frequency - how often to sample

# see Section 1.4.2 for how to construct a time series object in R

## NP4 
plot(AP)

plot(decompose(AP)) #additive model
AP.add <- decompose(AP)
plot(stl(AP,s.window=12)) # uses lowess smoothing to obtain trend
plot(stl(AP,s.window=36)) # notice the residuals are larger when the trend is more smooth

AP.decom <- decompose(AP,type="mult") 
plot(AP.decom)
ts.plot(cbind(AP.decom$trend,AP.decom$trend*AP.decom$seasonal),lty=1:2)


new.ts <- AP.add$trend + AP.add$seasonal
new.ts.mult <- AP.decom$trend*AP.decom$seasonal
ts.plot(cbind(ts(new.ts),ts(new.ts.mult)),lty=c(1,1),col=c("black","red"))

### 2. Correlation
## NS2
data(AirPassengers)
AP <- AirPassengers
AP

# Consider another data set containing the mean global temperature each month
www <- "http://www.maths.adelaide.edu.au/andrew.metcalfe/Data/global.dat"
temp <- scan(www)
temp.ts <- ts(temp,st=c(1856,1),end=c(2005,12),fr=12) 
temp.ts

subTemp <- window(temp.ts,start=c(1949,1),end=c(1960,12)) #extract data during time window of AP

plot(c(subTemp),c(AP))
abline(v=mean(subTemp),lty=2)
abline(h=mean(AP),lty=2)

cov(subTemp,AP)
cor(subTemp,AP)

# Is it reasonable to assume a statinary model (in the mean) for AP data set?
plot(AP)

# Is it reasonable to assume a statinary model (in the mean) for temperature data set?
plot(subTemp)


## NS3
plot(AP[1:(length(AP)-1)],AP[2:length(AP)],xlab="x_(t)",ylab="x_(t+1)") # lag 1
cor(AP[1:(length(AP)-1)],AP[2:length(AP)]) # lag 1

plot(AP[1:(length(AP)-6)],AP[7:length(AP)],xlab="x_(t)",ylab="x_(t+6)") # lag 6
cor(AP[1:(length(AP)-6)],AP[7:length(AP)]) # lag 6

acf(AP) # usually a trend in the time series will appear as slow decay in autocorrelations


# extra residuals (errors) after removing trend and seasonal pattern
AP.decom <- decompose(AP,"multiplicative")
plot(ts(AP.decom$random)) 
acf(AP.decom$random[7:138])  #compare lag 1 and lag 6 autocorrelations to that obtained previously

sd(AP[7:138])
sd(AP[7:138]- AP.decom$trend[7:138])
sd(AP.decom$random[7:138])


## NS5

# Australian Bureau of Statistics publishes data on building approvals each month and
# a few weeks later the Building Activity Publication lists the value of building work
# done in each quarter

# Approvals - total dwellings approved per month (averaged over three months)
# Activity - value of work done over past three months (millions of AUS dollars)

www <- "http://www.maths.adelaide.edu.au/andrew.metcalfe/Data/ApprovActiv.dat"
Build.dat <- read.table(www,header=T); attach(Build.dat)
App.ts <- ts(Approvals,start=c(1996,1),freq=4)
Act.ts <- ts(Activity,start=c(1996,1),freq=4)
ts.plot(App.ts,Act.ts,lty=c(1,3))

# ccf can be calculated for any two time series that overlap, but if they both have trends
# or similar seasonal effects, these will dominate the plot

# population ccf is defined for stationary processes.  It is typical to remove trend and 
# seasonal effects before investigating cross-correlations

app.ran <- decompose(App.ts)$random
act.ran <- decompose(Act.ts)$random
acf(ts.union(app.ran,act.ran),na.action=na.pass) #ignore missing values

ccf(app.ran,act.ran,na.action=na.pass)

print(acf(ts.union(app.ran,act.ran),na.action=na.pass))

### 3. Basic Models
##############################
## NS1 - simulting white noise
##############################
set.seed(5) # if want results to be repeatable, you must set the random seed
w <- rnorm(100) # here we simulate Gaussian white noise
plot(w,type="l") 

# since we simulated Gaussian white noise, the t.s. values should resemble a normal curve
hist(w,prob=T,col="grey")  # plots a histogram of the white noise t.s.
x <- seq(-3,3,length=1000)
points(x,dnorm(x),type="l",col="red") # adds a line to the plot representing the normal distribution density

acf(w) # white noise is uncorrelated so correlations should be approximately 0 for all lags > 0


##############################
## NS2 - fitting the white noise model (only parameter is the variance)
##############################
var(w) # estimate variance parameter by the sample variance

##############################
## NS3 - simulating a random walk
##############################
n <- 1000
w <- rnorm(n) # DWN for process
x <- rep(NA,n) # vector to store random walk t.s.
x[1] <- w[1] 
for(t in 2:n)
{
  x[t] <- x[t-1] + w[t]
}
plot(x,type='l')

acf(x) # random walk is highly correlated so we see high autocorrelation at all lags
acf(diff(x)) # first-order difference of random walk t.s. is a white noise t.s. so we see no autocorrelation in the differences


##############################
## NS4 - testing if a time series is a random walk with drift
##############################
HPweb <- "http://www.maths.adelaide.edu.au/andrew.metcalfe/Data/HP.txt"
HP.dat <- read.table(HPweb,header=T); attach(HP.dat) # data contains HP stock prices over a long period of time

plot(as.ts(Price)) # we see trends in the data suggesting it is not DWN
lines(lowess(Price),col="red",lty=2,lwd=2) # add lowess line to see general trends in data
lines(lowess(Price,f=1/5),col="blue",lty=2,lwd=2) # f indicates the proportion of the t.s. to use during smoothing
# higher values of f will result in a smoother trend line, smaller values will show more variability

DP <- diff(Price)  # compute first order differences to assess if random walk model is appropriate

# If a random walk model is appropriate, differences will look like DWN (possibly plus a drift term)
plot(as.ts(DP)) # plot of differences looks like a DWN t.s.
acf(as.ts(DP)) # no evidence of autocorrelation so differences look like DWN 

# Based on the investigation of the differences above, a random walk model seems appropriate.
# Should the random walk model have a drift parameter?
t.test(DP)  # perform t-test on differences to see if the mean of the differences is non-zero
# Since p-value < .05, reject null hypothesis that drift parameter is zero

# Fitting the random walk model with drift
mean(DP) # estimate of drift parameter
var(DP) # estimate of variance parameter for white noise



##############################
## NS7
##############################

# Simulating AR(1) processes
set.seed(15)
n <- 1000
w <- rnorm(n) # white noise t.s.
x <- z <- rep(NA,n)
x[1] <- z[1] <- w[1]
for(t in 2:n)
{
  x[t] <- .7*x[t-1] + w[t] # stationary process
  z[t] <- z[t-1] + w[t]   # non-stationary process
}

par(mfrow=c(2,1)) # plot the two t.s. in the same window
plot(x,type="l")
plot(z,type="l")

acf(x) # autocorrelation is near zero for lags > 7
acf(z) # autocorrelation is high for all lags due to non-stationarity

# partial autocorrelation plots (can help determine order of the AR process )
pacf(x) # near zero at all lags > 1 suggesting process is AR(1)
pacf(z) # also near zero at all lags > 1 suggesting process is AR(1)



# Fitting an AR(p) model to data
x.ar <- ar(x,method="mle")  
x.ar$order  # selected using AIC (Akaike Information Criterion), which is a model selection criterion (model with lowest AIC is best)
x.ar$ar # estimate of parameters
sqrt(x.ar$asy.var[1]) # standard error of parameter estimate
x.ar$ar + c(-2,2)*sqrt(x.ar$asy.var[1]) # 95% confidence interval for parameters

z.ar <- ar(z,method="mle",demean=FALSE) 
z.ar$order
z.ar$ar
sqrt(diag(z.ar$asy.var)) # vector of standard errors for parameters
cbind(z.ar$ar - 2*sqrt(diag(z.ar$asy.var)), z.ar$ar - 2*sqrt(diag(z.ar$asy.var))) # 95% confidence intervals for parameters

### 4. Regression
######################
## NS2
######################

# Simulate a time series with autocorrelated errors (i.e. errors are NOT DWN)
set.seed(1)
n <- 100
z <- w <- rnorm(n,sd=20) # simulate white noise

# create AR(1) errors
for(t in 2:n)
{
  z[t] <- 0.8*z[t-1] + w[t] # stationary t.s.
}

Time <- 1:n
x <- 50 + 3*Time + z # create t.s. with straight line trend (50+3t) and AR(1) errors

plot(x,xlab="time",type="l")

# Fitting the regression model
x.lm <- lm(x ~ Time) # only predictor here is t (time)

summary(x.lm)  # parameter estimates of alpha_0 and alpha_1 are close to true values (50 and 3)

# BEFORE we rely on the standard error estimates and p-values associated with the t-tests
# for each coefficient, we must look at the residuals.  
#  (1) If the residuals look like DWN, we can use the regression results.  
#  (2) If the residuals are autocorrelated, the standard errors are likely to be 
#   underestimated.  In this case, we should NOT interpret the p-values associated 
#   with the t-tests.

acf(resid(x.lm))  # significant autocorrelation at lags 1 and 2
pacf(resid(x.lm)) # no partial autocorrelation after lag 1, so residuals look like AR(1) (which they are!)

# So if we can't use the standard errors from standard regression procedures, what do we do?
# Generalized least squares


######################
## NS3
######################

## Fitting t.s. regression with GLS
library(nlme)

# We need to specify error model to fit GLS so first we must determine an appropriate
# error model.  We decided above that an AR(1) process might be appropriate for errors
acf(resid(x.lm))[1]  # autocorrelation at lag 1
ar(resid(x.lm))   # estimated AR model

x.gls <- gls(x ~ Time, cor = corAR1(.665))
summary(x.gls)  # Notice standard error estimates are much bigger than when using lm command
confint(x.gls) # 95% confidence intervals for the parameters



######################
## NS4
######################
# Consider again the monthly average global temperature time series
www <- "http://www.maths.adelaide.edu.au/andrew.metcalfe/Data/global.dat"
temp <- scan(www)
temp.ts <- ts(temp,st=c(1856,1),end=c(2005,12),fr=12) # Recall the global temperature data
temp.ts

sub.temp <- window(temp.ts,start=c(1970,1),end=c(2005,12)) #extract data during time window of AP
plot(sub.temp)

Seas <- cycle(sub.temp)
Time <- time(sub.temp)
temp.lm <- lm(sub.temp ~ 0 + Time + factor(Seas))
summary(temp.lm) # coefficient for time variable and all 12 monthly effects


## prediction for next ten years
new.t <- seq(2006,len=10*12,by=1/12)
new.t
new.dat <- data.frame(Time=new.t,Seas=rep(1:12,10))
pred.dat <- predict(temp.lm,new.dat)
pred.dat

plot(sub.temp,xlim=c(1970,2015),type="l")
lines(new.t,pred.dat,col="red")





######################
## NS5
######################

TIME <- seq(1,12,len=1000)
plot(TIME,sin(2*pi*TIME/12),type="l")  # a single harmonic term is not very realistic
plot(TIME,sin(2*pi*TIME/12) 
     + .2*sin(2*pi*2*TIME/12) 
     + .1*sin(2*pi*4*TIME/12) 
     + .1*cos(2*pi*4*TIME/12),type="l")  # this looks more realistic


## Let's simulate a t.s. from a harmonic model
set.seed(1)
yrs <- 10
TIME <- 1:(yrs*12) # ten years (Note that we are counting months so we must divide by 12 in the harmonic terms)
w <- rnorm(yrs*12,sd=.5) # white noise

Trend <- .1 + .005*TIME + .001*TIME^2
plot(Trend,type="l")

Seasonal <- sin(2*pi*TIME/12) + .2*sin(4*pi*TIME/12) + .1*sin(8*pi*TIME/12) + .1*cos(8*pi*TIME/12) 
plot(Seasonal,type="l")

x <- Trend + Seasonal + w  # Final time series. Is there any autoregressive component here?
plot(x,type="l")

## How do we estimate a harmonic model?  First we must create possible harmonic terms
# and then we will feed them all into a regression and see which ones are significant
n.freq <- 6 # number of frequencies we want to consider
SIN <- COS <- matrix(nr=length(TIME),nc=6) # rows equal to number of time units, columns equal to number of frequencies

for(i in 1:n.freq)
{
  COS[,i] <- cos(2*pi*i*TIME/12)
  SIN[,i] <- sin(2*pi*i*TIME/12)
}

pred.mat <- cbind(TIME,I(TIME^2),COS,SIN)  # matrix of predictor variables
colnames(pred.mat) <- c("time","time2",paste("cos",1:6,sep=""),paste("sin",1:6,sep="")) # label the columns of the predictor matrix

x.lm <- lm(x ~ pred.mat)
summary(x.lm)  # Focusing on the significant predictors, what model does this suggest?

x.lm.sig <- lm(x ~ pred.mat[,c(2,9,10)]) # model with just significant predictors in the first model
summary(x.lm.sig)

x.lm.true <- lm(x ~ pred.mat[,c(1:2,6,9:10,12)])  # fit the true model (that which generated the data)
summary(x.lm.true)

## To compare the models, we will look at the AIC values.  (Model with smallest AIC is best.)
AIC(x.lm)
AIC(x.lm.sig)  # model with just significant predictors has lower AIC than full model
AIC(x.lm.true)  # true model has slightly worse AIC model than that with just the significant predictors

## How do we assess whether a model is adequate?  Look at residual plots.  If model is okay, residuals should be DWN.
plot(resid(x.lm.sig),type="l")  # centered around zero, no pattern
abline(a=0,b=0,col="red")
acf(as.ts(resid(x.lm.sig))) # no evidence of autocorrelation




######################
## NS6
######################
# Consider again the global temperature t.s. 
www <- "http://www.maths.adelaide.edu.au/andrew.metcalfe/Data/global.dat"
temp <- scan(www)
temp.ts <- ts(temp,st=c(1856,1),end=c(2005,12),fr=12) # Recall the global temperature data
sub.temp <- window(temp.ts,start=c(1970,1),end=c(2005,12)) #extract data during time window of AP
plot(sub.temp)

# Previously we fit a model with 12 monthly effects to capture the seasonal trend.
# The seasonal effect appears relatively smooth so a harmonic model with fewer parameters
# may work better at explaining the seasonal pattern.

# Again we create a matrix of possible harmonic terms
n.freq <- 6 
SIN <- COS <- matrix(nr=length(sub.temp),nc=6) 
for(i in 1:n.freq)
{
  COS[,i] <- cos(2*pi*i*time(sub.temp)) # we do not divide by 12 here since time is in years
  SIN[,i] <- sin(2*pi*i*time(sub.temp))
}

# We will consider linear and quadratic time terms for the trend, but since time is in years we will first 
# standardize the time variable so we don't have huge numbers 
# For example:
summary(time(sub.temp)^2)

mean(time(sub.temp))
sd(time(sub.temp))

std.time <- (time(sub.temp)-mean(time(sub.temp)))/sd(time(sub.temp))

pred.mat <- cbind(std.time,I((std.time)^2),COS,SIN)  # matrix of possible predictor variables
colnames(pred.mat) <- c("time","time2",paste("cos",1:6,sep=""),paste("sin",1:6,sep="")) 

temp.lm1 <- lm(sub.temp ~ pred.mat)
summary(temp.lm1)



# Let's pull out the statistically significant predictors 
temp.lm2 <- lm(sub.temp ~ pred.mat[,c(1,9:10)])
summary(temp.lm2)

# Compare the model with the 12 monthly effects to this model with harmonic terms
AIC(temp.lm1)
AIC(temp.lm2) # harmonic model is better fit

# Now we look at the residuals of the harmonic model to determine if it adequately fits the data
plot(time(sub.temp),resid(temp.lm2),type="l")
abline(a=0,b=0,col="red")
acf(resid(temp.lm2))  

# What is our diagnosis?

pacf(resid(temp.lm2))


res.ar <- ar(resid(temp.lm2),method="mle")
res.ar$ar

# Let's now look at the residuals from the AR model
plot(res.ar$resid,type="l")
acf(res.ar$resid,na.action = na.pass)


# What is the final model for the data?
# Let's refit the regression model using gls and an AR(2) structure
# For this we need to use the corARMA option with p=2 (order of AR) and q=0 (moving average order)
# Note: when you specify parameter values the function uses these to initalize the error model
# but updates them during the estimation process
# If you specify 'fixed=TRUE', the function will keep these error parameters fixed
temp.lm3 <- gls(sub.temp ~ pred.mat[,c(1,9:10)],cor=corARMA(c(0.493,0.307),p=2,fixed=TRUE)) 
temp.lm4 <- gls(sub.temp ~ pred.mat[,c(1,9:10)],cor=corARMA(c(0.493,0.307),p=2)) 
temp.lm5 <- gls(sub.temp ~ pred.mat[,c(1,9:10)],cor=corARMA(p=2))

summary(temp.lm3)
summary(temp.lm4)  # model estimates 4 and 5 are the same
summary(temp.lm5)

AIC(temp.lm2)
AIC(temp.lm3)
AIC(temp.lm4)

var(res.ar$resid,na.rm=TRUE)

### 5. Stationary Models
### NS3

# Simulate a MA(3)
set.seed(1)
b <- c(0.8, 0.6, 0.4)
n <- 1000
x <- w <- rnorm(n)
for(t in 4:n)
{
  for(j in 1:3)
  {
    x[t] <- x[t] + b[j]*w[t-j]
  }
}
plot(x,type="l")
acf(x)


## NS4

# Fitting an MA(3) model
x.ma <- arima(x,order=c(0,0,3))
x.ma

# approximate 95% confidence intervals
seB <- sqrt(diag(x.ma$var.coef))
cbind(x.ma$coef - 2*seB, x.ma$coef + 2*seB)  # do our CIs contain the true values?

# Let's now assess model fit
acf(x.ma$residuals) 

# What if we only fit an MA(1) to the data?
x.ma1 <- arima(x,order=c(0,0,1))
x.ma1

acf(x.ma1$residuals)  # does this model provide a satisfactory fit to the data?



## NS4 (part 2)

# what if we fit an AR model to the data?
acf(x)  # what does the acf of an AR process look like?
pacf(x)

x.ar <- ar(x)
x.ar  # YIKES!
acf(x.ar$res,na.action = na.pass)


## NS6

# Simulating in R
set.seed(1)
x <- arima.sim(n=1000,list(ar=-.6,ma=0.5))
plot(x,type="l")
acf(x)
pacf(x)
arima(x,order=c(1,0,1))

## Electricity production time series (6.6.3)
# Monthly supply of electricity (millions of kWh) in Australia
# from January 1958 tp December 1990
www <- "http://www.maths.adelaide.edu.au/andrew.metcalfe/Data/cbe.dat"
CBE <- read.table(www,header=T)
head(CBE)
dim(CBE)
Elec.ts <- ts(CBE[, 3], start = 1958, freq = 12)

par(mfrow=c(2,1))
plot(Elec.ts)
plot(log(Elec.ts))
## What regression model would you try fitting to the data first?







Time <- 1:length(Elec.ts)
Time[1:15]

Imth <- cycle(Elec.ts)
Imth[1:15]

Elec.lm <- lm(log(Elec.ts) ~ Time + I(Time^2) + factor(Imth))  # What model does this fit?
summary(Elec.lm)




acf(resid(Elec.lm))  #What do you see?


# Correlogram apepars to cycle with a period of 12 months suggesting the monthly indicator 
# variables are not sufficient to capture the seasonality effects.  What do we do???
# In the next chapter, we find we can capture these patterns using a nonstationary model.
# For now, let's see how well we can do using an ARMA model.

### Finding the best ARMA model
best.order <- c(0, 0, 0)
best.aic <- Inf  # initialize best AIC at infinity so that the first model will beat it
for (i in 0:2)  # loop through possible p parameters for AR part
{
  for (j in 0:2) # loop through possible q parameters for MA part
  {
    fit.aic <- AIC(arima(resid(Elec.lm), order = c(i,0,j)))  # get AIC for ARMA(i,j) model
    if (fit.aic < best.aic) # If this model is better than the best so far, keep it!
    {
      best.order <- c(i, 0, j)
      best.arma <- arima(resid(Elec.lm), order = best.order)
      best.aic <- fit.aic
    }
  }
}
best.order
best.arma
best.aic




acf(resid(best.arma))


# If we want to assess the significance of the predictors in the regression model,
# we should refit the regression model with GLS and an AR(2) error structure
library(nlme)
Elec.gls <- gls(log(Elec.ts) ~ Time + I(Time^2) + factor(Imth),cor=corARMA(p=2))  # What model does this fit?
summary(Elec.gls)



# Suppose we are intersted in forecasting future values of electricity production
# for the next three years

new.time <- seq(length(Elec.ts)+1, length = 36)
new.data <- data.frame(Time = new.time, Imth = rep(1:12,3))

predict.lm <- predict(Elec.lm, new.data)  # predicts regression function

predict.arma <- predict(best.arma, n.ahead = 36) # predicts errors

elec.pred <- ts(exp(predict.lm + predict.arma$pred), start = 1991,freq = 12) # put predictions together
ts.plot(cbind(Elec.ts, elec.pred), lty = 1:2)  

# We know there is still autocorrelation in the errors.  Predictions could likely
# be improved with a better model.

### 6. Nonstationary Models
## NS3

www <- "http://www.maths.adelaide.edu.au/andrew.metcalfe/Data/cbe.dat"
CBE <- read.table(www,header=T)
head(CBE)
dim(CBE)
Elec.ts <- ts(CBE[, 3], start = 1958, freq = 12)

plot(Elec.ts)
plot(diff(Elec.ts))
plot(diff(log(Elec.ts)))


# Simulating from an ARIMA model
# What model does this simulate from?
set.seed(1)
n <- 1000
x <- w <- rnorm(n)
for (i in 3:n)
{
  x[i] <- 0.5 * x[i - 1] + x[i - 1] - 0.5 *x[i - 2] + w[i] + 0.3 * w[i - 1]
}
plot(x,type="l")
arima(x, order = c(1, 1, 1))


# Or you can use the built in function in R
y <- arima.sim(model = list(order = c(1, 1, 1), ar = 0.5, ma = 0.3), n = 1000)
arima(y, order = c(1, 1, 1))



## NS7

AIC (arima(log(Elec.ts), order = c(1,1,0),seas = list(order = c(1,0,0), 12)))

AIC (arima(log(Elec.ts), order = c(0,1,1),seas = list(order = c(0,0,1), 12)))


# What does this function do?
get.best.arima <- function(x.ts, maxord = c(1,1,1,1,1,1))
{
  best.aic <- 1e8
  n <- length(x.ts)
  for (p in 0:maxord[1]) for(d in 0:maxord[2]) for(q in 0:maxord[3])
    for (P in 0:maxord[4]) for(D in 0:maxord[5]) for(Q in 0:maxord[6])
    {
      fit <- arima(x.ts, order = c(p,d,q),
                   seas = list(order = c(P,D,Q),
                               frequency(x.ts)), method = "CSS")
      fit.aic <- -2 * fit$loglik + (log(n) + 1) * length(fit$coef)
      if (fit.aic < best.aic)
      {
        best.aic <- fit.aic
        best.fit <- fit
        best.model <- c(p,d,q,P,D,Q)
      }
    }
  list(best.aic, best.fit, best.model)
}

best.arima.elec <- get.best.arima( log(Elec.ts),
                                   maxord = c(2,2,2,2,2,2))
best.fit.elec <- best.arima.elec[[2]]
acf( resid(best.fit.elec) )
best.arima.elec [[3]]

ts.plot( cbind( window(Elec.ts,start = 1981),
                exp(predict(best.fit.elec,12)$pred) ), lty = 1:2)



# out of sample prediction
plot(Elec.ts)

Elec.ts.sub <- window(Elec.ts,start=1958,end=c(1984,12))

best.arima.elecSUB <- get.best.arima( log(Elec.ts.sub),
                                      maxord = c(2,2,2,2,2,2))

best.arima.elecSUB[[3]]

predict.vals <- predict(best.arima.elecSUB[[2]],12*6)
Elec.ts.sub2 <- window(Elec.ts,start=1985)


pred.err <- mean((exp(predict.vals$pred)-Elec.ts.sub2)^2)
ts.plot(window(Elec.ts,start = 1975),lty = 1,ylim=c(min(Elec.ts),max(Elec.ts)+1000))
points(time(Elec.ts.sub2), exp(predict.vals$pred),type="l",col="red",lty=2) ## add predicted values




### NS9

# Simulate a GARCH model 
# What is the model?

set.seed(1)
alpha0 <- 0.1
alpha1 <- 0.4
beta1 <- 0.2
w <- rnorm(10000)
a <- rep(0, 10000)
h <- rep(0, 10000)
for (i in 2:10000) {
  h[i] <- alpha0 + alpha1 * (a[i - 1]^2) + beta1 * h[i-1]
  a[i] <- w[i] * sqrt(h[i])
}
plot(a,type="l")
plot(a[1:500],type="l")

acf(a)
acf(a^2)



library(tseries)
a.garch <- garch(a,grad="numerical",trace=FALSE,order=c(1,1)) # numerically estimates parameters
confint(a.garch)  # Do the confidence intervals contain the true values?  Why are the CIs so narrow?

a.garch.res   <- a.garch$res[-1]
acf(a.garch.res)
acf(a.garch.res^2)





## Example with climate series
www <- "http://www.maths.adelaide.edu.au/andrew.metcalfe/Data/stemp.dat"
stemp <- read.table(www,header=T)
stemp.ts <- ts(stemp, start = 1850, freq = 12)
plot(stemp.ts)
stemp.best <- get.best.arima(stemp.ts, maxord = rep(2,6))
stemp.best[[3]]
