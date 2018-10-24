### Arctic Final Project
PolarIce <- read.csv(file.choose(), quote = " ' ")
View(PolarIce)
str(PolarIce)
Arctic.ts <- ts(PolarIce[, 2], start = 1990, freq = 12)
plot(Arctic.ts, main="Arctic Sea Ice Extend Time Series")
lines(lowess(Arctic.ts,f=1/4),col="red",lty=2,lwd=2)
summary(Arctic.ts)
decompose(Arctic.ts, type="add")
plot(decompose(Arctic.ts, type="add")) #better

# arma linear model
Time <- 1:length(Arctic.ts)
std<-(Time-mean(Time))/sd(Time) # std is used becaue of the value of time is big -> cubic term can be extremely bigger
Time[1:12]
Imth <- cycle(Arctic.ts)
Imth[1:12]
Arctic.lm <- lm(Arctic.ts ~ std + I(std^2) + I(std^3) + factor(Imth)-1)
summary(Arctic.lm) # linear model significant
acf(resid(Arctic.lm))
length(Arctic.lm)
best.order <- c(0, 0, 0)
best.aic <- Inf  
for (i in 0:2)  
{
  for (j in 0:2) 
  {
    fit.aic <- AIC(arima(resid(Arctic.lm), order = c(i,0,j))) 
    if (fit.aic < best.aic) 
    {
      best.order <- c(i, 0, j)
      best.arma <- arima(resid(Arctic.lm), order = best.order)
      best.aic <- fit.aic
    }
  }
}
best.order # 2 0 1
best.arma
best.aic # best 31.5538
acf(resid(best.arma)) #white noise
pacf(resid(best.arma))
plot(resid(best.arma), main = "Residual Plot of ARMA Linear")
residual<-resid(best.arma)
acf((residual-mean(residual))^2)
coef(best.arma)
library(tseries)
Arctic.garch<-garch(residual,trace=T)
Arctic.garch$order
t(confint(Arctic.garch))
Arctic.garch.res<-resid(Arctic.garch)[-1]
acf(Arctic.garch.res)
acf(Arctic.garch.res^2)
plot(Arctic.garch.res, main = "Residual Plot of Garch", type="l")
coef(Arctic.garch)

# arma harmonic model
SIN <- COS <- matrix(nr = length(Arctic.ts), nc = 6)
for (i in 1:6) {
  COS[, i] <- cos(2 * pi * i * time(Arctic.ts))
  SIN[, i] <- sin(2 * pi * i * time(Arctic.ts))
}
TIME <- (time(Arctic.ts) - mean(time(Arctic.ts)))/sd(time(Arctic.ts))
Arctic.lm1 <- lm(Arctic.ts ~ TIME + I(TIME^2) +
                   COS[,1] + SIN[,1] + SIN[,2] +
                   COS[,3] + SIN[,3] + COS[,5])
summary(Arctic.lm1) # harmonic model significant
acf(resid(Arctic.lm1))

best.order <- c(0, 0, 0)
best.aic <- Inf  
for (i in 0:2)  
{
  for (j in 0:2) 
  {
    fit.aic <- AIC(arima(resid(Arctic.lm1), order = c(i,0,j))) 
    if (fit.aic < best.aic) 
    {
      best.order <- c(i, 0, j)
      best.arma <- arima(resid(Arctic.lm1), order = best.order)
      best.aic <- fit.aic
    }
  }
}
best.order # 2 0 2
best.arma
best.aic # 52.41859
acf(resid(best.arma)) # white noise
plot(resid(best.arma), main = "Residual Plot of ARMA Harmonic")

# arima
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
best.arima.Arctic <- get.best.arima(resid(Arctic.lm),
                                    maxord = c(3,3,3,3,3,3))
best.fit.Arctic <- best.arima.Arctic[[2]]
acf(resid(best.fit.Arctic) ) # white noise
pacf(resid(best.fit.Arctic) )
best.arima.Arctic [[3]] # 0 0 0 1 0 0
best.arima.Arctic [[1]] # aic 41.67164
best.fit.Arctic #coefficients
plot(resid(best.fit.Arctic), main="Residual Plot of ARIMA")
best.fit.Arctic

# arma linear model predict
new.time <- seq(length(Arctic.ts)+1, length = 60)
std_new <- (new.time-mean(Time))/sd(Time)
new.data <- data.frame(std = new.time, Imth = rep(1:12, 5))
new.data <- data.frame(std = std_new, Imth = rep(1:12, 5))
predict.lm <- predict(Arctic.lm, new.data)  
predict.arma <- predict(best.arma, n.ahead = 60) 
Arctic.pred <- ts(predict.lm + predict.arma$pred, start = 2011+3/12, freq = 12)
ts.plot(cbind(Arctic.ts, Arctic.pred), main = "Time Series Plot of Arctic and Prediction", ylab="Arctic.ts Prediction", lty = 1:2) 
x <- c(Arctic.ts, Arctic.pred)
x <- ts(x, start = 1990, freq = 12)
lines(lowess(x ,f=1/5), col="red")

# simulation
Arctic.ts <- ts(PolarIce[, 2], start = 1990, freq = 12)
new.time <- seq(1, length = length(Arctic.ts)+12*5-3)
std_new <- (new.time-mean(new.time))/sd(new.time)
new.data <- data.frame(std = std_new, Imth = rep(1:12,26))
Arctic.lm
predict.lm <- predict(Arctic.lm, new.data)
predict.arma <- predict(best.arma, n.ahead = length(new.time)) 
Arctic.pred <- ts(predict.lm + predict.arma$pred, start = 1990, freq = 12)
ts.plot(cbind(Arctic.ts, Arctic.pred), main = "Simulation Plot", ylab="Arctic.ts Simulation", lty = 1:2, col=c("black","red")) 
x <- Arctic.pred
lines(lowess(x,f=1/5), col="red")

# forecast
library(forecast)
fit <- auto.arima(Arctic.ts, trace=T)
AIC(fit)
Model1 <- forecast(fit, h=60)
plot(Model1)
lines(lowess(Model1 , f=1/4),col="red")
acf(resid(fit))
pacf(resid(fit))
acf(resid(Model1))
pacf(resid(Model1))
acf(resid(fit)^2)
