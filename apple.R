#Apple stock price
Apple <- read.csv(file.choose(), quote = " ' ", head = T)
View(Apple)
str(Apple)
Apple.ts <- ts(Apple[, 6], start = 2017+6/12, freq = 20)
plot(Apple.ts, main="Apple stock price")
lines(lowess(Apple.ts,f=1/6),col="red")
summary(Apple.ts)
decompose(Apple.ts, type="mul")
plot(decompose(Apple.ts, type="mul"))

# arma linear model
Time <- 1:length(Apple.ts)
std<-(Time-mean(Time))/sd(Time) # std is used becaue of the value of time is big -> cubic term can be extremely bigger
Time[1:20]
Imth <- cycle(Apple.ts)
Imth[1:20]
Apple.lm <- lm(Apple.ts ~ std + I(std^2) + I(std^3) + factor(Imth)-1)
summary(Apple.lm) # linear model significant
acf(resid(Apple.lm))
length(Apple.lm)
best.order <- c(0, 0, 0)
best.aic <- Inf  
for (i in 0:2)  
{
  for (j in 0:2) 
  {
    fit.aic <- AIC(arima(resid(Apple.lm), order = c(i,0,j))) 
    if (fit.aic < best.aic) 
    {
      best.order <- c(i, 0, j)
      best.arma <- arima(resid(Apple.lm), order = best.order)
      best.aic <- fit.aic
    }
  }
}
best.order # 2 0 1
best.arma
best.aic # best 1242.324
acf(resid(best.arma)) #white noise
pacf(resid(best.arma))
plot(resid(best.arma), main = "Residual Plot of ARMA Linear")
residual<-resid(best.arma)
acf((residual-mean(residual))^2)
coef(best.arma)

library(tseries)
Apple.garch<-garch(residual,trace=T)
Apple.garch$order
t(confint(Apple.garch))
Apple.garch.res<-resid(Apple.garch)[-1]
acf(Apple.garch.res)
acf(Apple.garch.res^2)
plot(Apple.garch.res, main = "Residual Plot of Garch", type="l")
coef(Apple.garch)

# arma linear model predict
new.time <- seq(length(Apple.ts)+1, length = 20)
std_new <- (new.time-mean(Time))/sd(Time)
new.data <- data.frame(std = new.time, Imth = rep(1:20, 1))
new.data <- data.frame(std = std_new, Imth = rep(1:20, 1))
predict.lm <- predict(Apple.lm, new.data)  
predict.arma <- predict(best.arma, n.ahead = 20) 
Apple.pred <- ts(predict.lm + predict.arma$pred, start = 2031+8/12, freq = 20)
ts.plot(cbind(Apple.ts, Apple.pred), main = "Time Series Plot of Apple stock price Prediction", ylab="Apple.ts Prediction", lty = 1:2) 
x <- c(Apple.ts, Apple.pred)
x <- ts(x, start = 2017+7/12, freq = 20)
lines(lowess(x ,f=1/20), col="red")

#forecast
library(forecast)
model <- auto.arima(Apple.ts, trace = T)
forecast <- forecast(model, h=10)
plot(forecast)
     