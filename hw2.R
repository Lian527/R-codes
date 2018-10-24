##1
#a
www <- "http://www.maths.adelaide.edu.au/andrew.metcalfe/Data/cbe.dat"
CBE <- read.table(www, header = T)
CBE[1:4, ]
class(CBE)
Choc.ts <- ts(CBE[, 1], start = 1958, freq = 12)
plot(Choc.ts)
aggregate(Choc.ts)
plot(aggregate(Choc.ts))
boxplot(Choc.ts~cycle(Choc.ts))
summary(Choc.ts)
#b
decompose(Choc.ts, type="mul")
plot(decompose(Choc.ts,type="mul"))
Choc.decom<- decompose(Choc.ts,type="mult")
ts.plot(Choc.decom$trend, Choc.decom$seasonal+Choc.decom$trend,lty=1:2,
        main="",xlab="Year", ylab="Tonnes")
ts.plot(Choc.decom$trend, Choc.decom$seasonal*Choc.decom$trend,lty=1:2,
        main="Multiplicative",xlab="Year", ylab="Tonnes")
##2
#a
InData <- read.csv(file.choose(), quote = " ' ")
View(InData)
S.ts<- ts(InData$Serendipity,start=1,freq=1)
C.ts<- ts(InData$Cagey,start=1,freq=1)
plot(S.ts)
plot(C.ts)
#b
plot(S.ts[1:15],S.ts[2:16])
plot(C.ts[1:15],C.ts[2:16])
#c
acf(S.ts)#may be autocorrelation 看相邻两条线长度是否相近
acf(C.ts)#may be autocorrelation
##3
#a
www <- "http://www.maths.adelaide.edu.au/andrew.metcalfe/Data/global.dat"
Global<-scan(www)
View(Global)
G.ts<- ts(Global,start=c(1856,1),end=c(2005,12),freq=12)
plot(G.ts)
G_mul<-decompose(G.ts,type="mul")
plot(decompose(G.ts, type="mul"))
sd(G.ts)
sd(G.ts/G_mul$seasonal) 
plot(G_mul$trend*G_mul$seasonal)
#b
resid<-G_mul$random
plot(resid)
resid1<-na.omit(resid)
acf(resid1)
##4
#a
w <- 1:100
x <- w + 1 * rnorm(100) #k=scale,rnom=add independent random variation
y <- w + 1 * rnorm(100)
ccf(x, y) #depend only on lag
x <- w + 10 * rnorm(100)
y <- w + 10 * rnorm(100)
ccf(x, y)
x <- w + 100 * rnorm(100)
y <- w + 100 * rnorm(100)
ccf(x, y)
#b
Time <- 1:370
x <- sin(2 * pi * Time / 37)
y <- sin(2 * pi * (Time + 4) / 37)
ccf(x, y) #x,y very similar depend on time la
x <- sin(2 * pi * Time / 37)+100 * rnorm(370)
y <- sin(2 * pi * (Time + 4) / 37)+100 * rnorm(370)
ccf(x, y) #k=100 difference became bigger, so we can conclude adding independent affects the similarity of two sets.
#ccf shows the change of similarity between x and y.

##additional problem
#1st
n=1:1000
n.ts = ts(n,start=1, freq=12)
plot(n.ts)
nn<-decompose(n.ts,type="mul")
plot(nn)
acf(n.ts)
#2nd
t<- 1:50
y=cos(t)
m.ts = ts(y,start=1, freq=12)
plot(y~t,type="l")
mm<-decompose(m.ts)
plot(mm)
acf(m.ts)
#3rd
b<-c(1,2,3,4,5,6,7,8,9,10)
m<-rep(b,times=5)
m
#a
m.ts<- ts(m,start=c(1),end=c(10),freq=12)
plot(m.ts)
#b
m.decom<- decompose(m.ts,type="mult")
plot(m.decom$trend)
plot(m.decom$seasonal)
#c
acf(m.ts)

##Homework 3
#1
w <- rexp(1000)-1
plot(w, type="l")
hist(w)
acf(w)
#2a
x <- w <- rnorm(100)
for (t in 2:100) x[t] <- -0.9*x[t - 1] + w[t]
x.ar<- ar(x)
x.ar$ar
xp<-rep(NA,10)
xp[1]<-x[100]
for (t in 2:10) xp[t]<- -0.9214468*xp[t-1]
xp

x <- w <- rnorm(100)
for (t in 2:100) x[t] <- -0.5*x[t - 1] + w[t]
x.ar<- ar(x)
x.ar$ar
xp<-rep(NA,10)
xp[1]<-x[100]
for (t in 2:10) xp[t]<- -0.485222*xp[t-1]
xp

x<-rep(NA,100)
w <- rnorm(100)
x[1]<- w[1]
for (t in 2:100) x[t] <- 0.5*x[t - 1] + w[t]
x.ar<- ar(x)
x.ar$ar
xp<-rep(NA,10)
xp[1]<-x[100]
for (t in 2:10) xp[t]<- 0.5908098*xp[t-1]
xp

x<-rep(NA,100)
w <- rnorm(100)
x[1]<- w[1]
for (t in 2:100) x[t] <- 0.9*x[t - 1] + w[t]
x.ar<- ar(x)
x.ar$ar
xp<-rep(NA,10)
xp[1]<-x[100]
for (t in 2:10) xp[t]<- 0.7680603*xp[t-1]
xp
#2b
x <- w <- rnorm(100)
for (t in 2:100) x[t] <- 1.01*x[t - 1] + w[t]
x.ar<- ar(x)
x.ar$ar

x <- w <- rnorm(100)
for (t in 2:100) x[t] <- 1.02*x[t - 1] + w[t]
x.ar<- ar(x)
x.ar$ar

x <- w <- rnorm(100)
for (t in 2:100) x[t] <- 1.05*x[t - 1] + w[t]
x.ar<- ar(x)
x.ar$ar
#4a
x <- w <- rnorm(1000)
x[1]<- w[1]
x[2]<- w[2]
for (t in 3:1000) x[t] <- (5/6)*x[t - 1]-(1/6)*x[t-2] + w[t]
x
#4b
acf(x)
pacf(x)
#4c
x.ar<- ar(x)
x.ar$ar
x.ar$order
#4d
x.ar$ar[1]+c(-2,2)*sqrt(x.ar$asy.var[1])
x.ar$ar[2]+c(-2,2)*sqrt(x.ar$asy.var[1])
#4f
acf(x.ar$resid[-(1:x.ar$order)])
#5c
x <- w <- rnorm(1000)
x[1]<- w[1]
x[2]<- w[2]
for (t in 3:1000) x[t] <- (3/2)*x[t - 1]-(1/2)*x[t-2] + w[t]
x
y<- diff(x)
y
#5d
y.ar<- ar(y)
y.ar$ar
y.ar$ar+c(-2,2)*sqrt(y.ar$asy.var[1])
#5e
acf(y.ar$resid[-(1:y.ar$order)])
#Additional Problems
#1a
load(file="/Users/Lian/Documents/573/ThreeSeries.Rdat")
str(dat)
View(dat)
x1<- dat[,1]
x2<- dat[,2]
x3<- dat[,3]
ts.plot(x1)
lines(lowess(x1,f=1/9),col="red")
ts.plot(x2)
lines(lowess(x2,f=1/9),col="red")
ts.plot(x3)
lines(lowess(x3,f=1/9),col="red")
#b
plot(x1,type="l",main="x1")
plot(x2,type="l",main="x2")
plot(x3,type="l",main="x3")
#c
x2d<- diff(x2)
ts.plot(x2d)
acf(x2d)
x3d<- diff(x3)
ts.plot(x3d)
acf(x3d)
#d
t.test(x2d)
t.test(x3d)
#e
er<- rnorm(100)
x1.ar<- ar(x1)
x1.ar$ar
x1.ar$order
x1.AR<- rep(NA, 100)
for (t in 1:100) x1.AR[t]=x1[t]
acf(x1.AR)$acf
x2.ar<- ar(x2)
x2.ar$ar
x2.ar$order
x2.AR<- rep(NA, 100)
x2.AR<- x2[1]
for (t in 2:100) x2.AR[t]<- 0.9961804*x2.AR[t-1]+er[t]
acf(x2.AR)$acf
x3.ar<- ar(x3)
x3.ar$ar
x3.ar$order
x3.AR<- rep(NA, 100)
x3.AR[1]<- x3[1]
x3.AR[2]<- x3[2]
x3.AR[3]<- x3[3]
for (t in 4:100) x3.AR[t]=0.4385353*x3.AR[t-1]+0.3040421*x3.AR[t-2]+0.2413627*x3.AR[t-3]+er[t]
acf(x3.AR)$acf


##1
#a
set.seed(1)
z <- w <- rnorm(100, sd=25)
for (t in 2:100) z[t] <- 0.5 * z[t - 1] + w[t]
Time <- 1:100
x <- 70 + 2*Time - 3*I(Time^2) + z
plot(x, xlab = "time", type = "l")
#b
x.lm <- lm(x ~ Time + I(Time^2))
coef(x.lm)
sqrt(diag(vcov(x.lm)))
#c
confint(x.lm)
#d
acf(resid(x.lm))
pacf(resid(x.lm))
#e
library(nlme)
ar(resid(x.lm))
x.gls <- gls(x ~ Time+I(Time^2), cor = corAR1(0.4355))
coef(x.gls)
sqrt(diag(vcov(x.gls)))

##2
#5.3.2
www <- "http://www.maths.adelaide.edu.au/andrew.metcalfe/Data/global.dat"
Global <- scan(www)
Global.ts <- ts(Global, st = c(1856, 1), end = c(2005, 12), fr = 12)
temp <- window(Global.ts, start=c(1970,1),end=c(2005,12))
plot(temp)
temp.lm <- lm(temp ~ time(temp))#第一个模型
coef(temp.lm)
confint(temp.lm)
acf(resid(lm(temp ~ time(temp))))
#5.6.3
SIN <- COS <- matrix(nr = length(temp), nc = 6)
for (i in 1:6) {
  COS[, i] <- cos(2 * pi * i * time(temp))
  SIN[, i] <- sin(2 * pi * i * time(temp))
}
TIME <- (time(temp) - mean(time(temp)))/sd(time(temp))
mean(time(temp))
sd(time(temp))
temp.lm1 <- lm(temp ~ TIME + I(TIME^2) +
                 COS[,1] + SIN[,1] + COS[,2] + SIN[,2] +
                 COS[,3] + SIN[,3] + COS[,4] + SIN[,4] +
                 COS[,5] + SIN[,5] + COS[,6] + SIN[,6])
coef(temp.lm1)/sqrt(diag(vcov(temp.lm1)))
temp.lm2 <- lm(temp ~ TIME + SIN[, 1] + SIN[, 2])
coef(temp.lm2)
AIC(temp.lm)
AIC(temp.lm1)
AIC(temp.lm2)
plot(time(temp), resid(temp.lm2), type = "l")
abline(0, 0, col = "red")
acf(resid(temp.lm2))
pacf(resid(temp.lm2))
res.ar <- ar(resid(temp.lm2), method = "mle")
res.ar$ar
sd(res.ar$res[-(1:2)])
acf(res.ar$res[-(1:2)])
library(nlme)#gls检查三个模型
acf(resid(temp.lm))[1]
x.gls <- gls(temp ~ time(temp), cor = corAR1(0.706))#temp.lm
coef(x.gls)
sqrt(diag(vcov(x.gls)))
acf(resid(temp.lm1))[1]
x.gls1 <- gls(temp ~ TIME + I(TIME^2) +
                COS[,1] + SIN[,1] + COS[,2] + SIN[,2] +
                COS[,3] + SIN[,3] + COS[,4] + SIN[,4] +
                COS[,5] + SIN[,5] + COS[,6] + SIN[,6], cor = corAR1(0.709))#temp.lm1
coef(x.gls1)
sqrt(diag(vcov(x.gls1)))
acf(resid(temp.lm2))[1]#same as ar(resid(temp.lm2))$ar
x.gls2 <- gls(temp ~ TIME + SIN[, 1] + SIN[, 2], cor = corAR1(0.705))#temp.lm2
coef(x.gls2)
sqrt(diag(vcov(x.gls2)))
summary(x.gls2)
summary(temp.lm2)

##6
www <- "http://www.maths.adelaide.edu.au/andrew.metcalfe/Data/global.dat"
Global <- scan(www)
Global.ts <- ts(Global, st = c(1856, 1), end = c(2005, 12), fr = 12)
temp <- window(Global.ts, start=c(1970,1),end=c(2005,12))
plot(temp)
SIN <- COS <- matrix(nr = length(temp), nc = 6)
for (i in 1:6) {
  COS[, i] <- cos(2 * pi * i * time(temp))
  SIN[, i] <- sin(2 * pi * i * time(temp))
}
TIME <- (time(temp) - mean(time(temp)))/sd(time(temp))
library(nlme)
x.gls2 <- gls(temp ~ TIME + SIN[, 1] + SIN[, 2], cor = corAR1(0.705))#temp.lm2 using GLS
coef(x.gls2)
sqrt(diag(vcov(x.gls2)))
temp.lm2 <- lm(temp ~ TIME + SIN[, 1] + SIN[, 2])#linear model
temp
#a
confint(x.gls2,level=0.99)
confint(temp.lm2,level=0.99)
#b
plot(temp.lm2$residuals,x.gls2$residuals,col=c("yellow","green"))
#c
ar(temp.lm2$residuals)# AR(2) Coefficients
x.gls.ar2 <- gls(temp ~ TIME + SIN[, 1] + SIN[, 2], cor = corARMA(c(0.4952,0.2973),p=2))
coef(x.gls.ar2)
sqrt(diag(vcov(x.gls.ar2)))
#d
summary(x.gls2)
summary(temp.lm2)
summary(x.gls.ar2)
#e
x.gls <- gls(temp ~ TIME + SIN[, 1] + SIN[, 2], cor = corAR1(0.705))
an<-aggregate(time(temp),FUN="mean")
an_re<-aggregate(temp,FUN="mean")
Model1 <- lm(an_re~an)
confint(Model1, level=0.99)

##6.2
#a
set.seed(1)
b <- c(0.5)
x <- w <- rnorm(100)
for (t in 2:100) {
  for (j in 1) x[t] <- x[t] + b[j] * w[t - j]
}
plot(x, type = "l")
acf(x)
pacf(x)
#b
set.seed(1)
b <- c(2)
x <- w <- rnorm(100)
for (t in 2:100) {
  for (j in 1) x[t] <- x[t] + b[j] * w[t - j]
}
plot(x, type = "l")
acf(x)
pacf(x)

##6.4
#a
data(AirPassengers)
AP <- AirPassengers
AP
class(AP)
start(AP); end(AP); frequency(AP)
summary(AP)
AP.ts <- ts(AP, start = 1949, freq = 12)
plot(AP.ts)
plot(log(AP.ts))
acf(log(AP.ts))
pacf(log(AP.ts))
Time <- 1:length(AP.ts)
Time[1:12]
Imth <- cycle(AP.ts)
Imth[1:12]
AP.lm <- lm(log(AP.ts) ~ Time + I(Time^2) + factor(Imth)) 
summary(AP.lm)
acf(resid(AP.lm))
#b
best.order <- c(0, 0, 0)
best.aic <- Inf  
for (i in 0:2)  
{
  for (j in 0:2) 
  {
    fit.aic <- AIC(arima(resid(AP.lm), order = c(i,0,j)))  
    if (fit.aic < best.aic) 
    {
      best.order <- c(i, 0, j)
      best.arma <- arima(resid(AP.lm), order = best.order)
      best.aic <- fit.aic
    }
  }
}
best.order
best.arma
best.aic
acf(resid(best.arma))
#c
new.time <- seq(length(AP.ts)+1, length = 12)
new.data <- data.frame(Time = new.time, Imth = rep(1:12,1))
predict.lm <- predict(AP.lm, new.data)  
predict.arma <- predict(best.arma, n.ahead = 12) 
AP.pred <- ts(exp(predict.lm + predict.arma$pred), start = 1961,freq = 12)
ts.plot(cbind(AP.ts, AP.pred), lty = 1:2)  

##6.5
#a
for (k in 1:20) rho.k <- (alpha^(k-1) * (alpha + beta) * (1 + alpha*beta))/(1 + alpha*beta + beta^2)
#b
alpha <- 0.7
beta <- -0.5
for (k in 1:20) rho.k <- (alpha^(k-1) * (alpha + beta) * (1 + alpha*beta))/(1 + alpha*beta + beta^2)
for (k in 1:20) rho.k[k] <- (alpha^(k-1) * (alpha + beta) * (1 + alpha*beta))/(1 + alpha*beta + beta^2)
plot(rho.k, type = "l")
#c
set.seed(1)
x <- arima.sim(n = 100, list(ar = 0.7, ma = -0.5))
coef(arima(x, order = c(1, 0, 1)))
acf(x)

set.seed(1)
x <- arima.sim(n = 1000, list(ar = 0.7, ma = -0.5))
coef(arima(x, order = c(1, 0, 1)))
acf(x)

##7
#ar
set.seed(1)
x <- arima.sim(n = 10000, list(ar = -0.6, ma = 0.5))
coef(arima(x, order = c(1, 0, 1)))

best.ar <- arima(x, order=c(0,0,0))
best.aic <- Inf  
for (i in 0:8)  
{  fit.aic <- AIC(arima(x, order = c(i,0,0)))
if (fit.aic < best.aic) 
{
  best.order <- c(i, 0, 0)
  best.ar <- arima(x, order = best.order)
  best.aic <- fit.aic
}

}
best.order
best.ar
best.aic
acf(x)
#ma
set.seed(1)
x <- arima.sim(n = 10000, list(ar = -0.6, ma = 0.5))
coef(arima(x, order = c(1, 0, 1)))

best.ma <- arima(x, order=c(0,0,0))
best.aic <- Inf  
for (j in 0:8)  
{  fit.aic <- AIC(arima(x, order = c(0,0,j)))
if (fit.aic < best.aic) 
{
  best.order <- c(0, 0, j)
  best.ma <- arima(x, order = best.order)
  best.aic <- fit.aic
}

}
best.order
best.ma
best.aic
acf(x)
#arma(1,1)
set.seed(1)
x <- arima.sim(n = 10000, list(ar = -0.6, ma = 0.5))
coef(arima(x, order = c(1, 0, 1)))
AIC(arima(x, order = c(1, 0, 1)))
acf(x)
