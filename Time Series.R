##Chapter 1 Times Series Data
#1.4.1 A flying start: Air passenger booking
data(AirPassengers)
AP <- AirPassengers
AP
class(AP)
start(AP); end(AP); frequency(AP)
plot(AP, ylab = "Passengers (1000's)")
layout(1:2)
plot(aggregate(AP))
boxplot(AP ~ cycle(AP))
#1.4.2 Unemployment: Maine
www <- "http://www.massey.ac.nz/~pscowper/ts/Maine.dat"
Maine.month <- read.table(www, header = TRUE)
attach(Maine.month)
class(Maine.month)
Maine.month.ts <- ts(unemploy, start = c(1996, 1), freq = 12)
Maine.annual.ts <- aggregate(Maine.month.ts)/12
layout(1:2)
plot(Maine.month.ts, ylab = "unemployed (%)")
plot(Maine.annual.ts, ylab = "unemployed (%)")
Maine.Feb <- window(Maine.month.ts, start = c(1996,2), freq = TRUE)
Maine.Aug <- window(Maine.month.ts, start = c(1996,8), freq = TRUE)
Feb.ratio <- mean(Maine.Feb) / mean(Maine.month.ts)
Aug.ratio <- mean(Maine.Aug) / mean(Maine.month.ts)
Feb.ratio
Aug.ratio
www <- "http://www.massey.ac.nz/~pscowper/ts/USunemp.dat"
US.month <- read.table(www, header = T)
attach(US.month)
US.month.ts <- ts(USun, start=c(1996,1), end=c(2006,10), freq = 12)
plot(US.month.ts, ylab = "unemployed (%)")
#1.4.3 Multiple time series: Electricity, beer and chocolate data
www <- "http://www.massey.ac.nz/~pscowper/ts/cbe.dat"
CBE <- read.table(www, header = T)
CBE[1:4, ]
class(CBE)
Elec.ts <- ts(CBE[, 3], start = 1958, freq = 12)
Beer.ts <- ts(CBE[, 2], start = 1958, freq = 12)
Choc.ts <- ts(CBE[, 1], start = 1958, freq = 12)
plot(cbind(Elec.ts, Beer.ts, Choc.ts))
AP.elec <- ts.intersect(AP, Elec.ts)
start(AP.elec)
end(AP.elec)
AP.elec[1:3, ]
AP <- AP.elec[,1]; Elec <- AP.elec[,2]
layout(1:2)
plot(AP, main = "", ylab = "Air passengers / 1000's")
plot(Elec, main = "", ylab = "Electricity production / MkWh")
plot(as.vector(AP), as.vector(Elec),
       xlab = "Air passengers / 1000's",
       ylab = "Electricity production / MWh")
abline(reg = lm(Elec ~ AP))
cor(AP, Elec)
#1.4.4 Quarterly exchange rate:GBP to NZ dollar
www <- "http://www.massey.ac.nz/~pscowper/ts/pounds_nz.dat"
Z <- read.table(www, header = T)
Z[1:4, ]
Z.ts <- ts(Z, st = 1991, fr = 4)
plot(Z.ts, xlab = "time / years",
     ylab = "Quarterly exchange rate in $NZ / pound")
Z.92.96 <- window(Z.ts, start = c(1992, 1), end = c(1996, 1))#window function used to extract subseries
Z.96.98 <- window(Z.ts, start = c(1996, 1), end = c(1998, 1))
layout (1:2)
plot(Z.92.96, ylab = "Exchange rate in $NZ/pound",
       xlab = "Time (years)" )
plot(Z.96.98, ylab = "Exchange rate in $NZ/pound",
       xlab = "Time (years)" )
#1.4.5 Global temperature series
www <- "http://www.massey.ac.nz/~pscowper/ts/global.dat"
Global <- scan(www)
Global.ts <- ts(Global, st = c(1856, 1), end = c(2005, 12),
                  fr = 12)
Global.annual <- aggregate(Global.ts, FUN = mean)
plot(Global.ts)
plot(Global.annual)
New.series <- window(Global.ts, start=c(1970, 1), end=c(2005, 12))
New.time <- time(New.series)
plot(New.series); abline(reg=lm(New.series ~ New.time))
#1.5.5 Decomposition in R
plot(decompose(Elec.ts))
Elec.decom <- decompose(Elec.ts, type = "mult")
plot(Elec.decom)
Trend <- Elec.decom$trend
Seasonal <- Elec.decom$seasonal
ts.plot(cbind(Trend, Trend * Seasonal), lty = 1:2)
#1.6 Summary of commands used in examples
read.table #reads data into a data frame
attach #makes names of column variables available
ts #produces a time series object
aggregate #creates an aggregated series
ts.plot #produces a time plot for one or more series
window #extracts a subset of a time series
time #extracts the time from a time series object
ts.intersect #creates the intersection of one or more time series
cycle #returns the season for each value in a series
decompose #decomposes a series into the components trend, seasonal effect, and residual 
stl #decomposes a series using loess smoothing
summary #summarises an R object

##Chapter 2 Correlation
#2.2.1 Expected value
www <- "http://www.massey.ac.nz/~pscowper/ts/Herald.dat"
Herald.dat <- read.table(www, header = T)
attach (Herald.dat)
x <- CO; y <- Benzoa; n <- length(x)
sum((x - mean(x))*(y - mean(y))) / (n - 1)
mean((x - mean(x)) * (y - mean(y)))
cov(x, y)
cov(x,y) / (sd(x)*sd(y))
#2.2.5 Autocorrelation
www <- "http://www.massey.ac.nz/~pscowper/ts/wave.dat"
wave.dat <- read.table (www, header=T) ; attach(wave.dat)
plot(ts(waveht)) ; plot(ts(waveht[1:60]))
acf(waveht)$acf[2]
plot(waveht[1:396],waveht[2:397]) #for lag 1
acf(waveht, type = c("covariance"))$acf[2]
#2.3.2 Example based on air passenger series
data(AirPassengers)
AP <- AirPassengers
AP.decom <- decompose(AP, "multiplicative")
plot(ts(AP.decom$random[7:138]))
acf(AP.decom$random[7:138])
sd(AP[7:138])
sd(AP[7:138] - AP.decom$trend[7:138])
sd(AP.decom$random[7:138])
#2.3.3 Example based on the Font Reservoir series
www <- "http://www.massey.ac.nz/~pscowper/ts/Fontdsdt.dat"
Fontdsdt.dat <- read.table(www, header=T)
attach(Fontdsdt.dat)
plot(ts(adflow), ylab = 'adflow')
acf(adflow, xlab = 'lag (months)', main="")
#2.5 Summary of commands used in examples
mean #returns the mean (average)
var #returns the variance with denominator n − 1
sd #returns the standard deviation
cov #returns the covariance with denominator n − 1
cor #returns the correlation
acf #returns the correlogram (or sets the argument to obtain autocovariance function)

##Chapter 3 Forecasting Strategies
#3.2.2 Building approvals and building activity time series
www <- "http://www.massey.ac.nz/~pscowper/ts/ApprovActiv.dat"
Build.dat <- read.table(www, header=T) ; attach(Build.dat)
App.ts <- ts(Approvals, start = c(1996,1), freq=4)
Act.ts <- ts(Activity, start = c(1996,1), freq=4)
ts.plot(App.ts, Act.ts, lty = c(1,3))
acf(ts.union(App.ts, Act.ts))
app.ran <- decompose(App.ts)$random
app.ran.ts <- window (app.ran, start = c(1996, 3) )
act.ran <- decompose (Act.ts)$random
act.ran.ts <- window (act.ran, start = c(1996, 3) )
acf (ts.union(app.ran.ts, act.ran.ts))
ccf (app.ran.ts, act.ran.ts)
print(acf(ts.union(app.ran.ts, act.ran.ts)))
