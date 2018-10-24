#R Example 1
# Create a data vector using c()
# = and <- are "assignment operators"
y=c(1,2,3,4)
y
mean(y)
#R is case sensitive!
Y<- 20
Y
#Loading a "built-in" data set
#Use str() or View() to look at the data
data(chickwts)
str(chickwts)
View(chickwts)
summary(chickwts)
#Summary Statistics
#Approaches to referencing a single column in a data.frame
#1 $ Approach
mean(chickwts$weight)
#2 with() function
with(mean(weight), data=chickwts)
#3 data=option (not available with all function!)
aggregate(weight~feed, data=chickwts, FUN=mean)
#4 attach(): Use with caution!
mean(weight)
attach(chickwts)
mean(weight)
detach(chickwts)
#Summary Graphics
boxplot(weight~feed, data=chickwts, col="grey")

#Regression - Corn Example
#(Simple linear) regression is used to model the linear relationship between a numerical response variable and a single numerical predictor. In this example, corn yield is the response and fertilizer(X) is the predictor.
Corn<- read.csv("C:/hess/STAT512/RNotes/Intro and R/Corn.csv")
str(Corn)
#Scatterplot
plot(Yield~X, data=Corn)
#Overlay fitted regression line
abline(lm(Yield~X, data=Corn))
#Regression
Fit<- lm(Yield~X, data=Corn)
Fit
summary(Fit)
#Confidence Intervals
confint(Fit, level=0.95)
#Diagnostic plots
plot(Fit)

#One-way ANOVA - Rice Example
#One-way ANOVA is used to compare means when there are more than two groups. In this example, Tukey adjusted pairwise comparisons are also considered. In this example, the effects of four acids on the growth of rice seedlings are compared in a completely randomized design. Seedling shot dry weights are compared after 7 days in solution.
#Load the dplyr and emmeans packages.
#Remember packages need to be installed before first use!
library(dplyr)
library(emmeans)
#Two approaches to importing the data
Rice<- read.csv("C:/hess/STAT512/RNotes/Intro and R/Rice.csv")
Rice<- read.csv(file.choose())
str(Rice)
boxplot(weight~trt, data=Rice, main="Boxplots")
#Use dplyr package to calculate summary statistics by trt
#First we use the group_by function
#Resulting object is a "Tibble"
RiceGrpd<- group_by(Rice, trt)
RiceGrpd
#Then pass the grouped data to the summarise function
#Summary statistics are automatically computed "by group"
SumStats<- summarise(RiceGrpd,
                     n=n(),
                     mean=mean(weight),
                     sd=sd(weight),
                     SE=sd/sqrt(n))
SumStats
#In practice, we can combine the two steps
SumStats<- summarise(group_by(Rice, trt),
                     n=n(),
                     mean=mean(weight),
                     sd=sd(weight),
                     SE=sd/sqrt(n))
#One-way ANOVA
#Note: trt should be defined as.factor! See str() output above
Fit<- lm(weight~trt, data=Rice)
#Summary() output is not of direct interest here
summary(Fit)
#anova() output is typically of more interest in ANOVA settings
anova(Fit)
#Diagnostic plots
par(mfrow=c(2,2))
plot(Fit)
#Use emmeans package to get pairwise comparisons with Tukey adjustment
emmeans(Fit, pairwise~trt)

#Design Matrix Example(For Illustration)
Corn<- read.csv("Downloads/Corn.csv")
Corn
Fit<- lm(Yield~X, data=Corn)
summary(Fit)
model.matrix(Fit)
X<- model.matrix(Fit)
is.matrix(X)
Y<- Corn$Yield
BetaHat<- solve(t(X)%*%(X)%*%t(X)%*%Y)
BetaHat

#ANOVA as Regression(For Illustration)
library(emmeans)
Indata<- read.csv("Downloads/Corn.csv")
str(InData)
InData$trt<- as.factor(InData$trt)#Important:Need to redefine trt as.factor!
str(InData)
aggregate(y~trt, Fun=mean, data=InData)
#Approach1:one-way ANOVA
Model1<- lm(y~trt, data=InData)
anova(Model1)
emmeans(Model1, pairwise~trt, adjust="none")
model.matrix(Model1)
summary(Model1)
#Approach2:No intercept model
Model2<- lm(y ~ trt-1, data=InData)
model.matrix(Model2)
summary(Model2)
anova(Model2)
#Approach3:Regression with Indicator Variables
Model3<- lm(y ~ x1+x2+x3, data=InData)
summary(Model3)
#Approach4:Regression with Indicator Variables
Model4<- lm(y ~ x2+x3, data=InData)
model.matrix(Model4)
summary(Model4)
#What happens if we don't define trt as a factor?
InData<- read.csv("Downloads/Corn.csv")
str(InData)
Model5<- lm(y~trt, data=InData)
summary(Model5)
model.matrix(Model5)