#导入数据
InData<- read.csv(file.choose(), quote = " ' ", header = T)
View(InData)
str(InData)

#数据清洗
library(VIM)
aggr_plot <- aggr(InData, col=c('blue','red'), 
                  numbers=TRUE, sortVars=TRUE, 
                  labels=names(InData), 
                  cex.axis=.7, gap=3)
InData <- InData[,-7]

aggr_plot <- aggr(InData, col=c('blue','red'), 
                  numbers=TRUE, sortVars=TRUE, 
                  labels=names(InData), 
                  cex.axis=.7, gap=3)
InData <- InData[,-7]

aggr_plot <- aggr(InData, col=c('blue','red'),
                  numbers=TRUE, sortVars=TRUE, 
                  labels=names(InData), 
                  cex.axis=.7,  gap=3)

#多重插补(MI)
library(mice)
NewData <- mice(InData, m = 3, maxit = 4, meth = 'cart', seed = 1234)
CompleteData <- complete(NewData,1)

##Stepwise
library(car)
library(MuMIn)
Fullmodel <- lm(ChangeRatio ~ TotalValue + TotalCapital + FreeFloat 
                + DYR + EPS + BPS + NetCashFlowPerShare 
                + GrossEarningPerShare + ROE + ROA + LiabilityRatio 
                + FlowRate + PE + PB, data = CompleteData)
options(na.action="na.fail")
AllSubsets <- dredge(Fullmodel, rank="AIC")
head(AllSubsets)

Model <- lm(ChangeRatio ~ BPS + EPS + PB + ROE, data = CompleteData)
summary(Model)

#Mix Models
Mix <- lm(ChangeRatio ~ BPS * EPS * PB * ROE, data = CompleteData)
options(na.action="na.fail")
AllSubsets2 <- dredge(Mix, rank="AIC")
head(AllSubsets2)

Mix1 <- lm(ChangeRatio ~ BPS + EPS + PB + ROE + BPS*EPS
                + BPS*PB + BPS*ROE + EPS*PB + EPS*ROE + PB*ROE
                + BPS*EPS*PB + BPS*EPS*ROE + BPS*PB*ROE + EPS*PB*ROE
                +BPS*EPS*PB*ROE, data = CompleteData)
summary(Mix1)

Mix2 <- lm(ChangeRatio ~ BPS + BPS*PB + BPS*PB*EPS + BPS*ROE*EPS 
           + BPS*PB*ROE + PB*ROE*EPS, data = CompleteData)
summary(Mix2)

#AIC
AIC(Model, Mix1, Mix2) # best AIC Mix2
anova(Model, Mix1)
anova(Model, Mix2) 
anova(Mix1, Mix2) # drop Model
summary(Mix1) # better adjusted r square
summary(Mix2)
Anova(Mix1)
#[BPS\ BPS:PB\ BPS:PB:EPS\ BPS:ROE:EPS\ BPS:PB:ROE\ PB:ROE:EPS] significant
