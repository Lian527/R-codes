InData <- read.csv(file.choose(), quote = " ' ", row.names = 1)
View(InData)
pairs(InData)
cor(InData)
Model1<- lm(TIME~PROTEIN,
            data=InData)
summary(Model1)
Model2<- lm(TIME~ANTIBIO,
            data=InData)
summary(Model2)
Model3<- lm(TIME~SUPPLEM,
            data=InData)
summary(Model3)
Model4 <- lm(TIME~PROTEIN+ANTIBIO+SUPPLEM
             , data=InData)
summary(Model4)
plot(Model4)
confint(Model4)

InData <- read.csv(file.choose(), quote = " ' ")
View(InData)
pairs(InData)
Model1<- lm(BodyFat~Triceps+Thigh+Midarm
            , data=InData)
summary(Model1)
c1 = matrix(c(0,1,0,0,
              0,0,1,0,
              0,0,0,1), nrow=3, byrow=TRUE)
lht(Model1,c1,rhs=c(0,0,0))
library(car)
c2<- c(0,1,0,0)
lht(Model1,c2,rhs=c(2))
c3 <- matrix(c(0,0,1,0,
               0,0,0,1),nrow=2, byrow=TRUE)
lht(Model1,c3,rhs=c(0,0))
summary(Model1)
Model1<- lm(BodyFat~Triceps+Midarm
            , data=InData)
summary(Model1)
plot(Model1)
NewData<- data.frame(Triceps=20, Midarm=25)
predict(Model1, NewData,interval="prediction")
predict(Model1, NewData,interval="confidence")
predict(Model1, NewData,interval="prediction")

#hw3
InData <- read.csv(file.choose(), quote = " ' ")
View(InData)
library(car) 
str(InData)
scatterplot(Yield~Days,data=InData)
Model1<- lm(Yield~Days,data=InData)#linear regression model
plot(Model1)
summary(Model1)
Model2<- lm(Yield~Days+I(Days^2),data=InData)#quadratic regression model
plot(Model2)
summary(Model2)
Model3<- lm(Yield~Days+I(Days^2)+I(Days^3),data=InData)#cubic regression model
plot(Model3)
summary(Model3)
c<- matrix(c(0,1,0,0,
             0,0,1,0,
             0,0,0,1), nrow=3, byrow=TRUE)
lht(Model3,c,rhs=c(0,0,0))

# HW 4
InData <- read.csv(file.choose(), quote = " ' ")
View(InData)
str(InData)
# 1
library(car)
Model1 <- lm(Yield~Days+I(Days^2)+I(Days^3),data=InData) # cubic regression model
summary(Model1)
Model2 <- lm(Yield~Days+I(Days^2),data=InData) # quadratic regression model
summary(Model2)
Model3 <- lm(Yield~Days,data=InData)#linear regression model
summary(Model3)
# 2
extractAIC(Model1)
extractAIC(Model2)
extractAIC(Model3)
# 3
InData <- read.csv(file.choose(), quote = " ' ")
View(InData)
str(InData)
FullModel <- lm(BodyFat~., InData)
summary(FullModel)
FullModel1 <- lm(BodyFat~.-Thigh, InData)
summary(FullModel1)
# 4
NullModel <- lm(BodyFat~1, InData)
Model1<- NullModel
add1(Model1, scope=FullModel, test="F")
Model2 <-  update(Model1,~.+Thigh)
add1(Model2, scope=FullModel, test="F")
# 5
library(MuMIn)
FullModel <- lm(BodyFat ~ ., data = InData)
options(na.action = "na.fail")
dredge(FullModel, rank = "AIC", extra = c("R^2"))


##1
#A
InData <- read.csv(file.choose(), quote = " ' ")
View(InData)
str(InData)
Model1<- glm(formula=cbind(withHA,withoutHA)~CK , family=binomial("logit"),data=InData)
summary(Model1)
#B
total<- InData[,2]+InData[,3]
total
InData$rate<- InData$withHA/total
plot(rate~CK, data=InData)
CKnew<- seq(0,550,1)
phta<- predict(Model1, list(CK=CKnew),type="response")
lines(phta~CKnew)
#C
exp(Model1$coef)
exp(confint(Model1))
#D
Model1$coef
exp(0.03510439)
exp(0.03510439*10)
#E
NullModel<- glm(cbind(withHA,withoutHA)~1,family=binomial(link="logit"),data=InData)
1-logLik(Model1)/logLik(NullModel)
#F
library(MASS)
probs<- seq(0.1,0.9,0.05)
ld<- dose.p(Model1,cf=1:2,p=probs)
ld

##2
#A
InData <- read.csv(file.choose(), quote = " ' ")
View(InData)
str(InData)
SumTable<- table(InData$low,InData$race)
SumTable
prop.table(SumTable,2)
chisq.test(SumTable)
#B
SumTable2<- table(InData$low,InData$smoke)
SumTable2
prop.table(SumTable2,2)
chisq.test(SumTable2)
#C
library(emmeans)
InData$resp<-ifelse(InData$low == "1",1,0)
table(InData$low, InData$resp)
InData$race<-as.factor(InData$race)
InData$smoke<-as.factor(InData$smoke)
Model1<-glm(resp~smoke,data=InData,family=binomial(link="logit"))
emmeans(Model1, pairwise~smoke, type="response")
#D
library(car)
library(MuMIn)
Fullmodel<- glm(resp~age+mwt+race+smoke,data=InData,family=binomial(link="logit"))
options(na.action="na.fail")
AllSubsets<- dredge(Fullmodel,rank="AIC")
head(AllSubsets)
FinalModel<- glm(resp~mwt+race+smoke,data=InData,family=binomial(link="logit"))
Anova(FinalModel,type=3)
#E
InData$smoke <- factor(InData$smoke,levels(InData$smoke)[c(2,1)])
table(InData$smoke,InData$low)
summary(FinalModel)
exp(coef(FinalModel))
exp(confint(FinalModel))
#F
emmeans(FinalModel, pairwise~smoke,type="response")
#G
library(ResourceSelection)
hoslem.test(FinalModel$y,fitted(FinalModel),g=10)


##1
Irrigation<- read.csv(file.choose(), quote = " ' ")
View(Irrigation)
str(Irrigation)
library(dplyr)
library(car)
library(emmeans)
Irrigation$Farm<-as.factor(Irrigation$Farm)             
SumStats<- summarise(group_by(Irrigation, Method),
                     n=n(),
                     mean=mean(Weight),
                     sd=sd(Weight),
                     SE=sd/sqrt(n))
SumStats
library(ggplot2)
ggplot(SumStats,aes(x=Method, y=mean))+geom_bar(stat="identity")+geom_errorbar(aes(ymin=mean-SE,ymax=mean+SE),width=0.2)
Model1<- lm(Weight~Method+Farm, data= Irrigation)
Anova(Model1, type=3)
emmeans(Model1, pairwise~Method)
par(mfrow=c(1,2))
plot(Model1)
summary(Model1)
library(multcompView)
emout <- emmeans(Model1, pairwise ~ Method)
emout
cld(emout)
Anova(Model1,type=3)
Model2<- lm(Weight ~Method , data=Irrigation)
Anova(Model2,type=3)

##2
GrassMiss<- read.csv(file.choose(), quote = " ' ")
View(GrassMiss)
str(GrassMiss)
library(car)
library(emmeans)
GrassMiss$Block<- as.factor(GrassMiss$Block)
GrassMiss
aggregate(Y~Trt, FUN=mean, data=GrassMiss) 
Model3<- lm(Y~Block+Trt, data=GrassMiss)
Anova(Model3, type=3)
emmeans(Model3, pairwise~Trt)
summary(Model3)
Out<- data.frame(GrassMiss,Yhat=predict(Model3,newdata=GrassMiss))
Out
mean<- (2.414588+2.410588+2.428235+2.502588+2.482235)/5
mean


##1
#B
InData<- read.csv(file.choose(), quote = " ' ")
View(InData)
str(InData)
library(dplyr)
library(car)
library(emmeans)
InData$Roadway<- as.factor(InData$Roadway)  
with(interaction.plot(Treatment, Concentration, cracks), data=InData)
SumStats<- summarise(group_by(InData, Treatment:Concentration),
                     n=n(),
                     mean=mean(cracks),
                     sd=sd(cracks),
                     SE=sd/sqrt(n))
SumStats
#C
Model1<- lm(cracks ~ Roadway + Treatment*Concentration, data = InData)
Anova(Model1, type=3)
#D
Anova(Model1, type=3)
#E
emmeans(Model1, pairwise~(Concentration|Treatment))
#F
emmeans(Model1, pairwise~(Treatment|Concentration))

##2
#B
InData<- read.csv(file.choose(), quote = " ' ")
View(InData)
str(InData)
library(car)
library(emmeans)
InData$method<- as.factor(InData$method)
InData$row<- as.factor(InData$row)
InData$col<- as.factor(InData$col)
with(interaction.plot(micro, method, y), data=InData)
SumStats<- summarise(group_by(InData, micro:method),
                     n=n(),
                     mean=mean(y),
                     sd=sd(y),
                     SE=sd/sqrt(n))
SumStats
#C
Model2<- lm(y ~ row + col + method + micro*method, data = InData)
Anova(Model2, type=3)
#D
Anova(Model2, type=3)
#F
emmeans(Model2 , pairwise ~ micro:method)


Shrimp <- read.csv(file.choose(), quote = " ' ")
View(Shrimp)
options(contrasts=c("contr.sum", "contr.poly"))
library(dplyr)
library(car)
library(emmeans)
#1
Shrimp$Temp <- as.factor(Shrimp$Temp)
Shrimp$Dens <- as.factor(Shrimp$Dens)
Shrimp$Sal <- as.factor(Shrimp$Sal)
str(Shrimp)
aggregate(Gain ~ Temp , data = Shrimp, FUN = mean)
SumStats<- summarise(group_by(Shrimp, Temp, Dens, Sal),
                     n=n(),
                     mean=mean(Gain),
                     sd=sd(Gain),
                     SE=sd/sqrt(n))
SumStats
library(ggplot2)
qplot(x = Sal, y = mean, colour = Dens, group = Dens, data = SumStats) + geom_line() + facet_grid(. ~ Temp)
#2
Model1 <- lm(Gain ~ Temp*Dens*Sal, data = Shrimp)
Anova(Model1, type = 3)
#3
emmeans(Model1, pairwise ~ Dens|Temp*Sal)
#5
emmeans(Model1, pairwise ~ Dens)
#8
Model2 <- lm(Gain ~ Dens*Sal, data = Shrimp[Shrimp$Temp=="25",])
Anova(Model2, type = 3)
Model3 <- lm(Gain ~ Dens*Sal, data = Shrimp[Shrimp$Temp=="35",])
Anova(Model3, type = 3)
#9
emmeans(Model2, pairwise ~ Dens|Sal)
emmeans(Model3, pairwise ~ Dens|Sal)


##1
InData <- read.csv(file.choose(), quote = " ' ")
View(InData)
str(InData)
library(emmeans)
library(car)
#A
InData$Person <- as.factor(InData$Person)
with(table(Person, Treatments), data = InData)
#C
Model1 <- lm(AreaRed ~ Person + Treatments, data = InData)
Anova(Model1, type = 3)
#D
emmeans(Model1, pairwise ~ Treatments)
emout <- emmeans(Model1, pairwise ~ Treatments)
emout
cld(emout$emmeans)
detach("package:emmeans")
#E
library(Matrix)
library(lme4) 
library(lmerTest)
library(pbkrtest)
library(emmeans)
#F
Model2 <- lmer(AreaRed ~ (1|Person) + Treatments, data = InData)
anova(Model2, ddf="Kenward-Roger")
#G
emmeans(Model2, pairwise ~ Treatments)
emout <- emmeans(Model2, pairwise ~ Treatments)
emout
cld(emout$emmeans)
#H

##2
Varieties <- read.csv(file.choose(), quote = " ' ")
View(Varieties)
str(Varieties)
library(emmeans)
library(car)
#A
Model1 <- lm(Yield ~ Var, data = Varieties) 
Anova(Model1, type = 3)
#C
emmeans(Model1, pairwise ~ Var)
#D
library(Matrix)
library(lme4) 
library(pbkrtest)
library(emmeans)
Model2 <- lmer(Yield ~ (1|Var), data=Varieties)
summary(Model2)
#E
library(lmerTest)
rand(Model2)
#F
BLUPs <- ranef(Model2)$Var + 24.381
BLUPs


##Lab1
#1
InData<- read.csv(file.choose(), quote = " ' ")
View(InData)
library(ggplot2)
library(emmeans)
str(InData)
InData$Age<- as.factor(InData$Age)
p<- qplot(Ppt,FisherAlpha,shape=Age,color=Age,data=InData)
p+geom_smooth(method="lm",se=FALSE,fullrange=T)
#2
library(dplyr)
SumStats<- summarise(group_by(InData,Age),
                     n=n(),
                     mean=mean(FisherAlpha),
                     sd=sd(FisherAlpha),
                     SE=sd/sqrt(n))
SumStats
#3
library(car)
model1<- lm(FisherAlpha~Ppt,data=InData)
Anova(model1,type=3)
library(MuMIn)
options(na.action="na.fail")
dredge(model1,rank="AIC")
#4
model2<- lm(FisherAlpha~Ppt+Age,data=InData)
summary(model2)
extractAIC(model2)
Anova(model2,type=3)
#5
model3<-lm(FisherAlpha~Ppt*Age,data=InData)
summary(model3)
extractAIC(model3)
Anova(model3,type=3)
#6 第五个模型，pvalue不significant，用backward后age和ppt会significant
#7 
library(MuMIn)
options(na.action="na.fail")
dredge(model3,rank="AIC")
#8
plot(model2)#some outliers, data seems in good fit
#9
library(emmeans)
emmeans(model2,pairwise~Age)
#10
#11
InData<- read.csv(file.choose(), quote = " ' ")
View(InData)
library(ggplot2)
library(emmeans)
str(InData)
InData$Topography<- as.factor(InData$Topography)
p<- qplot(Ppt,FisherAlpha,shape=Topography,color=Topography,data=InData)
p+geom_smooth(method="lm",se=FALSE,fullrange=T)
#12
model4<-lm(FisherAlpha~Ppt*Topography,data=InData)
summary(model4)
extractAIC(model4)
Anova(model4,type=3)
#13 #no interaction
model5<- lm(FisherAlpha-1~Ppt*Topography,data=InData)
summary(model5)
#14
library(emmeans)
lstrends(model4,pairwise~Topography,var="Ppt")
#15
emmeans(model4,pairwise~Topography,at=list(Ppt=2000))
emmeans(model4,pairwise~Topography,at=list(Ppt=3000))
