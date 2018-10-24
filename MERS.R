MERS <- read.csv(file.choose(),quote="'")
str(MERS)
MERS$Under_disease<-as.factor(MERS$Under_disease)
MERS$Sx<-as.factor(MERS$Sx)
MERS$Fever<-as.factor(MERS$Fever)
MERS$Chill<-as.factor(MERS$Chill)
MERS$Cough<-as.factor(MERS$Cough)
MERS$Dyspnea<-as.factor(MERS$Dyspnea)
MERS$Myalgia<-as.factor(MERS$Myalgia)
MERS$Headache<-as.factor(MERS$Headache)
MERS$GI<-as.factor(MERS$GI)
MERS$Sputum<-as.factor(MERS$Sputum)
MERS$Thr_pain<-as.factor(MERS$Thr_pain)
MERS$Pneu<-as.factor(MERS$Pneu)

#1 Table, response var
Table<-table(MERS$Under_disease, MERS$ï..Trt_out)
Table
prop.table(Table,1)
chisq.test(Table)
MERS$respon<-ifelse(MERS$ï..Trt_out == "death", 1, 0)
table(MERS$ï..Trt_out, MERS$respon)

#2 Backward elim
Fullmodel <- glm(respon~Sex+Hospital+Under_disease+Inf_stage+
                   Incu_stage+Age+Fever+Chill+Cough+Dyspnea+
                   Myalgia+Headache+GI+Sputum+Thr_pain+
                   Pneu, data=MERS, family=binomial(link="logit"))
library(car)
Anova(Fullmodel,type=3)
model1<-update(Fullmodel,~.-Chill)
Anova(model1,type=3)
model1<-update(model1,~.-Cough)
model1<-update(model1,~.-Fever)
model1<-update(model1,~.-Sputum)
model1<-update(model1,~.-Hospital)
model1<-update(model1,~.-Under_disease)
model1<-update(model1,~.-GI)
model1<-update(model1,~.-Thr_pain)
model1<-update(model1,~.-Headache)
model1<-update(model1,~.-Inf_stage)
model1<-update(model1,~.-Sex)
model1<-update(model1,~.-Pneu)
model1<-update(model1,~.-Myalgia)
bac_final <- model1 
summary(bac_final)
AIC(bac_final)
### Is it ok to use Anova type3 to backward elimination

#3 AIC selection w/ Backward
AICbac_final <- step(Fullmodel, direction="backward", trace=0)
AICbac_final
summary(AICbac_final, type=3)
## Provides results on probability
exp(coef(AICbac_final))
exp(confint(AICbac_final))
Anova(AICbac_final,type=3)
library(emmeans)
emmeans(AICbac_final, pairwise ~ Dyspnea, type="response")
### check the fit
library(ResourceSelection)
hoslem.test(AICbac_final$y, fitted(AICbac_final), g=10)





#4 Mumin
library(MuMIn)
options(na.action="na.fail")
allsub <- dredge(AICbac_final, rank="AIC")
head(allsub)
