library(survival)
lung
dim(lung)
lung=na.omit(lung)

lung$sex=factor(lung$sex,labels = c("male","female"))
str(lung)
fitKM=survfit(Surv(time,status)~sex,data=lung)
summary(fitKM,time=c(1,seq(100,1100,100)))

plot(fitKM,conf.int = FALSE,col = 1:2)
legend("topright",legend = c("Male","Female"),col = 1:2,lty = 1)

survdiff(Surv(time,status)~sex,data=lung)

lung$agecat=ifelse(lung$age>62,1,0)
lung$agecat=factor(lung$agecat,labels = c("young","old"))
str(lung)
fitAge=survfit(Surv(time,status)~agecat,data=lung)
summary(fitAge,time=c(1,seq(100,1100,100)))
plot(fitAge,conf.int = FALSE,col=c("black","orange"))
legend("topright",legend=c("Young","Old"),col = c("black","orange"),lty=1)
survdiff(Surv(time,status)~agecat,data=lung)

modelA=coxph(Surv(time,status)~.-inst-agecat,data=lung,ties="breslow")
summary(modelA)
modelB=update(modelA,~.-age-pat.karno-meal.carno-wt.loss,data=lung,ties="breslow")
summary(modelB)
modelC=update(modelB,~.-ph.karno,data=lung,ties="breslow")
summary(modelC)

install.packages("lmtest")

library(lmtest)

lrtest(modelC,modelA)

predict(modelC,nowdata=data.frame(sex="female",ph.ecog=3),type="risk")

expR=survreg(Surv(time,status)~.-inst-agecat,data=lung)
summary(expR)
AIC(expR,modelA)   

#Q2
muX=function(x){1/(3*(120-x))}
ages=seq(0,120)
muX(ages)
data.frame(ages,mu=muX(ages))
plot(ages,muX(ages),col=2,lty=3)

tpx=function(x, t){((120-x-t)/(120-x))^(1/3)}
paste("The probability is", round(tpx(3,4)-tpx(30,9),5))
