### Cox regression in R
library(knitr)
library(tidyverse)
library(lubridate)
library(survival)
library(survminer)
library(gtsummary)
library(ISwR)
library(KMsurv)
library(pmsampsize)
________________________________________________________________________________
## The lung dataset
The lung dataset contain subjects with advanced lung cancer from the North 
Central Cancer Treatment Group. Some variables we will use to demonstrate methods include:
time: Survival time in days
status: censoring status 0=censored, 1=dead
sex: Male=1 Female=2

str(lung)
head(lung)
________________________________________________________________________________
## Cox regression
coxph(Surv(time, status) ~ sex, data = lung)
________________________________________________________________________________
## Formatting Cox regression results
# We can see a tidy version of the output using the tidy function from the broom package:
library(broom)
broom::tidy(
  coxph(Surv(time, status) ~ sex, data = lung), 
  exp = TRUE
) %>% 
  kable()

coxph(Surv(time, status) ~ sex, data = lung) %>% 
  gtsummary::tbl_regression(exp = TRUE)
_________________________________________________________________________________
## The quantity of interest from a Cox regression model is a hazard ratio (HR). 
#The HR represents the ratio of hazards between two groups at any given point in time.
#The HR is interpreted as the instantaneous rate of occurrence of the event of interest 
#in those who are still at risk for the event. It is not a risk, though it is commonly 
#interpreted as such. If you have a regression parameter β (from column estimate in 
#our coxph) then HR = exp(β)
#A HR < 1 indicates reduced hazard of death whereas a HR > 1 indicates an increased
#hazard of death.So our HR = 0.59 implies that around 0.6 times as many females are 
#dying as males, at any given time.

## Assessing proportional hazards (I)
mv_fit <- coxph(Surv(time, status) ~ sex + age, data = lung)
cz <- cox.zph(mv_fit)
print(cz)

plot(cz)
__________________________________________________________________________________
## Another example of Cox PH Regression
data("melanom", package="ISwR")
str(melanom)
melanom
head(melanom)
attach(melanom)
___________________________________________________________________________________
## Create the survival object:
mod.sex <- coxph(Surv(days,status==1)~sex)
summary(mod.sex)
mod.cov <- coxph(Surv(days,status==1)~sex+log(thick))
summary(mod.cov)
## Verifying the PH Assumption (II)
fit1 <- coxph(Surv(days,status==1) ~ log(thick)+ strata(sex),data=melanom)
plot(survfit(fit1), lty=c(1,1), lwd=c(2,2), col=c("pink", "blue"), fun="cloglog",xlab="Days",ylab="Log-Cumulative Hazard Function", xlim=c(100, 5000) )
legend(100, -1, lty=c(1,1), lwd=c(2,2), col=c("pink", "blue"),legend=c("Females", "Males"),bty="n")
___________________________________________________________________________________
## As before, let’s do a formal test to check PH assumption:
check.ph <- cox.zph(mod.cov, transform="km", global=TRUE)
check.ph

plot(check.ph[2], xlab="Days")
abline(h=0, lty=3,lwd=2)
abline(h=coef(mod.cov)[2], lty=3,col="red",lwd=2)

mod.cov.strat <- coxph(Surv(days,status==1)~sex+log(thick)+strata(ulc))
summary(mod.cov.strat)
______________________________________________________________________________________
## Plotting estimated survival curves in the strata:
plot(survfit(mod.cov.strat), col=c("red","black"),ylim=c(0.5,1), xlab="Days", ylab="Survival")
legend(3000,1, c("Ulcerated Tumor", "Not Ulcerated Tumor"), col=c("red","black"),bty="n", lty=c(1,1)) 
sex.f <- as.factor(sex)
sex.f
mod.cov.strat.f <- coxph(Surv(days,status==1)~sex.f+log(thick)+strata(ulc))
summary(mod.cov.strat.f)
_______________________________________________________________________________________
## In order to estimate survival curves for subjects with certain 
## values of the covariates, we could use the option newdata in survfit:
par(mfrow=c(1,2))
plot(survfit(mod.cov.strat, newdata=data.frame(sex=2,thick=194)), xlab = "Days", ylab="Survival",col=c("red","black"),mark.time=F)
title("Male, tumor thickness=194")
legend(1000, .2, c("Ulcerated Tumor", "Not Ulcerated Tumor"), col=c("red","black"),bty="n", lty=c(1,1)) 
plot(survfit(mod.cov.strat, newdata=data.frame(sex=1,thick=194)), xlab = "Days", ylab="Survival",col=c("red","black"),mark.time=F)
title("Female, tumor thickness=194")
legend(1000, .2, c("Ulcerated Tumor", "Not Ulcerated Tumor"), col=c("red","black"),bty="n", lty=c(1,1))
_________________________________________________________________________________________
par(mfrow=c(1,2))
plot(survfit(mod.cov.strat, newdata=data.frame(sex=2,thick=709)), xlab = "Days", ylab="Survival",col=c("red","black"),mark.time=F) 
title("Male, tumor thickness=709")
legend(1000, .2, c("Ulcerated Tumor", "Not Ulcerated Tumor"), col=c("red","black"),bty="n", lty=c(1,1)) 
plot(survfit(mod.cov.strat, newdata=data.frame(sex=1,thick=709)), xlab = "Days", ylab="Survival",col=c("red","black"),mark.time=F) 
title("Female, tumor thickness=709")
legend(1000, .2, c("Ulcerated Tumor", "Not Ulcerated Tumor"), col=c("red","black"),bty="n", lty=c(1,1)) 
_________________________________________________________________________________________
## Sample size for Survival outcomes
pmsampsize(type = "s", rsquared = 0.051, parameters = 30, rate = 0.065,timepoint = 2, meanfup = 2.07)


