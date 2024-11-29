install.packages("tidyverse")
library(tidyverse)
library(survival)
library(survminer)
________________________________________________________________________________
df <- survival::ovarian %>% as_tibble()
df
df %>% glimpse()
________________________________________________________________________________
## Implementation of a Survival Analysis in R
# Load required packages
library(survival)
library(survminer)
library(dplyr)
________________________________________________________________________________
# Import the ovarian cancer dataset and have a look at it
data(ovarian)
glimpse(ovarian)
help(ovarian)
________________________________________________________________________________
# Dichotomize age and change data labels
df$rx <- factor(ovarian$rx, 
                     levels = c("1", "2"), 
                     labels = c("A", "B"))
df$resid.ds <- factor(ovarian$resid.ds, 
                           levels = c("1", "2"), 
                           labels = c("no", "yes"))
df$ecog.ps <- factor(ovarian$ecog.ps, 
                          levels = c("1", "2"), 
                          labels = c("good", "bad"))

# Data seems to be bimodal
hist(df$age)
________________________________________________________________________________
df <- df %>% mutate(age_group = ifelse(age >=50, "old", "young"))
df$age_group <- factor(df$age_group)
________________________________________________________________________________
# Fit survival data using the Kaplan-Meier method
surv_object <- Surv(time = df$futime, event = df$fustat)
surv_object 
##  [1]   59   115   156   421+  431   448+  464   475   477+  563   638 
## [12]  744+  769+  770+  803+  855+ 1040+ 1106+ 1129+ 1206+ 1227+  268 
## [23]  329   353   365   377+
________________________________________________________________________________
fit1 <- survfit(surv_object ~ rx, data = df)
summary(fit1)
## Call: survfit(formula = surv_object ~ rx, data = ovarian)
## 
##                 rx=A 
##  time n.risk n.event survival std.err lower 95% CI upper 95% CI
##    59     13       1    0.923  0.0739        0.789        1.000
##   115     12       1    0.846  0.1001        0.671        1.000
##   156     11       1    0.769  0.1169        0.571        1.000
##   268     10       1    0.692  0.1280        0.482        0.995
##   329      9       1    0.615  0.1349        0.400        0.946
##   431      8       1    0.538  0.1383        0.326        0.891
##   638      5       1    0.431  0.1467        0.221        0.840
## 
##                 rx=B 
##  time n.risk n.event survival std.err lower 95% CI upper 95% CI
##   353     13       1    0.923  0.0739        0.789        1.000
##   365     12       1    0.846  0.1001        0.671        1.000
##   464      9       1    0.752  0.1256        0.542        1.000
##   475      8       1    0.658  0.1407        0.433        1.000
##   563      7       1    0.564  0.1488        0.336        0.946
________________________________________________________________________________
ggsurvplot(fit1, data = ovarian, pval = TRUE)
________________________________________________________________________________
# Examine prdictive value of residual disease status
fit2 <- survfit(surv_object ~ resid.ds, data = df)
ggsurvplot(fit2, data = df, pval = TRUE)
________________________________________________________________________________
# Fit a Cox proportional hazards model
fit.coxph <- coxph(surv_object ~ rx + resid.ds + age_group + ecog.ps, 
                   data = df)
fit.coxph
ggforest (fit.coxph,data = df)
_______________________________________________________________________________

