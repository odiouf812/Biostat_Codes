# install.packages(c("lubridate", "ggsurvfit", "gtsummary", "tidycmprsk"))
library(lubridate)
library(ggsurvfit)
library(gtsummary)
library(tidycmprsk)

# devtools::install_github("zabore/condsurv")
library(condsurv)

## loading the data

lung <- read.csv(file.choose(), header = TRUE)
lung
## Here are the first 6 observations:
head(lung[, c("time", "status", "sex")])

## Creating survival objects and curves
Surv(lung$time, lung$status)[1:10]

## survival function
s1 <- survfit(Surv(time, status) ~ 1, data = lung)
s1
str(s1)

## Kaplan-Meier plots
survfit2(Surv(time, status) ~ 1, data = lung) %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  )

## We can add the confidence interval using add_confidence_interval():
survfit2(Surv(time, status) ~ 1, data = lung) %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  ) + 
  add_confidence_interval()

## Typically we will also want to see the numbers at risk in a table below the x-axis. 
## We can add this using add_risktable():
survfit2(Surv(time, status) ~ 1, data = lung) %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  ) + 
  add_confidence_interval() +
  add_risktable()


## Estimating x -year survival
summary(survfit(Surv(time, status) ~ 1, data = lung), times = 365.25)

## Estimating median survival time
survfit(Surv(time, status) ~ 1, data = lung)
lung %>% 
  filter(status == 1) %>% 
  summarize(median_surv = median(time))

## Comparing survival times between groups
survdiff(Surv(time, status) ~ sex, data = lung)


## survival function by sex
s2 <- survfit(Surv(time, status) ~ sex, data = lung)
s2
str(s2)

## Kaplan-Meier plots by sex
survfit2(Surv(time, status) ~ sex, data = lung) %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  )

