## Binary logistic regression in R

# import and rename dataset
library(kmed)
________________________________________________________________________________
dat <- heart
# select variables
library(dplyr)
dat <- dat |>
  select(
    age,
    sex,
    cp,
    thalach,
    class
  )

# print dataset's structure
str(dat)

# For greater readability, we rename the variables cp, 
# thalach and class with more informative names:

# rename variables
dat <- dat |>
  rename(
    chest_pain = cp,
    max_heartrate = thalach,
    heart_disease = class
  )

# We transform the variables sex and chest_pain into factor and set 
# the labels accordingly:

# recode sex
dat$sex <- factor(dat$sex,
                  levels = c(FALSE, TRUE),
                  labels = c("female", "male")
)

# recode chest_pain
dat$chest_pain <- factor(dat$chest_pain,
                         levels = 1:4,
                         labels = c("typical angina", "atypical angina", "non-anginal pain", "asymptomatic")
)

# recode heart_disease into 2 classes
dat$heart_disease <- ifelse(dat$heart_disease == 0,
                            0,
                            1
)
  
# set labels for heart_disease
dat$heart_disease <- factor(dat$heart_disease,
                            levels = c(0, 1),
                            labels = c("no disease", "disease")
)
________________________________________________________________________________

### the final data frame and some basic descriptive statistics:
# print first 6 observations
head(dat)

# basic descriptive statistics
summary(dat)
________________________________________________________________________________
## Univariable binary logistic regression

#Quantitative independent variable
# save model
m1 <- glm(heart_disease ~ age,
          data = dat,
          family = "binomial"
)
# print results
summary(m1)
## we compute the OR for the age by computing exp(^β1)= exp(0.053).
# OR for age
exp(coef(m1)["age"])

# prob(heart disease) for age = 0
exp(coef(m1)[1]) / (1 + exp(coef(m1)[1]))

# 95% CI for the OR for age
exp(confint(m1,
            parm = "age"
))

# probability of developing a heart disease for a patient aged 30 years old:

# predict probability to develop heart disease
pred <- predict(m1,
                newdata = data.frame(age = c(30)),
                type = "response"
)

# print prediction
pred

# 95% confidence interval for the prediction
lower <- pred$fit - (qnorm(0.975) * pred$se.fit)
upper <- pred$fit + (qnorm(0.975) * pred$se.fit)
c(lower, upper)

# load package
install.packages("ggeffects")
install.packages("sjPlot")
library(sjPlot)

# plot
plot_model(m1,
           type = "pred",
           terms = "age"
) +
  labs(y = "Prob(heart disease)")
________________________________________________________________________________

# Qualitative independent variable

# save model
m2 <- glm(heart_disease ~ sex,
          data = dat,
          family = "binomial"
)

# print results
summary(m2)

# OR for sex
exp(coef(m2)["sexmale"])

# predict the probability of developing a heart disease for a male:

# predict probability to develop heart disease
pred1 <- predict(m2,
                newdata = data.frame(sex = c("male")),
                type = "response"
)

# print prediction
pred1

# plot
plot_model(m2,
           type = "pred",
           terms = "sex"
)
________________________________________________________________________________
### Multivariable binary logistic regression

# save model
m3 <- glm(heart_disease ~ .,
          data = dat,
          family = "binomial"
)

# print results
summary(m3)

## the reduced model.
# save reduced model
m3_reduced <- glm(heart_disease ~ age + sex + max_heartrate,
                  data = dat,
                  family = "binomial"
)

# compare reduced with full model
anova(m3_reduced, m3,
      test = "LRT"
)

# OR and 95% CI
round(exp(cbind(OR = coef(m3), confint(m3))), 3)

## Suppose that this patient is a 32-year-old woman, 
## suffering from chest pain of the type non-anginal and she achieved a maximum 
## heart rate of 150.

# create data frame of new patient
new_patient <- data.frame(
  age = 32,
  sex = "female",
  chest_pain = "non-anginal pain",
  max_heartrate = 150
)

# predict probability to develop heart disease
pred2 <- predict(m3,
                newdata = new_patient,
                type = "response"
)

# print prediction
pred2

# 1. age, sex and chest pain on prob of disease
plot_model(m3,
           type = "pred",
           terms = c("age", "chest_pain", "sex"),
           ci.lvl = NA # remove confidence bands
) +
  labs(y = "Prob(heart disease)")

# 2. max heart rate, chest pain and sex on prob of disease
plot_model(m3,
           type = "pred",
           terms = c("max_heartrate", "chest_pain", "sex"),
           ci.lvl = NA # remove confidence bands
) +
  labs(y = "Prob(heart disease)")
________________________________________________________________________________
## Interaction

# save model without interaction
m4 <- glm(heart_disease ~ age + sex,
          data = dat,
          family = "binomial"
)

# save model with interaction
m4_inter <- glm(heart_disease ~ age * sex,
                data = dat,
                family = "binomial"
)

# We first assess the interaction visually via the plot_model() function:

# plot
plot_model(m4_inter,
           type = "pred",
           terms = c("age", "sex"),
           ci.lvl = NA # remove confidence bands
) +
  labs(y = "Prob(heart disease)")

# For this, we can compare the two models (the one without compared to the one 
#with interaction) with a likelihood ratio test (LRT), 
#using the anova() function:The probability that she develops a heart disease is:

anova(m4, m4_inter,
      test = "LRT"
)

drop1(m4_inter,
      test = "LRT"
)
________________________________________________________________________________
## Model selection

# save initial model
m5 <- glm(heart_disease ~ (age + sex + chest_pain + max_heartrate)^2,
          data = dat,
          family = "binomial"
)

# select best model according to AIC using mixed selection
m5_final <- step(m5,
                 direction = "both", # both = mixed selection
                 trace = FALSE # do not display intermediate steps
)

# display results of final model
summary(m5_final)

## compare models manually using AIC or the pseudo-R2 with the tab_model()

tab_model(m3, m4, m5_final,
          show.ci = FALSE, # remove CI
          show.aic = TRUE, # display AIC
          p.style = "numeric_stars" # display p-values and stars
)
________________________________________________________________________________
## Quality of a model
# Validity of the predictions
# Accuracy

# create a vector of predicted probabilities
preds <- predict(m5_final,
                 newdata = select(dat, -heart_disease), # remove real outcomes
                 type = "response"
)

# if probability < threshold, patient is considered not to have the disease
preds_outcome <- ifelse(preds < 0.5,
                        0,
                        1
)

# transform predictions into factor and set labels
preds_outcome <- factor(preds_outcome,
                        levels = c(0, 1),
                        labels = c("no disease", "disease")
)

# compare observed vs. predicted outcome
tab <- table(dat$heart_disease, preds_outcome,
             dnn = c("observed", "predicted")
)

# print results
tab

accuracy <- sum(diag(tab)) / sum(tab)
accuracy

## Sensitivity and specificity

# sensitivity
sensitivity <- tab[2, 2] / (tab[2, 2] + tab[2, 1])
sensitivity

# specificity
specificity <- tab[1, 1] / (tab[1, 1] + tab[1, 2])
specificity
________________________________________________________________________________
### AUC and ROC curve

# load package
library(pROC)

# save roc object
res <- roc(heart_disease ~ fitted(m5_final),
           data = dat
)

# plot ROC curve
ggroc(res, legacy.axes = TRUE)

# print AUC
res$auc

# plot ROC curve with AUC in title
ggroc(res, legacy.axes = TRUE) +
  labs(title = paste0("AUC = ", round(res$auc, 2)))
________________________________________________________________________________
### Reporting results

# load package
library(gtsummary)

# print table of results
tbl_regression(m5_final, exponentiate = TRUE)











