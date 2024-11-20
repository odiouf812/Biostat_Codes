smoke <- read.csv(file.choose(), header = TRUE)
str(smoke)

## Data exploration
### For descriptive statistics, we use epidisplay::codebook as before.
epiDisplay::codebook(smoke)
### In addition, we are also interested to look at the observed rates,
smoke %>% mutate(rate = round(case/person_yrs, 4))

## Univariable analysis
### cigar_day
pois_case1 = glm(case ~ cigar_day, data = smoke, family = "poisson", 
                 offset = log(person_yrs))
summary(pois_case1) 
### smoke_yrs
pois_case2 = glm(case ~ smoke_yrs, data = smoke, family = "poisson", 
                 offset = log(person_yrs))
summary(pois_case2)

## Multivariable analysis
pois_case = glm(case ~ cigar_day + smoke_yrs, data = smoke, 
                family = "poisson", offset = log(person_yrs))
summary(pois_case)

## Interaction
pois_casex = glm(case ~ cigar_day * smoke_yrs, data = smoke,
                 family = "poisson", offset = log(person_yrs))
summary(pois_casex)

## Model fit assessment
epiDisplay::poisgof(pois_case)
### The P-value of chi-square goodness-of-fit is more than 0.05, which indicates the model has good fit.
### Model-to-model AIC comparison
AIC(pois_case1, pois_case2, pois_case)
### The comparison by AIC clearly shows that the multivariable model pois_case is the best model as it has the lowest AIC value.
### Scaled Pearson chi-square statistic using quasipoisson
qpois_case_summ = summary(glm(case ~ cigar_day + smoke_yrs, data = smoke, 
                              family = "quasipoisson", 
                              offset = log(person_yrs)))
qpois_case_summ$dispersion
### The value of dispersion i.e. the scaled Pearson chi-square statistic is close to 1. This again indicates that the model has good fit.

## Standardized residuals
std_res = rstandard(pois_case)
std_res[abs(std_res) > 1.96]
index = names(std_res[abs(std_res) > 1.96])
### points of discrepancies
cbind(smoke[index,], case_pred = pois_case$fitted[index]) %>%
  mutate(rate = round(case/person_yrs, 4), 
         rate_pred = round(case_pred/person_yrs, 4))

install.packages("gtsummary")
library(gtsummary)
pois_case_final = pois_case
tbl_regression(pois_case_final, exponentiate = TRUE)