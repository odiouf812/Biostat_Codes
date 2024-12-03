### Multiple Linear Regression in R
# Quick data exploration

## The Multiple Linear Regression Assumptions

An important aspect when building a multiple linear regression model is to make sure that the following key assumptions are met.

The residual values are normally distributed. This can be checked by either using a normal probability plot or a histogram.
There must be a linear relationship between the dependent and the independent variables. This can be illustrated by scatterplots 
showing a linear or curvilinear relationship.
Then, multicollinearity is another assumption, meaning that the independent variables are not highly correlated with each other. 
Multicollinearity makes it difficult to identify which variables better explain the dependent variable. This assumption is verified by 
computing a matrix of Pearson’s bivariate correlations among all the independent variables. If there is no collinearity in the data, 
then all the values should be less than 0.8. 
The homoscedasticity assumes that the variance of the residual errors is similar across 
the value of each independent variable. One way of checking that is through a plot of the predicted values against the standardized 
residual values to see if the points are equally distributed across all the values of the independent variables.

________________________________________________________________________________________________________________________________________
## Loading required R packages
library(tidyverse)
library(ggpubr)
theme_set(theme_pubr())
_________________________________________________________________________________________________________________________________________
# Examples of data and problem
# Load the package
coronary <- read_dta("C:/Users/Lenovo/Downloads/coronary.dta")
glimpse(coronary)
__________________________________________________________________________________________________________________________________________
## Simple Linear Regression
# Data exploration
install.packages("gtsummary")
library(gtsummary)
coronary %>% 
  select(chol, dbp) %>% 
  tbl_summary()
## histograms and box-and-whiskers plots,
hist_chol <-
  ggplot(coronary, aes(chol)) + 
  geom_histogram(color = "black", fill = "white")
hist_dbp <-
  ggplot(coronary, aes(dbp)) + 
  geom_histogram(color = "black", fill = "white")
bplot_chol <- 
  ggplot(coronary, aes(chol)) + 
  geom_boxplot()
bplot_dbp <- 
  ggplot(coronary, aes(dbp)) + 
  geom_boxplot()

ggarrange(hist_chol, bplot_chol, hist_dbp, bplot_dbp)
__________________________________________________________________________________________________
## Univariable analysis
slr_chol <- lm(chol ~ dbp, data = coronary)
summary(slr_chol)
library(tidyverse)
library(dplyr)
library(broom)
tidy(slr_chol, conf.int = TRUE)
__________________________________________________________________________________________________
# Model fit assessment
library(rsq)
library(dplyr)
library(mhurdle)
rsq(slr_chol)
__________________________________________________________________________________________________
# assess the model fit by a scatter plot,
plot_slr <- 
  ggplot(coronary, aes(x = dbp, y = chol)) + 
  geom_point() + geom_smooth(method = lm)
plot_slr
__________________________________________________________________________________________________
# Presentation and interpretation
tbl_regression(slr_chol)
__________________________________________________________________________________________________
# the model equation,
chol=3.0+0.04×dbp
summary(slr_chol)
__________________________________________________________________________________________________
## Multiple Linear Regression
# Data exploration
# numerical
coronary %>% 
  select(-id, -cad, -race, -gender) %>% 
  tbl_summary()
# categorical
coronary %>% 
  dplyr::select(race, gender) %>% 
  tbl_summary() 

# the bivariate correlation statistics between the numerical variables.
# Github
library(devtools)
install_github("ggobi/ggally")
# CRAN
install.packages("GGally")
library(GGally)
coronary %>% 
  dplyr::select(race, gender) %>% 
  ggpairs()
__________________________________________________________________________________________________
### Univariable analysis
slr_chol0 <- lm(chol ~ 1, data = coronary)  # intercept only model
add1(slr_chol0, scope = ~ sbp + dbp + age + bmi + race + gender, 
     test = "F")
#In the context of exploratory research, 
#we want to choose only variables with P-values < 0.25 to be included in MLR. 
#To obtain the P-values, you may perform separate SLRs for each of the predictors (on your own). 
#However, obtaining P-value for each predictor is easy by add1() function.
___________________________________________________________________________________________________
# Multivariable analysis
mlr_chol <- lm(chol ~ dbp + bmi + race, data = coronary)
summary(mlr_chol)

# Stepwise automatic variable selection

# forward
mlr_chol_stepforward <- step(slr_chol0, scope = ~ dbp + bmi + race, 
                             direction = "forward")

# Backward selection starts with a model containing all variables. 
# Then, it proceeds by removing one variable after another, 
# of which it aims to find the model with the lowest AIC.
# backward
mlr_chol_stepback <- step(mlr_chol, direction = "backward")

# Bidirectional selection, as implemented in R, starts as with the model with all variables. 
# Then, it proceeds with removing or adding variables, which combines both forward and backward selection methods. 
# It stops once it finds the model with the lowest AIC.
# both
mlr_chol_stepboth <- step(mlr_chol, direction = "both")

## Preliminary model
# Let say, after considering the P-value, stepwise selection (in exploratory research) 
# and expert opinion, we decided that our preliminary model is,

# chol ~ dbp + race

# and we fit the model again to view basic information of the model,

mlr_chol_sel <- lm(chol ~ dbp + race, data = coronary)
summary(mlr_chol_sel)

# Interaction
summary(glm(chol ~ dbp + race + dbp:race, data = coronary))
________________________________________________________________________________________________________________
# Model fit assessment
rsq(mlr_chol_sel, adj = TRUE)

# Histogram and box-and-whiskers plot
rraw_chol <- resid(mlr_chol_sel)
hist(rraw_chol)
boxplot(rraw_chol)

# Scatter plots
rstd_chol <- rstandard(mlr_chol_sel)  # standardized residuals
pstd_chol <- scale(predict(mlr_chol_sel))  # standardized predicted values
plot(rstd_chol ~ pstd_chol, xlab = "Std predicted", ylab = "Std residuals")
abline(0, 0)  # normal, linear, equal variance

plot(rraw_chol ~ coronary$dbp, xlab = "DBP", ylab = "Raw Residuals")
abline(0, 0)
_____________________________________________________________________________________________________________
# Presentation and interpretation
After passing all the assumption checks, we may now decide on our final model. 
We may rename the preliminary model mlr_chol_sel to mlr_chol_final for easier reference.

mlr_chol_final <- mlr_chol_sel
# install.packages("gtsummary")
library(gtsummary)
tbl_regression(mlr_chol_final)

# It will be useful to be able to save the output in the spreadsheet format for later use. 
# We can use tidy() function in this case and export it to a .csv file,
library(broom)
tib_mlr = tidy(mlr_chol_final, conf.int = TRUE)
write.csv(tib_mlr, "mlr_final.csv")
_____________________________________________________________________________________________________________


