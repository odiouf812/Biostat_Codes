### GEE

GEE is a population-averaged (e.g., marginal) model, whereas mixed effects models 
are subject/cluster-specific. For example, we can use GEE when we are interested 
in exploring the population average effect. On the other hand, when we are interested 
in individual/cluster-specific effects, we can use the mixed effects models.

Also, in contrast to the mixed effects models where we assume error term and beta 
coefficients for subject both follow normal distribution, GEE is distribution free. 
However, in GEE, we assume some correlation structures for within subjects/clusters. 
Hence, we can use GEE for non-normal data such as skewed data, binary data, or count data.

________________________________________________________________________________

# Load required packages
knitr::opts_chunk$set(echo = TRUE)
require(HSAUR2)
library(gee)
library(MuMIn)
library(geepack)
library("lme4")
require(afex)
________________________________________________________________________________
## Data Description

In addition to BtheB dataset in part 1, we used respiratory data from HSAUR2 
package to demonstrate the analysis with non-normal responses:
  
  The response variable in this dataset is status (the respiratory status), 
which is a binary response Other covariates are: treatment, age, gender, the study 
center. The response has been measured at 0, 1, 2, 3, 4 mths for each subject

data("respiratory", package = "HSAUR2")
head(respiratory)
________________________________________________________________________________
### GEE models

# Binary response

library(gee)
data("respiratory", package = "HSAUR2")
## Data manipulation
resp <- subset(respiratory, month > "0")
resp$baseline <- rep(subset(respiratory, month == "0")$status, rep(4, 111))
## Change the response to 0 and 1
resp$nstat <- as.numeric(resp$status == "good")
resp$month <- resp$month[, drop = TRUE]
________________________________________________________________________________
Now we will fit a regular glm, i.e., model without random effect or any 
correlation structures. For binary outcomes, the estimated coefficients are 
log odds.

## Regular GLM 

resp_glm <- glm(status ~ centre + treatment + gender + baseline+ age, 
                data = resp, family = "binomial")
summary(resp_glm)

# Now we will fit GEE with independent correlation structure:

## GEE with identity matrix
resp_gee.in <- gee(nstat ~ centre + treatment + gender + baseline + age, 
                   data = resp, family = "binomial", 
                   id = subject,corstr = "independence", 
                   scale.fix = TRUE, scale.value = 1)

summary(resp_gee.in)

________________________________________________________________________________

## Let fit the GEE model with exchangeable correlation structure:

## GEE with exchangeable matrix
resp_gee.ex <- gee(nstat ~ centre + treatment + gender + baseline+ age, 
                   data = resp, family = "binomial", 
                   id = subject, corstr = "exchangeable", 
                   scale.fix = TRUE, scale.value = 1)
summary(resp_gee.ex)
________________________________________________________________________________

## Let’s check the estimated coefficients from all three models.

summary(resp_glm)$coefficients
summary(resp_gee.in)$coefficients
summary(resp_gee.ex)$coefficients
________________________________________________________________________________

## Gaussian response

BtheB$subject <- factor(rownames(BtheB))
nobs <- nrow(BtheB)
BtheB_long <- reshape(BtheB, idvar = "subject", 
                      varying = c("bdi.2m", "bdi.3m", "bdi.5m", "bdi.8m"), 
                      direction = "long")
BtheB_long$time <- rep(c(2, 3, 5, 8), rep(nobs, 4))
osub <- order(as.integer(BtheB_long$subject))
BtheB_long <- BtheB_long[osub,]

btb_gee.ind <- gee(bdi ~ bdi.pre + treatment + length + drug, 
                   data = BtheB_long, id = subject, 
                   family = gaussian, corstr = "independence")
summary(btb_gee.ind)

## With exchangeable correlation matrix:

btb_gee.ex <- gee(bdi ~ bdi.pre + treatment + length + drug,
                  data = BtheB_long, id = subject, 
                  family = gaussian, corstr = "exchangeable")

summary(btb_gee.ex)
________________________________________________________________________________

## Compare with Random Effects

## Generalized mixed effect model model
library("lme4")
resp_lmer <- glmer(nstat ~ baseline + month + treatment + 
                     gender + age + centre + 
                     (1 | subject), family = binomial(), 
                   data = resp)
________________________________________________________________________________

write.csv(resp, "resp.csv")
write.csv(respiratory, "respiratory.csv")
write.csv(BtheB, "BtheB.csv")
write.csv(BtheB_long, "BtheB_long.csv")

dir()
getwd()
setwd(choose.dir()) 
getwd()













