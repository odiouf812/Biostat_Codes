### Generalized Linear Models in R
library(faraway)

## GLM families

GLMs are useful when the range of your response variable is constrained and/or 
the variance is not constant or normally distributed.GLM models transform the 
response variable to allow the fit to be done by least squares.The transformation 
done on the response variable is defined by the link function. This transformation 
of the response may constrain the range of the response variable. 
The variance function specifies the relationship of the variance to the mean. 
In R, a family specifies the variance and link functions which are used in the model fit. 
As an example the “poisson” family uses the “log” link function and “μ” as 
the variance function. A GLM model is defined by both the formula and the family.

## Binary response variable (Logistic)

hsb <- read.csv(file.choose(), header = TRUE)

str(hsb)
________________________________________________________________________________
g1 <- glm(I(prog == "academic") ~ gender + race + ses + schtyp + read + write + science + socst,
          family = binomial(), 
          data = hsb)

summary(g1)
________________________________________________________________________________
# Instead, we will use step() with the criteria being the LRT to reduce 
# unneeded variables from the model.

step(g1, test="LRT")

g2 <- glm(I(prog == "academic") ~ ses + schtyp + read + write + science + socst, 
          family = "binomial", 
          data = hsb)

summary(g2)

________________________________________________________________________________
### Count response variable

data(discoveries)
disc <- data.frame(count = as.numeric(discoveries),
                   year = seq(0, (length(discoveries) - 1)))
yearSqr <- disc$year^2

p1 <- glm(count ~ year + yearSqr, 
          family = "poisson",
          data = disc)

summary(p1)

### We can check the goodness of fit of this model. 
### We will use the deviance of the residuals for this test.

1 - pchisq(deviance(p1), df.residual(p1))

## The presence of overdispersion suggested the use of the 
## F-test for nested models. We will test if the squared term 
## can be dropped from the model.

drop1(p1, test = "F")
________________________________________________________________________________
## Quasi-Poisson model

# The invention count model from above needs to be fit using the quasi-Poisson family, 
# which will account for the greater variance in the data.

p2 <- glm(count ~ year + yearSqr, 
          family = "quasipoisson",
          data = disc)
summary(p2)


