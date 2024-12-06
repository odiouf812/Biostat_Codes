Generalized Estimating Equations 

Generalized estimating equations, or GEE, is a method for modeling longitudinal 
or clustered data. It is usually used with non-normal data such as binary or 
count data.The name refers to a set of equations that are solved to obtain parameter
estimates (i.e., model coefficients). If interested, see Agresti (2002) for 
the computational details. In this article we simply aim to get you started with 
implementing and interpreting GEE using the R statistical computing environment.
________________________________________________________________________________

URL <- "http://static.lib.virginia.edu/statlab/materials/data/depression.csv"
dat <- read.csv(URL, stringsAsFactors = TRUE)
dat$id <- factor(dat$id)
dat$drug <- relevel(dat$drug, ref = "standard")
head(dat, n = 3)

length(unique(dat$id))
________________________________________________________________________________

library(magrittr)
with(dat, tapply(depression, list(diagnose, drug, time), mean)) %>% 
  ftable() %>% 
  round(2)

________________________________________________________________________________

# install.packages("gee")
library(gee) # version 4.13-25
dep_gee <- gee(depression ~ diagnose + drug*time,
               data = dat, 
               id = id, 
               family = binomial,
               corstr = "independence")

summary(dep_gee)

dep_gee2 <- gee(depression ~ diagnose + drug*time,
                data = dat, 
                id = id, 
                family = binomial,
                corstr = "exchangeable")
summary(dep_gee2)

dep_gee3 <- gee(depression ~ diagnose + drug*time,
                data = dat, 
                id = id, 
                family = binomial,
                corstr = "AR-M", Mv = 1)

dep_gee3$working.correlation



