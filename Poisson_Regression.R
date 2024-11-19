require(ggplot2)
require(sandwich)
require(msm)
update.packages()
mydata <- read.csv(file.choose(), header = TRUE)
mydata <- within(mydata, {
  prog <- factor(prog, levels=1:3, labels=c("General", "Academic", 
                                            "Vocational"))
  id <- factor(id)
})
### We can use the tapply function to display the summary statistics by program type. The table below shows the average numbers of awards by program type and seems to suggest that program type is a good candidate for predicting the number of awards, our outcome variable, because the mean value of the outcome appears to vary by prog. Additionally, the means and variances within each level of prog–the conditional means and variances–are similar. A conditional histogram separated out by program type is plotted to show the distribution.
with(mydata, tapply(num_awards, prog, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))
install.packages("ggplot2")
library(ggplot2)
ggplot(mydata, aes(num_awards, fill = prog)) +
  geom_histogram(binwidth=.5, position="dodge")
### Poisson regression : At this point, we are ready to perform our Poisson model analysis using the glm function. We fit the model and store it in the object m1 and get a summary of the model at the same time.
summary(m1 <- glm(num_awards ~ prog + math, family="poisson", data=mydata))
