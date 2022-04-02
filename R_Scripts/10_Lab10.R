### Lecture 16: Logistic regression ###

library(here)
library(arm)

## Read in data ##
Boar <- read.delim(here::here("data", "Lect16dat.txt"))
names(Boar)<-c("TB", "sex", "age", "lengthCT")

## Plot raw data
boxplot(Boar$lengthCT ~ Boar$TB, xlab="TB", ylab="length (cm)")
plot(Boar$TB ~ Boar$lengthCT, ylab="TB", xlab="length (cm)")

#logistic model
mod.log <- glm(TB ~ lengthCT, family = binomial("logit"), data = Boar, na.action = na.omit)

## Confidence intervals
# Likelihood profile method
# Note these are log odds
confint(mod.log)

# Odds (model coefficients & CI)
exp(cbind(OR = coef(mod.log), confint(mod.log)))
# Interpretation: For 1 unit increase in body length, the odds of TB infection increase by a factor of 1.03

## Probabilities ##
## Probability of infection at mean length
# Note coef[1] = intercept, coef[2] = effect of length
# invlogit function from arm package: function(x) {1/(1+exp(-x))}
# invlogit is equivalent to plogis
invlogit(coef(mod.log)[1] + coef(mod.log)[2]*mean(Boar$lengthCT, na.rm = TRUE))
plogis(coef(mod.log)[1] + coef(mod.log)[2]*mean(Boar$lengthCT, na.rm = TRUE))

## Evaluate difference in probability of TB given a one unit change in length, near the mean length. 
# Remember that the logistic fit is not linear in terms of probabilities, so it matters where we evaluate the fitted function. Here, we choose to evaluate near the mean of the predictor- this is where change in probability of infection is steepest.
# Probability of infection at mean(length)
p.mean <- plogis(coef(mod.log)[1] + coef(mod.log)[2]*mean(Boar$lengthCT, na.rm = TRUE))

# Probability of infection at mean(length) + 1
p.mean1 <- plogis(coef(mod.log)[1] + coef(mod.log)[2]*(mean(Boar$lengthCT, na.rm = TRUE) + 1))

# Difference
p.mean1 - p.mean

## "Divide by 4 rule ##
# slope/4 is maximum difference in Pr(y=1) for a 1 unit change in x
coef(mod.log)[2]/4
# Estimate is similar to the difference method. For a 1 unit change in x, probability of TB infection increases by no more than 0.8%

## Pseudo R2 ##
# One of many flavors of pseudo R2 metrics
with(summary(mod.log), 1 - deviance/null.deviance)


### Plots ###
## Binned residual plot
#binnedplot function from arm package
x <- predict(mod.log)
y <- resid(mod.log)
binnedplot(x,y)

## Fitted model with CI
# These are Wald-type CIs (large sample approximation)
log.pl <- Boar %>% ggplot(aes(x = lengthCT, y = TB)) + 
  geom_point(alpha = 0.5) +
  stat_smooth(formula = y ~ x, method="glm", se = TRUE, method.args = list(family=binomial))

## When you have multiple continuous predictors, hold one (or all but one) at its mean and evaluate the remaining predictor

# For purposes of demonstration only:
# generate another continuous variable 
Boar <- Boar %>% mutate(varX = rnorm(nrow(Boar), 5, 1))

mod.demo <- glm(TB ~ lengthCT + varX, family = binomial("logit"), data = Boar, na.action = na.omit)

# generate values at which to evaluate the model
# Holding varX constant here
newvalues <- with(Boar, data.frame(lengthCT = seq(from = min(lengthCT, na.rm = TRUE), to = max(lengthCT, na.rm = TRUE), length.out = nrow(Boar)), varX = mean(varX)))

predictions <- cbind(newvalues, predict(mod.demo, newdata = newvalues, type = "link", se = TRUE))

# Using normal approximation of confidence intervals here
# plogis is inverse logit
predictions <- within(predictions, {
  PredictedProbs <- plogis(fit)
  loCI <- plogis(fit - (1.96 * se.fit))
  hiCI <- plogis(fit + (1.96 * se.fit))
})

pl <- ggplot(predictions, aes(x = lengthCT, y = PredictedProbs)) + 
  geom_ribbon(aes(ymin = loCI, ymax = hiCI), alpha = 0.2) + 
  geom_line(size = 1) +
  geom_point(data = Boar, aes(x = lengthCT, y = TB))
