### Lab 9 / Lect 15 example ###

library(here)
library(nlme)
library(tidyverse)

### Squid data ###
squid <- read.delim(here("data", "Lect15dat.txt"))

## Convert to numeric, remove last row
squid <- squid %>% mutate(fMonth = as.factor(MONTH)) %>%
                   mutate(weight = as.numeric(Testisweight)) %>%
                   mutate(DML = as.numeric(DML)) %>%
                   filter(weight > 1)

squid.pl <- squid %>% ggplot(aes(x = DML, y = weight)) + 
                            geom_point(aes(color = fMonth))

### syntax for random effects using nlme ###
## Just for demo, let's treat month as a random effect
squid.lme <- lme(weight ~DML, (random=~1|fMonth), data = squid)

### gls model ###
squid.gls <- gls(weight ~ DML*fMonth, data = squid)

## Model heterogeneous variance using nlme ##
# See syntax for other variance structures available in nlme in lecture notes
# Linear increase in variance with predictor
varfix <- varFixed(~DML)
squid.varfix <- gls(weight ~ DML*fMonth, weights=varfix, data = squid)

# use standardized (Pearson) residuals for residual plots of models with structured variance. Pearson residuals are scaled by the variance.
plot(squid.varfix) # note standardized residuals on y-axis
qqnorm(squid.varfix) # also produces standardized residuals
qqline(squid.varfix) # This won't work.

# If you want to visualize the 1:1 line on a qqnorm, specify the type of residuals
qqnorm(resid(squid.varfix, type = "pearson"))
qqline(resid(squid.varfix, type = "pearson"))

### Confidence intervals on parameters ###
# The intervals function uses a normal approximation to generate 95% confidence intervals on parameters
intervals(squid.gls)
