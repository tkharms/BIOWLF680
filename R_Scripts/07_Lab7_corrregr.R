### Correlation & simple linear regression ###

library(here)
library(tidyverse)
library(Hmisc)
library(psych)
library(car)
library(boot)

## Correlation data
corr.dat <- read.csv(here("data", "Lect10_data.csv"))
corr.dat <- corr.dat %>% select(x1:x3)

### Scatterplot matrices ###
## Simple scatterplot matrix
pairs(~x1 + x2 + x3, data = corr.dat)

## Fancier scatterplot matrix from psych package
pairs.panels(corr.dat, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

### Correlations ###
## Pearson correlation coefficient of a single pair of variables
# Pearson is default 
cor(corr.dat$x1, corr.dat$x2)

# Spearman rank correlation coefficient
cor(corr.dat$x1, corr.dat$x2, method = "spearman")

# Kendal correlation
cor(corr.dat$x1, corr.dat$x2, method = "kendall")

## Correlation table
# Pearson is default
cor(corr.dat)

## Significance of correlation coefficient 
# on a single pair of variables
# Pearson is default
cor.test(corr.dat$x1, corr.dat$x2)

# Spearman
cor.test(corr.dat$x1, corr.dat$x2, method = "spearman")

## Correlation table from Hmisc
rcorr(as.matrix(corr.dat), type = "pearson")

### Regression ###
mod1 <- lm(x1 ~ x2, data = corr.dat)

plot(mod1)

summary(mod1)
anova(mod1)

## Plot confidence band around regression line
ggplot(corr.dat, aes(x = x1, y = x2)) +
  geom_point() +
  geom_smooth(method = lm)

### Bootstrap regression coefficients ###
# Bootstrap function from the car package = Boot. This is a simplified wrapper for the boot function we have used previously.
coef.boot <- Boot(mod1, R = 10000, method = "case")
summary(coef.boot)

# Bootstrap confidence intervals on regression coefficients, using boot.ci function from boot package
boot.ci(coef.boot, index=2)
               