### Lab 6: Power ###

library(here)
library(tidyverse)

### Power and t-tests ###

## 2-sample t-test
# Data from Lecture 4
fox <- read.csv(here("data", "Lect4_foxdata.csv"))

t.test(fox$no.fox, fox$fox, var.equal = TRUE)

## Arguments to power.t.test function:
# n = within group sample size (or a vector if unequal across groups)
# delta = difference between groups
# sd = pooled standard deviation; square root of the average of the two standard deviations squared
# sig.level = alpha
# power = power (1 - Type II error rate)

# Power of performed t-test
sdt <- sqrt(mean(sd(fox$fox)^2, sd(fox$no.fox)^2))

power.t.test(n = 25, delta = 8.345515, sd = sdt, sig.level = 0.05, type = "two.sample", alternative = "two.sided")

# Sample size needed to detect a significant difference
power.t.test(delta = 8.3, sd = sdt, sig.level = 0.05, power = 0.8, type = "two.sample", alternative = "two.sided")

# Minimum detectable difference
power.t.test(n=25, sd = sdt, sig.level = 0.05, power = 0.8, type = "two.sample", alternative = "two.sided")

## paired t-test
power.t.test(n = 25, delta = 8.345515, sd = sdt, sig.level = 0.05, type = "paired", alternative = "two.sided")

### Power and ANOVA ###
# Data from Lecture 5
dat.aov <- read.csv(here("data", "Lect07_data.csv"), header = TRUE)

## Effect size in ANOVA power calculation:
# d = difference between means / pooled SD
# f = 0.5*d (for two groups)
# partial eta-squared = SS-treatment / (SS-treatment + SS-residuals)
