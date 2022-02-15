### Lab 6: Power ###

library(here)
library(tidyverse)
library(pwr)
library(pwr2)

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
## Effect size in ANOVA power calculation:
# d = difference between means / pooled SD
# f = 0.5*d (for two groups)
# partial eta-squared = SS-treatment / (SS-treatment + SS-residuals)

# Data from Lecture 5
dat.aov <- read.csv(here("data", "Lect07_data.csv"), header = TRUE)

mod.aov <- aov(NPP ~ treat, data = dat.aov)

# Power of performed test
power.anova.test(groups = 3, n = 10, between.var = 539, within.var = 675, sig.level = 0.05)

## More flexibility in using hypothesized effect sizes with the pwr library
# Sample size needed to detect a significant difference
sumdat <- dat.aov %>% group_by(treat) %>%
            summarize(across(NPP, list(mn = mean, stdev = sd)))

# Difference between two most different groups
d <- sumdat[2,2] - sumdat[3,2]
sdpool <- sqrt((sumdat[2,3]^2 + sumdat[3,3]^2)/2)
dc <- as.numeric(d/sdpool)

pwr.anova.test(k = 3, f = 0.5*dc, sig.level = 0.05, power = 0.8)

# Minimum detectable difference
pwr.anova.test(k = 3, n = 5, sig.level = 0.05, power = 0.8)

# Sample size required for significant difference with medium average effect size between all groups
f <- 0.5
pwr.anova.test(k = 3, f = 0.5, sig.level = 0.05, power = 0.8)

### Power on 2-way ANOVA ###
# pwr.2way function from the pwr2 package

# Data from lecture 
Lect8 <- read.csv(here("data", "Lect08 data.csv"), header = TRUE)

## count observations by groups
n.nit <- Lect8 %>% group_by(nitrogentreat) %>%
                   summarize(n())

n.fungi <- Lect8 %>% group_by(fungitreat) %>%
                     summarize(n())

## number of groups
k.nit <- length(unique(Lect8$nitrogentreat))
k.fun <- length(unique(Lect8$fungitreat))

## Effect sizes
mod2.aov <- aov(NBI ~ nitrogentreat*fungitreat, data = Lect8)
mod2.aovtab <- anova(mod2.aov)

# eta^2 = SSeffect/(SSeffect + SSerror)
SS.nit <- mod2.aovtab$"Sum Sq"[1]
SS.fungi <- mod2.aovtab$"Sum Sq"[2]
SS.error <- mod2.aovtab$"Sum Sq"[4]
  
nit.etasq <- SS.nit/(SS.nit + SS.error)
fun.etasq <- SS.fungi/(SS.fungi + SS.error)
  
# f = sqrt(eta^2/(1 - eta^2))
f.nit <- sqrt(nit.etasq/(1 - nit.etasq))
f.fungi <- sqrt(fun.etasq/(1 - fun.etasq))

pwr.2way(a = k.nit, b = k.fun, alpha = 0.05, size.A = 20, size.B = 20, f.A = f.nit, f.B = f.fungi)
