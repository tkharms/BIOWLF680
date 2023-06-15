### Lab 4: ANOVA ###

library(here)
library(tidyverse)
library(car)
library(MASS)

#generate data
#Poisson distribution (count data), four groups
g0_10 <- rpois(10,2)
g25_50 <- rpois(10,4)
g50_75 <- rpois(10,8)
g75 <- rpois(10,15)
seedlings <- c(g0_10,g25_50,g50_75,g75)
burnhistory <- c(rep("y0-10",10), rep("y25-50",10), rep("y50-75", 10), rep("y75", 10))

Lab4 <- data.frame(burnhistory, seedlings)
write.csv(Lab4, here("data", "Lab4data.csv"))

#plot data
boxplot(seedlings ~ burnhistory, data = Lab4)

#ANOVA
mod <- lm(seedlings ~ burnhistory, data = Lab4)
summary(mod)
anova(mod)
plot(mod)

boxcox(seedlings + 0.001 ~ burnhistory, data = Lab4)

#square-root transform
mod2 <- lm(sqrt(seedlings+0.5) ~ burnhistory, data = Lab4)
summary(mod2)
anova(mod2)
plot(mod2)

#check variances
leveneTest(sqrt(seedlings+0.5) ~ burnhistory, data = Lab4)
leveneTest(seedlings ~ burnhistory, data = Lab4)

#posthoc tests
mod2.aov <- aov(sqrt(seedlings+0.5) ~ burnhistory, data = Lab4)
TukeyHSD(mod2.aov)

pairwise.t.test(sqrt(Lab4$seedlings+0.5), Lab4$burnhistory, p.adjust="bonferroni")

#Power analysis
#at 0.8 power
#2 samples needed per group