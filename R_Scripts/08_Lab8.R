### Lab 8: Mixed effects models & multimodel inference ###

library(nlme)
library(lme4)
library(here)
library(tidyverse)
library(AICcmodavg)
library(MuMIn)

### Data from Zuur ###
RIKZ <- read.delim(here("data", "Lect14_data.txt"), head = TRUE)

#plot data with grouping structure
NAP.pl <- RIKZ %>% ggplot(aes(x = NAP, y = Richness)) +
                   geom_point() +
                   facet_wrap(~as.factor(Beach), nrow = 2)

#convert Beach to a factor
RIKZ$fBeach <- as.factor(RIKZ$Beach)

### Fit multilevel models ###
## Group-level intercept ##
# Using lme4
randint <- lmer(Richness ~ NAP + (1|fBeach), data = RIKZ)

# Check assumptions
plot(randint)
# This looks scary

qqnorm(resid(randint))
qqline(resid(randint))
# long tail

summary(randint)

# Bootstrap confidence intervals
confint(randint, method = "boot") 

# Check assumptions on random effects
qqnorm(ranef(randint)$fBeach[[1]])
plot(ranef(randint)$fBeach[[1]])
#not great

## plot residuals by  group; allows visualization of variance with covariate and groups simultaneously
coplot(resid(randint) ~ NAP|fBeach, data=RIKZ)

## Correlated group-level slopes and intercepts ##
randintsl <- lmer(Richness ~ NAP + (1 + NAP|fBeach), data = RIKZ)
# Issues with convergence here. Let's try another optimizer

randintsl <- lmer(Richness ~ NAP + (1 + NAP|fBeach), data = RIKZ, 
                  control=lmerControl(optimizer="bobyqa"))

# Check assumptions
plot(randintsl)
# This looks scary

qqnorm(resid(randintsl))
qqline(resid(randintsl))
# long tails

summary(randintsl)

# Check assumptions on random effects
qqnorm(ranef(randintsl)$fBeach[[1]])
plot(ranef(randintsl)$fBeach[[1]])
#not great

## Uncorrelated group-level slopes and intercepts ##
rand.uncorr <- lmer(Richness ~ NAP + (1|fBeach) + (0 + NAP|fBeach), data = RIKZ)

# Check assumptions
plot(rand.uncorr)
# This looks scary

qqnorm(resid(rand.uncorr))
qqline(resid(rand.uncorr))
# long tails

summary(rand.uncorr)

# Check assumptions on random effects
qqnorm(ranef(rand.uncorr)$fBeach[[1]])
plot(ranef(rand.uncorr)$fBeach[[1]])
#not great
       
### Compare models ### 
# AIC table from AICcmodavg
aictab(list(randint = randint, randintsl = randintsl, rand.uncorr = rand.uncorr))
# Group-level intercept & correlated slopes and intercepts are equivalent. Let's work with the more parsimonious model (group-level intercepts)

### Estimate a pseudo-R2 for the full model and for the fixed effects only ###
# From MuMIN package
#R2m: marginal R2, variance explained by fixed factors only
#R2c: conditional R2, variance explained by fixed+random factors
r.squaredGLMM(randint)

## Comparing models with and without random intercepts
# fit gls model (no random effect) and refit all random effects models in nlme

mod.gls <- gls(Richness ~ NAP, data = RIKZ)
mod.int.nlme <- lme(Richness ~ NAP, random = ~1|fBeach, data = RIKZ)

anova(mod.gls, mod.int.nlme)

## Plot the fitted model ##
# Lots of options here. See ggeffects package

# Fixed effects
preds <- ggpredict(randint, type = "fixed")
plot(preds)

# Random effects
preds.rand <- ggpredict(randint, terms = c("NAP", "fBeach [sample=9]"), type = "random")
plot(preds.rand, add.data = TRUE)
plot(preds.rand, ci = TRUE)

# Example from lab: interaction effect
preds.rand.int <- ggpredict(mod.int,
                        terms = c("SOM", "trt [control, snowfence]"), 
                        type = "random")
plot(preds.rand.int)

### add CI on confidence interval
