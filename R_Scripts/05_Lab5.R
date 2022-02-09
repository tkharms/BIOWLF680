### 2-way ANOVA ###

library(tidyverse)
library(here)
library(MASS)
library(emmeans)
library(multcomp)
library(agricolae)

### Read in data ###
dat <- read.csv(here("data", "Lect08 data.csv"), header=TRUE)

### Plot the data ###
# basic boxplot
boxplot(dat$NBI ~ dat$nitrogentreat + dat$fungitreat)

# Grouped boxplot
box.pl <- dat %>% ggplot(aes(x = nitrogentreat, y = NBI, fill = fungitreat)) +
                  geom_boxplot(position = "dodge")

# Interaction plot
interaction.plot(dat$nitrogentreat, dat$fungitreat, dat$NBI)

### ANOVA ###
mod1.aov <- aov(NBI ~ nitrogentreat*fungitreat, data = dat)
summary(mod1.aov)

# Check assumptions
plot(mod1.aov)
leveneTest(mod1.aov)
boxcox(mod1.aov)

mod1.lm <- lm(NBI ~ nitrogentreat*fungitreat, data = dat)

### Post-hoc pairwise comparisons ###
## This will print differences among levels within each main effect and all pairwise comparisons
TukeyHSD(mod1.aov)

## For pairwise comparisons only:
# Create interaction term
tw_int <- with(dat, interaction(nitrogentreat, fungitreat))

# Re-run model specifying the interaction term
mod.int <- aov(NBI ~ tw_int, data = dat)

# Call Tukey's HSD on the model that explicitly specifies the interaction term
TukeyHSD(mod.int)

## Generate letters for display on figures
# Requires emmeans and multcomp packages. 

# Interaction effect
inter.ph <- emmeans(mod.int, pairwise ~ tw_int, adjust = "tukey")
inter.ph.lett <- cld(inter.ph$contrasts, Letter = "abcdefg")

# Main effect
inter.ph <- emmeans(mod1.aov, pairwise ~ nitrogentreat, adjust = "tukey")
inter.ph.lett <- cld(inter.ph$contrasts, Letter = "abcdefg")

