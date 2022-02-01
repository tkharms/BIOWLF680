### ANOVA & multiple comparisons ###

library(here)
library(tidyverse)

## Load Lect07 data
dat.aov <- read.csv(here("data", "Lect07_data.csv"), header = TRUE)

## basic boxplot
boxplot(NPP ~ treat, data = dat.aov)

## fancier boxplot
# Order the categories manually
dat.aov$trts <- factor(dat.aov$treat, levels = c("no_invasive", "fox", "rats"), labels = c("control", "fox", "rats"))
aov.box <- dat.aov %>% ggplot(aes(x = trts, y = NPP)) +
                       geom_boxplot(size = 1) +
                       ylab(expression("NPP (g"~m^"-2"*")")) +
                       xlab("") +
                       theme_bw() +
                       theme(axis.text = element_text(size = 24),
                             axis.title = element_text(size = 24),
                             panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(),
                             panel.border = element_rect(colour = "black", fill = NA, size = 1))

ggsave(aov.box, file = "anova_box.pdf", path = here("plots"), width = 10, height = 10, units = "in")

## Check sample sizes and groups
summary(dat.aov)

## Run model
# using aov
mod.aov <- aov(NPP ~ treat, data = dat.aov)
summary(mod.aov)

# using lm
mod.lm <- lm(NPP ~ treat, data = dat.aov)
summary(mod.lm)

## Check assumptions
# Normality
qqnorm(resid(mod.lm))
qqline(resid(mod.lm))

# Variance
plot(mod.lm)

## Post-hoc comparisons
# Tukey's HSD
# Note the TukeyHSD function requires an aov object (will not accept lm)
TukeyHSD(mod.aov, "treat")

# Bonferroni
pairwise.t.test(dat.aov$NPP, dat.aov$treat, p.adjust="bonferroni")
