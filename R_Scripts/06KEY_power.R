### Lab 6: Power ###

library(here)
library(tidyverse)
library(pwr)
library(pwr2)
library(MESS)

### Power and t-tests ###
# Data from Lab 3
Pilot1 <- read.csv(here("data", "Lab03_Pilot1.csv"))
names(Pilot1) <- c("pool", "treatment", "biomass")

# remove space from "no fish"
Pilot1 <- Pilot1 %>% mutate(treatment = ifelse(treatment == "no fish", "no_fish", "fish"))

Pilot2 <- read.csv(here("data", "Lab03_Pilot2.csv"))
names(Pilot2) <- c("pool", "biomass", "treatment")

# remove space from "no fish"
Pilot2 <- Pilot2 %>% mutate(treatment = ifelse(treatment == "no fish", "no_fish", "fish"))

## Pilot 1 ##
ggplot(Pilot1, aes(x = treatment, y = biomass)) +
  geom_boxplot()
# variances are grossly unequal

t.test(Pilot1$biomass ~ Pilot1$treatment, var.equal = FALSE)

## Power of performed 2-sample t-test, unequal variances
P1.delta <- Pilot1 %>% group_by(treatment) %>% 
                       summarise(mn = mean(biomass)) %>%
                       pivot_wider(names_from = treatment, values_from = mn) %>%
                       summarise(diff = fish - no_fish) %>%
                       pull(diff)

P1.n <- Pilot1 %>% group_by(treatment) %>% 
                   summarise(n = n()) %>%
                   filter(treatment == "fish") %>%
                   pull(n)

P1.sd <- Pilot1 %>% group_by(treatment) %>% 
                    summarise(std = sd(biomass)) %>%
                    filter(treatment == "no_fish") %>%
                    pull(std)
  
P1.sd.ratio <- Pilot1 %>% group_by(treatment) %>% 
                          summarise(std = sd(biomass)) %>%
                          pivot_wider(names_from = treatment, values_from = std) %>%
                          summarise(sd.ratio = fish/no_fish) %>%
                          pull(sd.ratio)

# power_t_test function from the MESS package, allows unequal variances between groups
power_t_test(n = P1.n, delta = P1.delta, sd = P1.sd, sig.level = 0.05, sd.ratio = P1.sd.ratio, type = "two.sample", alternative = "two.sided")

## Sample size needed to detect a significant difference, at 80% power
power_t_test(power = 0.8, delta = P1.delta, sd = P1.sd, sig.level = 0.05, sd.ratio = P1.sd.ratio, type = "two.sample", alternative = "two.sided")

# Minimum detectable difference
power_t_test(power = 0.8, n = P1.n, sd = P1.sd, sig.level = 0.05, sd.ratio = P1.sd.ratio, type = "two.sample", alternative = "two.sided")

## Pilot 2: paired t ##
# Power of performed t-test
P2.delta <- Pilot2 %>% pivot_wider(names_from = treatment, values_from = biomass) %>%
                       summarise(diff = mean(fish - no_fish)) %>%
                       pull(diff)

P2.n <- Pilot2 %>% group_by(treatment) %>% 
  summarise(n = n()) %>%
  filter(treatment == "fish") %>%
  pull(n)

P2.sd <- Pilot2 %>% pivot_wider(names_from = treatment, values_from = biomass) %>%
                    summarise(std = sd(fish - no_fish)) %>%
                    pull(std)

power.t.test(n = P2.n, delta = P2.delta, sd = P2.sd, sig.level = 0.05, type = "paired", alternative = "two.sided")

## Sample size needed to detect a significant difference, at 80% power
power.t.test(power = 0.8, delta = P2.delta, sd = P2.sd, sig.level = 0.05, type = "paired", alternative = "two.sided")

# Minimum detectable difference
power.t.test(power = 0.8, n = P2.n, sd = P2.sd, sig.level = 0.05, type = "paired", alternative = "two.sided")

### Power and ANOVA ###
## Effect size in ANOVA power calculation:
# d = difference between means / pooled SD
# f = 0.5*d (for two groups)
# partial eta-squared = SS-treatment / (SS-treatment + SS-residuals)

# Data from Lab 4
Lab4dat <- read.csv(here("data", "Lab4data.csv"))

# Rename categories to remove -
Lab4dat <- Lab4dat %>% mutate(burnhistory = ifelse(burnhistory == "y0-10", "y0_10",
                                                    ifelse(burnhistory == "y25-50", "y25_50",
                                                           ifelse(burnhistory == "y50-75", "y50_75", "y75"))))

## Power of performed ANOVA
# Number of groups
L4.k <- as.numeric(length(unique(Lab4$burnhistory)))
 
# Sample size (per group)
L4.n <- Lab4dat %>% group_by(burnhistory) %>%
                    summarise(n = n()) %>%
                    filter(burnhistory == "y0_10") %>%
                    pull(n)

# Difference between two most different groups
L4.lgdiff <- Lab4dat %>% group_by(burnhistory) %>%
                      summarise(mn = mean(seedlings)) %>%
                      mutate(lg.diff = max(mn) - min(mn)) %>%
                      summarise(lg.diff = mean(lg.diff)) %>%
                      pull(lg.diff)

# Effect size
L4.sd <- Lab4dat %>% group_by(burnhistory) %>%
                  summarise(across(seedlings, list(mn = mean, stdev = sd))) %>%
                  filter(seedlings_mn == max(seedlings_mn) | seedlings_mn == min(seedlings_mn)) %>%
                  dplyr::select(-seedlings_mn) %>%
                  pivot_wider(names_from = burnhistory, values_from = seedlings_stdev) %>%
                  mutate(sdpool = sqrt((y0_10^2 + y75^2)/2)) %>%
                  pull(sdpool)
  
L4.f <- 0.5*L4.lgdiff/L4.sd

pwr.anova.test(k = L4.k, f = L4.f, n = L4.n, sig.level = 0.05)

## Sample size needed to detect a significant difference
pwr.anova.test(k = L4.k, f = L4.f, sig.level = 0.05, power = 0.8)

## Minimum effect size discernable between two most different groups
minf <- pwr.anova.test(k = L4.k, n = L4.n, sig.level = 0.05, power = 0.8)

## Convert effect size to difference between means, given observed pooled SD
# Cohen's f = 0.5*standardized difference
d.std <- 2*minf$f

# Standardized difference, d, = diff/SDpooled 
# Minimum detectable difference between two most different groups
diff <- d.std*L4.sd

## Average effect size
# Convert Cohen's f to partial eta squared, and then to difference between means
# f = sqrt(p.eta.sq/(1-p.eta.sq))
# minf$f = 0.551, p.eta.sq = 0.235 approximately satisfies this relationship
# Interpretation: To detect a significant effect under the n and k of this experiment, the main effect of burnhistory must explain at least 23.5% of the variance in the response (seedling count)

## Variance in seedling count explained by burnhistory in Lab4 data
# Effect sizes
Lab4.aov <- aov(seedlings ~ burnhistory, data = Lab4dat)
Lab4.aovtab <- anova(Lab4.aov)

# p.eta^2 = SSeffect/(SSeffect + SSerror)
SS.burn <- Lab4.aovtab$"Sum Sq"[1]
SS.error <- Lab4.aovtab$"Sum Sq"[2]

p.eta <- SS.burn/(SS.burn + SS.error)
# In the Lab4 dataset, burnhistory explained 71% of the variance in seedling count

# f = sqrt(p.eta^2/(1 - p.eta^2))
f.burn <- sqrt(p.eta/(1 - p.eta))

# Convert effect size to difference between means, given observed pooled SD
# Cohen's f = 0.5*standardized difference
deta.std <- 2*f.burn

# Standardized difference, d, = diff/SDpooled 
# Average difference between groups in Lab 4 data:
diff.eta <- deta.std*L4.sd
