### Lab 8 : Multilevel models ###

library(nlme)
library(lme4)
library(here)
library(tidyverse)
library(ggeffects)
library(MuMIn)
library(lmerTest)
library(broom.mixed)

### Check out previous Lab 8 data 
old.dat <- read.csv(here("data", "Lab08data.csv"))

# New names
names(old.dat) <- c("plot", "CO2flux", "SOM", "trt", "fence")

# Replace categorical fish data with snowfence
old.dat <- old.dat %>% mutate(trt = ifelse(trt == "absent", "control", "snowfence")) %>%
                       mutate(CO2flux = CO2flux/4) %>%
                       mutate(SOM = SOM*15) %>% 
                       mutate(CO2flux = ifelse(trt == "control" & CO2flux < 1, CO2flux + 0.5, CO2flux)) %>%
                       mutate(CO2flux = ifelse(trt == "control", CO2flux*SOM/45, CO2flux)) %>%
                       mutate(CO2flux = ifelse(trt == "snowfence" & CO2flux < 1, CO2flux + 0.5, CO2flux)) %>%
                       mutate(SOM = ifelse(trt == "control", SOM + 15, SOM - 8)) %>%
                       mutate(trt = ifelse(trt == "control", "snowfence", "control"))

##                       mutate(CO2flux = ifelse(trt == "control" & SOM > 80, CO2flux - 0.5, CO2flux)) %>%
##                       mutate(SOM = ifelse(trt == "control" & SOM > 80, SOM - 10, SOM)) %>%
##                       mutate(CO2flux = ifelse(CO2flux < 1, CO2flux + 0.5, CO2flux)) %>%
##                       mutate(CO2flux = ifelse(SOM < 55, CO2flux + 0.25, CO2flux)) %>%
##                       mutate(CO2flux = ifelse(trt == "snowfence" & CO2flux > 1.75 & SOM < 70, CO2flux - 0.75, CO2flux)) 
                       
old.pl <- old.dat %>% ggplot(aes(x = SOM, y = CO2flux)) +
                      geom_point(aes(color = trt)) +
                      facet_wrap(~as.factor(fence))

write.csv(old.dat, here("data", "Lab08dat.csv"), row.names = FALSE)

old.dat$fence <- as.factor(old.dat$fence)

mod.int <- lmer(CO2flux ~ SOM*trt + (1|fence), data = old.dat)
summary(mod.int)
plot(mod.int)
qqnorm(resid(mod.int, type = "pearson"))
qqline(resid(mod.int, type = "pearson"))

mod.sl <- lmer(CO2flux ~ SOM*trt + (1|fence) + (0 + SOM|fence), data = old.dat)
mod.sl <- lmer(CO2flux ~ SOM*trt + (1|fence) + (0 + SOM|fence), data = old.dat, control = lmerControl(optimizer ="Nelder_Mead"))
mod.sl.corr <- lmer(CO2flux ~ SOM*trt + (SOM|fence), data = old.dat)

summary(mod.sl)
plot(mod.int)
qqnorm(resid(mod.int, type = "pearson"))
qqline(resid(mod.int, type = "pearson"))

mod.sl.all <- allFit(mod.sl)

# correlated random slope & intercept
mod.sl.corr <- lme(CO2flux ~ SOM*trt, random =~SOM|fence, data = old.dat)

# uncorrelated random slope & intercept
mod.sl.uncorr <- lme(CO2flux ~ SOM*trt, random = list(fence = pdDiag(~SOM)), data = old.dat)

### Confidence intervals
# Likelihood profile confidence intervals  
confint(mod.int)  

# Bootstrap confidence intervals
confint(mod.int, method = "boot")  

# coef from lmerTest
coef(summary(as(mod.int,"lmerModLmerTest")))

## R2
r2.mod.int <- r.squaredGLMM(mod.int)

## Plot the fitted model ##
# Lots of options here. See ggeffects package
preds <- ggpredict(mod.int, terms = c("SOM", "trt"), type = "fixed")
pl.mod.int <- plot(preds, add.data = TRUE) +
              annotate("text", x = 95, y = 0.25, size = 6, label = paste("R[m]^{2}~'='~", round(r2.mod.int[1], 2)), parse = TRUE) +
              annotate("text", x = 95, y = 0, size = 6, label = paste("R[m]^{2}~'='~", round(r2.mod.int[2], 2)), parse = TRUE) +
              ylab(expression(C*O[2]~"flux (mg"~m^-2*"*"*d^-1*")")) +
              xlab("soil organic matter (%)") +
              theme_bw() +
              theme(plot.title = element_blank(),
                    legend.title = element_blank(),
                    axis.text = element_text(size = 20),
                    axis.title = element_text(size = 20),
                    legend.position = c(0.15, 0.9),
                    legend.text = element_text(size = 16),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank())

ggsave(pl.mod.int, file = here("plots", "Lab8.pdf"), width = 8.5, height = 8, units = "in")

preds.rand.SOM <- ggpredict(mod.int, terms = c("SOM", "fence [sample=5]"), type = "random")
plot(preds.rand.SOM, add.data = TRUE)
plot(preds.rand.SOM, ci = TRUE)

preds.rand.trt <- ggpredict(mod.int, terms = c("trt", "fence [sample=5]"), type = "random")
plot(preds.rand.trt, ci = TRUE)

preds.rand <- ggpredict(mod.int,
                terms = c("SOM", "trt [control, snowfence]"), 
                type = "random")
plot(preds.rand)
