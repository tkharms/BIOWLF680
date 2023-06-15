setwd("~/Documents/Teaching/Data Analysis/2018/Labs/Lab 12 time")

library(lattice)
library(nlme)
library(tidyverse)
library(AICcmodavg)
library(here)

spiller <- read.csv(here::here("data", "spiller.csv"))
names(spiller) <- c("index", "spiders", "treat", "plot", "block", "month")
spiller <- spiller[complete.cases(spiller$spiders),]
spiller$blocka <- chartr("123456789", "ABCDEFGHI", spiller$block)
spiller <- select(spiller, -(block))
spiller$plot <- as.factor(spiller$plot)
write.csv(spiller, "spiller.csv")

spillerdat <- read.csv("spillerdat.csv")
nDate <- as.Date(spiller$date, "%m/%d/%Y")
spillerdat <- cbind(spiller, nDate)

spiller <- spiller %>% select(-c(X, index))
names(spiller)[names(spiller) == 'blocka'] <- 'block'

write.csv(spiller, here::here("data", "spiller.csv"))

spiller <- read.csv(here("data", "Lab13dat.csv"))

### Plots ###
# this is lattice plot
xyplot(spiders~month|treat*as.factor(block), data=spiller, type="o")

spiller %>% ggplot(aes(x = month, y = spiders, color = treat)) +
             geom_point() +
             geom_line() +
             facet_wrap(~block)

spiller %>% ggplot(aes(x = month, y = spiders, color = block)) +
  geom_point() +
  geom_line() +
  facet_wrap(~treat)

### Models ###
## models w/o block
mod.noblk <- gls(spiders~month*treat, na.action=na.omit, data=spiller)
mod.noblk.sr <- gls(sqrt(spiders)~month*treat, na.action=na.omit, data=spiller)

plot(mod.noblk)
plot(mod.noblk.sr)
qqnorm(resid(mod.noblk.sr))

## random effects
mod.nocorr.sr <- lme(sqrt(spiders)~month*treat, random =~ 1|block, na.action=na.omit, data=spiller)
mod.nocorr2.sr<-lme(sqrt(spiders)~month*treat, random =~ 1|block/plot, na.action=na.omit, data=spiller)

mod.nocorr3.sr<-lme(sqrt(spiders)~month*treat, random =~ 1|block/treat, na.action=na.omit, data=spiller)

plot(mod.nocorr.sr)
plot(mod.nocorr2.sr)

## Temporal autocorrelation
mod.cs <- lme(sqrt(spiders) ~ month*treat, random =~ 1|block/plot, correlation=corCompSymm(form=~month|block/plot), na.action=na.omit, data=spiller)

mod.ar1 <- lme(sqrt(spiders) ~ month*treat, random =~ 1|block/plot, correlation=corAR1(form=~month|block/plot), na.action=na.omit, data=spiller)

mod.arma11 <- lme(sqrt(spiders) ~ month*treat, random =~ 1|block/plot, correlation=corARMA(form=~month|block/plot, c(0.1, 0.1), p=1, q=1), na.action=na.omit, data=spiller)

mod.arma11.fix <- lme(sqrt(spiders) ~ month*treat, random =~ 1|block/plot, correlation=corARMA(form=~month, c(0.1, 0.1), p=1, q=1), na.action=na.omit, data=spiller)

mod.arma21 <- lme(sqrt(spiders) ~ month*treat, random =~ 1|block/plot, correlation=corARMA(form=~month|block/plot, c(0.1,0.5, 0.5), p=2, q=1), na.action=na.omit, data=spiller)

mod.arma31 <- lme(sqrt(spiders) ~ month*treat, random =~ 1|block/plot, correlation=corARMA(form=~month|block/plot, c(0.1,0.1, 0.5, 0.2), p=3, q=1), na.action=na.omit, data=spiller)

mod.arma12 <- lme(sqrt(spiders) ~ month*treat, random =~ 1|block/plot, correlation=corARMA(form=~month|block/plot, c(0.1,0.5, 0.5), p=1, q=2), na.action=na.omit, data=spiller)

mod.arma20 <- lme(sqrt(spiders) ~ month*treat, random =~ 1|block/plot, correlation=corARMA(form=~month|block/plot, c(0.1,0.5), p=2, q=0), na.action=na.omit, data=spiller)

mod.arma30 <- lme(sqrt(spiders) ~ month*treat, random =~ 1|block/plot, correlation=corARMA(form=~month|block/plot, c(0.1,0.5, 0.2), p=3, q=0), na.action=na.omit, data=spiller)

mod.arma22 <- lme(sqrt(spiders) ~ month*treat, random =~ 1|block/plot, correlation=corARMA(form=~month|block/plot, c(0.1, -0.5, -0.1, 0.1), p=2, q=2), na.action=na.omit, data=spiller)

acf(resid(mod.cs, type = "normalized"))
acf(resid(mod.ar1, type = "normalized"))
acf(resid(mod.arma11, type = "normalized"))
acf(resid(mod.arma21, type = "normalized"))
acf(resid(mod.arma31, type = "normalized"))
acf(resid(mod.arma12, type = "normalized"))
acf(resid(mod.arma20, type = "normalized"))
acf(resid(mod.arma30, type = "normalized"))
acf(resid(mod.arma22, type = "normalized"))

pacf(resid(mod.arma21, type = "normalized"))
AIC(mod.arma21, mod.arma20)

## residual plots
res.arma21 <- resid(mod.arma21, type="normalized")
plot(mod.arma21)
plot(res.arma21~spiller$month)
qqnorm(res.arma21)
qqline(res.arma21)
qqnorm(mod.arma21)
acf(res.arma21)
pacf(res.arma21)

acf(res.arma21)
lines(c(1:35),.3^(c(1:35)),col="red")

summary(mod.arma21)

## inferences on parameters
intervals(mod.arma21)

spiller$treata <- C(factor(spiller$treat), contr.treatment, base = 3)

mod.arma21a <- lme(sqrt(spiders) ~ month*treata, random =~ 1|block/plot, correlation=corARMA(form=~month|block/plot, c(0.1,0.5, 0.5), p=2, q=1), na.action=na.omit, data=spiller)
summary(mod.arma21a)

### figure of final model here ###

res.ar1 <- resid(mod.ar1, type="normalized")
plot(resid(mod.ar1, type="normalized"))
plot(res.ar1~spillerdat$nDate)
qqnorm(res.ar1)
qqline(res.ar1)
qqnorm(mod.ar1)
acf(res.ar1)
pacf(res.ar1)

acf(res.ar1)
lines(c(1:35),.195^(c(1:35)),col="red")

res.arma11 <- resid(mod.arma11, type="normalized")
plot(resid(mod.arma11, type="normalized"))
plot(res.arma11~spillerdat$nDate)
qqnorm(res.arma11)
qqline(res.arma11)
acf(res.arma11)
pacf(res.arma11)

res.arma21<-resid(mod.arma21, type="normalized")
plot(resid(mod.arma21, type="normalized"))
plot(res.arma21~spillerdat$nDate)
qqnorm(res.arma21)
qqline(res.arma21)
acf(res.arma21)
pacf(res.arma21)

