### Lab 9 KEY: heterogeneous variances ###

library(nlme)
library(AICcmodavg)
library(MuMIn)

# These are the old data from 2018
gfish <- read.csv(here("data", "Lab09data.csv"), header = T)

names(gfish)<-c("trawl", "year", "plank", "fish")

gfish.rev <- gfish
gfish.rev$fish <- gfish$fish + rnorm(nrow(gfish.rev), 12*gfish$plank, gfish$plank*3)

# Let's convert year to region to avoid confusion with temporal autocorrelation
gfish.rev <- gfish.rev %>% mutate(region = ifelse(year == 1995, "G1",
                                                  ifelse(year == 2000, "A2",
                                                         ifelse(year == 2005, "B6",
                                                                ifelse(year == 1992, "C3",
                                                                       ifelse(year == 2004, "D1",
                                                                              ifelse(year == 1996, "E7",
                                                                                     ifelse(year == 1990, "F2",
                                                                                            ifelse(year == 1994, "H5",
                                                                                                   ifelse(year == 1998, "I4",
                                                                                                          ifelse(year == 1999, "J6", "L3"))))))))))) %>%
  filter(region != "J6") %>%
  select(-c(year))

write.csv(gfish.rev, here("data", "Lab09data.csv"), row.names = FALSE)

plot(gfish$fish~gfish$plank)
plot(gfish.rev$fish~gfish.rev$plank)

fullmod<-lmer(fish~plank + (1 |region), data=gfish.rev)
plot(fullmod)
summary(fullmod)
qqnorm(resid(fullmod, type = "pearson"))
qqline(resid(fullmod, type = "pearson"))

fullmod.log <- lmer(log(fish) ~ plank + (1 |region), data=gfish.rev)
plot(fullmod.log)
summary(fullmod)
qqnorm(resid(fullmod.log, type = "pearson"))
qqline(resid(fullmod.log, type = "pearson"))

fullmod.sr <- lmer(sqrt(fish) ~ plank + (1 |region), data=gfish.rev)
plot(fullmod.sr)
summary(fullmod)
qqnorm(resid(fullmod.sr, type = "pearson"))
qqline(resid(fullmod.sr, type = "pearson"))

rawresid<-gfish$fish-fitted(fullmod)

pearr<-resid(fullmod, type="pearson", standardized=TRUE)
respr<-resid(fullmod, type="response")

#stand_resid<-resid(fullmod)/sqrt(88254)

plot(rawresid,pearr)
plot(pearr,respr)
plot(fitted(fullmod1), rawresid)

plot(rawresid, resid(fullmod))

varplfix<-varFixed(~plank)
mod2.vfixed<-lme(fish~plank, (random=~1|region), weights=varplfix, data=gfish.rev)
plot(mod2.vfixed)
rawresid<-gfish$fish-fitted(mod2.vfixed)

pearr<-resid(mod2.vfixed, type="pearson", standardized=TRUE)
respr<-resid(mod2.vfixed, type="response")
plot(rawresid,pearr)
plot(pearr,respr)
plot(fitted(mod2.vfixed), rawresid)

varyear<-varIdent(form=~1|region)
mod3.vIdent<-lme(fish~plank, (random=~1|region), weights=varyear, data=gfish.rev)
plot(mod3.vIdent)

varPow<-varPower(form=~plank)
mod4.vp<-lme(fish~plank, (random=~1|region), weights=varPow, data=gfish.rev)
plot(mod4.vp)
qqnorm(mod4.vp)
qqline(mod4.vp)

cont<-lmeControl(maxIter = 500, opt = "optim")
varPowy<-varPower(form=~plank|region)
mod5.vpy.reg <-lme(fish~plank, (random=~1|region), control=cont, weights=varPowy, data=gfish.rev)
plot(mod5.vpy.reg)
qqnorm(resid(mod5.vpy.reg))
qqnorm(resid(mod5.vpy.reg, type = "pearson"))
qqline(resid(mod5.vpy.reg, type = "pearson"))

varexp<-varExp(form=~plank)
mod6.vexp<-lme(fish~plank, (random=~1|region), weights=varexp, data=gfish.rev)
plot(mod6.vexp)
qqnorm(resid(mod6.vexp, type = "pearson"))
qqline(resid(mod6.vexp, type = "pearson"))

mod6.log.vexp<-lme(log(fish) ~ plank, (random=~1|region), weights=varexp, data=gfish.rev)
plot(mod6.log.vexp)
qqnorm(resid(mod6.log.vexp, type = "pearson"))
qqline(resid(mod6.log.vexp, type = "pearson"))

mod6.sr.vexp<-lme(sqrt(fish) ~ plank, (random=~1|region), weights=varexp, data=gfish.rev)
plot(mod6.sr.vexp)
qqnorm(resid(mod6.sr.vexp))

varexpy<-varExp(form=~plank|region)
mod7.vexpy<-lme(fish~plank, (random=~1|region), weights=varexpy, control=cont, data=gfish.rev)
plot(mod6.vexp)

varComb1 <-varComb(varFixed(~plank), varExp(form=~plank|region))
mod8.Comb1<-lme(fish~plank, (random=~1|region), weights=varComb1, control=cont, data=gfish.rev)
plot(mod8.Comb1)

varComb2 <-varComb(varIdent(form=~1|region), varPower(form=~plank))
mod9.Comb2<-lme(fish~plank, (random=~1|region), weights=varComb2, control=cont, data=gfish.rev)
plot(mod9.Comb2)

AICc(fullmod, mod2.vfixed, mod3.vIdent, mod4.vp, mod5.vpy, mod6.vexp, mod7.vexpy, mod8.Comb1, mod9.Comb2)

intervals(mod6.log.vexp)

r.squaredGLMM(mod9.Comb2)
