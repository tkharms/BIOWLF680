###Lab5 2-way ANOVA###

library(tidyverse)
library(here)
library(MASS)
library(emmeans)
library(multcomp)
library(agricolae)

lab5data <- read.csv(here("data", "Lab05data.csv"), header=TRUE)
names(lab5data) <- c("pfrost", "burntime", "ANPP")

#paired box plot
lab5box <- ggplot(lab5data, aes(y=ANPP, x=burntime, fill=pfrost)) +
    geom_boxplot(position="dodge") + 
    xlab(" ") +
    ylab("ANPP")

#interaction plot
interaction.plot(lab5data$burntime, lab5data$pfrost, lab5data$ANPP)

mod1 <- lm(ANPP ~ burntime*pfrost, data=lab5data)

mod1.aov <- aov(ANPP ~ burntime*pfrost, data=lab5data)

leveneTest(mod1.aov)
plot(mod1)
# Normality ok, variances unequal. Try log

mod1.log <- lm(log(ANPP) ~ burntime*pfrost, data = lab5data)
plot(mod1.log)
leveneTest(mod1.log)
#a-ok

summary(mod1.log)

mod1.sr <- lm(sqrt(ANPP + 0.5) ~ burntime*pfrost, data = lab5data)
mod1.sr.aov <- aov(sqrt(ANPP + 0.5) ~ burntime*pfrost, data = lab5data)

boxcox(mod1)

#Tukey's on interaction term
# (interaction not significant)
TukeyHSD(mod1.log)

tx <- with(lab5data, interaction(burntime, pfrost))
mod.aov<-aov(ANPP~tx, data=lab5data)
mod.lm<-lm(ANPP~tx, data=lab5data)
HSD.test(mod.lm, "tx", group=TRUE, console=TRUE)

#Tukey's on single term
TukeyHSD(mod1.sr.aov, "burntime")
HSD.test(mod1.sr, "burntime", group=TRUE, console=TRUE)

# Some students used log transform:
HSD.test(mod1.log, "burntime", group=TRUE, console=TRUE)

## Generate letters for display on figures
# Requires emmeans and multcomp packages. 
inter.ph <- emmeans(mod1.log, pairwise ~ burntime, adjust = "tukey")
inter.ph.lett <- cld(inter.ph, Letter = "abcdefg")

inter.ph <- glht(mod1.log, linfct = mcp(burntime = "Tukey"))
inter.ph.lett <- cld(inter.ph, Letter = "abcdefg")

inter.ph <- emmeans(mod1.aov, pairwise ~ nitrogentreat, adjust = "tukey")
inter.ph.lett <- cld(inter.ph$emmean, Letter = "abcdefg")

##power results from GPower
#power of interaction=0.986
#required replicates (power=0.95)=32
#required replicates for half effect size=112 (14/group)

##part2##
#Generate data
#did this quick and dirty. Data not reproduced here. They are gamma distributed
lab5_2<-read.csv("~/Documents/Teaching/Data Analysis/2016/Labs/Lab5/lab5part2data.csv")
names(lab5_2)<-c("pfrost", "burntime", "ANPP")

#paired box plot
lab5_2box<-qplot(burntime, ANPP, fill=pfrost, data=lab5_2, geom="boxplot", position="dodge", xlab=" ", ylab="ANPP")

#interaction plot
interaction.plot(lab5_2$burntime, lab5_2$pfrost, lab5_2$ANPP)

mod1<-lm(ANPP~burntime*pfrost, data=lab5_2)

mod1.aov <- aov(ANPP~burntime*pfrost, data=lab5_2)

boxcox(mod1)
#log transform
lab5_2$LN_ANPP<-log(lab5_2$ANPP+1)

lab5_2box<-qplot(burntime, LN_ANPP, fill=pfrost, data=lab5_2, geom="boxplot", position="dodge", xlab=" ", ylab="LN ANPP")

mod.ln<-lm(LN_ANPP~burntime*pfrost, data=lab5_2)

anova(mod.ln)


#square root transform
lab5_2$SR_ANPP<-sqrt(lab5_2$ANPP+0.5)

lab5_2box<-qplot(burntime, SR_ANPP, fill=pfrost, data=lab5_2, geom="boxplot", position="dodge", xlab=" ", ylab="SR ANPP")

mod.sr<-lm(SR_ANPP~burntime*pfrost, data=lab5_2)

##power
#power>0.99
#required replicates (power=0.95)=24 (3 reps per trt)
#required replicates, half effect size (power=0.95)=80 (or 72 would be just under 0.95 power)

#students used y to the 0.2 transformation
mod.st<-lm(I(ANPP^0.2)~burntime*pfrost, data=lab5_2)

anova(mod.ln)
##Lecture example##
twowaydata<-read.csv("~/Documents/Teaching/Data Analysis/2016/Lectures/twowaydata.csv")
twmod<-lm(NBI~nitrogentreat*fungitreat, data=twowaydata)
summary(twmod)
aov(twmod)
anova(twmod)

twmod.aov<-aov(NBI~nitrogentreat*fungitreat, data=twowaydata)

mod2<-lm(NBI~nitrogentreat+fungitreat, data=twowaydata)
anova(twmod,mod2)

#paired box plot
plot<-ggplot(twowaydata)+geom_boxplot(aes(x=nitrogentreat, y=NBI, fill=fungitreat))
