### Lab 3 key ###

library(here)
library(MASS)
library(tidyverse)
library(coin)

Pilot1 <- read.csv(here("data", "Lab03_Pilot1.csv"))
Pilot2 <- read.csv(here("data", "Lab03_Pilot2.csv"))
Chap1 <- read.csv(here("data", "Lab03_Chap1.csv"))

names(Pilot1) <- c("pool", "treatment", "biomass")
names(Pilot2) <- c("pool", "biomass", "treatment")
names(Chap1) <- c("pool", "biomass", "treatment")

### Pilot 1 ###
#two samples
#subset data by treatment
fish <- subset(Pilot1, treatment=="fish")
nofish <- subset(Pilot1, treatment=="no fish")

## evaluate normality
qqnorm(fish$biomass)
qqline(fish$biomass)

qqnorm(nofish$biomass)
qqline(nofish$biomass)
#data appear non-normal (and datasets are too small to properly evaluate)

## Evaluate equal variance
# Visualize variances
boxplot(biomass ~ treatment, data = Pilot1)
# variances are grossly unequal

# F-test of equal variance
Fvar <- var(fish$biomass)/var(nofish$biomass)
2*pf(Fvar,7,7)
#variances are unequal. Resampling test is inappropriate. Compare bootstrapped CI about the medians.

out <- boot(fish$biomass,function(x,i) median(x[i]), R=1000)

fishCI <- boot.ci(boot(fish$biomass,function(x,i) median(x[i]), R=1000))
nofishCI <- boot.ci(boot(nofish$biomass,function(x,i) median(x[i]), R=1000))

lowCI <- c(0.889,44.44)
upperCI <- c(3.54,190.29)

group.mds <- aggregate(Pilot1[, 3], list(Pilot1$treatment), median)
names(group.mds) <- c("treatment", "biomass")

# tidyverse approach to summarizing data
Pilot1.md <- Pilot1 %>% group_by(treatment) %>%
                        summarize(across(.cols = biomass, ~median(.x)))

#generate plot with custom values
p3 <- ggplot(Pilot1.md, aes(y = biomass, x = treatment)) +
  geom_point(size = 10) +
  geom_errorbar(aes(x = treatment, ymin = lowCI, ymax = upperCI), size = 1, width = 0.2) +
  xlab(" ") + 
  ylab("biomass")

#generate plot with custom values
p2 <- ggplot(group.mds, aes(y = biomass, x = treatment))+
geom_point(size = 10)+
geom_errorbar(aes(x = treatment, ymin = lowCI, ymax = upperCI), size = 1, width = 0.2)+
xlab(" ")+ 
ylab("biomass")

#resampled t-test for comparison. Inappropriate here due to unequal variances between groups
oneway_test(biomass~treatment, data = Pilot1, distribution="exact")
oneway_test(biomass~treatment, data=Pilot1, distribution = approximate(nresample = 12807))

##Pilot 2##
#paired samples

### TKH revise with pivot_wider
#compute differences
diffs <- Pilot2[1:8,2]-Pilot2[9:16,2] 
boxplot(diffs)
boxplot(Pilot2$biomass, Pilot2$treatment)

#evaluate normality
qqnorm(diffs)
qqline(diffs)
#differences do not appear normally distributed

#### TKH update for tidyverse
#apply a resampling test appropriate for paired observations
library(reshape2)
#convert from long to wide format
wPilot2<-dcast(Pilot2,  pool ~ treatment, value.var="biomass")
names(wPilot2)<-c("pool", "fish", "nofish")

#reshape using dplyr instead
wPilot2 <- spread(Pilot2, treatment, biomass)

#resampling test for paired differences from coin package
library(coin)
wilcoxsign_test(fish~nofish, data=wPilot2, distribution="exact")

##Chapter 1##
mod1<-lm(biomass~treatment, data=Chap1)
#variances unequal

#compare CI about medians

#if variances were equal, resampling test would be appropriate
oneway_test(biomass~treatment, data=Chap1, distribution=approximate(B=10000))

#"by-hand" method of calculating resampled t-test
#calculate difference between observed means
group.means<-aggregate(Pilot1[, 3], list(Pilot1$treatment), mean)
truediff<-abs(group.means[1,2]-group.means[2,2])

#function to carry out single permutation
perms<-function(){
  permsamps<-sample(Pilot1$biomass, replace=TRUE)
  tapply(permsamps,Pilot1$treatment,mean)->grps
  grps[1]-grps[2]
}

#iterate the permutation
diffs<-replicate(5000,perms())

#combine true diff with permuted diffs
alldiffs<-c(diffs,truediff)

#one-tailed test
sum(diffs>=truediff)/length(diffs)

#two-tailed test
2*sum(alldiffs>=truediff)/length(diffs)

#histogram of permuted differences
hist(diffs, main ="differences of permuted means", xlim=c(-200, 200))
#show observed difference
abline(v=truediff, col="red", lwd=5)

### Chap 1 ###
ch1.aov <- aov(biomass ~ treatment, data = Chap1)
leveneTest(ch1.aov)

plot(ch1.aov)

oneway_test(biomass ~ treatment, data = Chap1)
