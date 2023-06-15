setwd("~/Documents/Teaching/Data Analysis/2018/Labs/Lab 10 logistic")

library(arm)
library(here)

### Data from UCLA ###
ucla <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")

eider <- ucla
names(eider) <- c("doublebrood", "upwelling", "mass", "age")

eider$upwelling <- -1*eider$upwelling + rnorm(400, 350, 1) 
eider <- eider %>% mutate(upwelling = ifelse(doublebrood == 1, upwelling - rnorm(400, 20, 0.001), upwelling))
eider$mass <- eider$mass - rnorm(400, 1, 0.1) + eider$mass*rnorm(400, 1, 0.1)
  
eider <- eider %>% mutate(mass = round(mass, 2)) %>%
                   mutate(upwelling = round(upwelling, 1)) %>%
                   dplyr::select(-f_age)

write.csv(eider, here("data", "Lab10eider.csv"), row.names = FALSE)

eider$f_age <- as.factor(eider$age)

boxplot(eider$mass~eider$doublebrood, xlab="double brood", ylab="mass")
plot(eider$doublebrood~eider$mass, ylab="double brood", xlab="mass")
plot(eider$doublebrood~eider$upwelling, ylab="double brood", xlab="upwelling")

### Logistic model ###
mod1 <- glm(doublebrood ~ mass + upwelling*f_age, family = binomial("logit"), data = eider, na.action=na.omit)

mod2 <- glm(doublebrood ~ mass + upwelling + f_age, family = binomial("logit"), data = eider, na.action=na.omit)

summary(mod1)

## Confidence intervals
# Likelihood profile method
# Note these are log odds
confint(mod1)

# Odds (model coefficients & CI)
exp(cbind(OR = coef(mod1), confint(mod1)))
# Interpretation: For 1 unit increase in mass, the odds of a double brood increase by a factor of 2.97

### Plots ###
## Binned residual plot
#binnedplot function from arm package
x <- predict(mod1)
y <- resid(mod1)
binnedplot(x,y)

# Pseudo R2
with(summary(mod1), 1 - deviance/null.deviance)

## Fitted model
upwellr <- data.frame(length = seq(from = -600, to = 150, by = 50))
Preds <- predict(mod1, newdata = upwellr, type = "response")
# predict not working
plot(x = eider$mass, y = eider$double)
lines(upwellr$mass, Preds)

### Evaluate odds & probabilities ###

## Change in odds for 0.2 unit change in mass ##
# Hold upwelling at mean and evaluate at each age category
# Age 1
odd1 <- exp(coef(mod1)[1] + coef(mod1)[2]*(mean(eider$mass)) + coef(mod1)[3]*mean(eider$upwelling, na.rm = T))
# 1.02

odd1.2 <- exp(coef(mod1)[1] + coef(mod1)[2]*(mean(eider$mass)+0.2) + coef(mod1)[3]*mean(eider$upwelling, na.rm = T))

# diff
odd1.2/odd1
# 1.068 times increase in odds of double brood with 0.2 unit increase from mean mass

odd1.2-odd1
# Age 2
odd2 <- exp(coef(mod1)[1] + coef(mod1)[4] + coef(mod1)[2]*(mean(eider$mass)) + (coef(mod1)[3] + coef(mod1)[7])*mean(eider$upwelling, na.rm = T))
# 0.532

odd2.2 <- exp(coef(mod1)[1] + coef(mod1)[4] + coef(mod1)[2]*(mean(eider$mass)+0.2) + (coef(mod1)[3] + coef(mod1)[7])*mean(eider$upwelling, na.rm = T))
# 0.568

# diff
odd2.2/odd2
# 1.068 times increase in odds of double brood with 0.2 unit increase from mean mass

# Age 3
odd3 <- exp(coef(mod1)[1] + coef(mod1)[5] + coef(mod1)[2]*(mean(eider$mass)) + (coef(mod1)[3] + coef(mod1)[8])*mean(eider$upwelling, na.rm = T))
# decrease: 0.27

odd3.2 <- exp(coef(mod1)[1] + coef(mod1)[5] + coef(mod1)[2]*(mean(eider$mass)+0.2) + (coef(mod1)[3] + coef(mod1)[8])*mean(eider$upwelling, na.rm = T))
# increase: 0.288

# diff
odd3.2/odd3
# 1.068

# Age 4
odd4 <- exp(coef(mod1)[1] + coef(mod1)[6] + coef(mod1)[2]*(mean(eider$mass)) + (coef(mod1)[3] + coef(mod1)[9])*mean(eider$upwelling, na.rm = T))
# 0.222

odd4.2 <- exp(coef(mod1)[1] + coef(mod1)[6] + coef(mod1)[2]*(mean(eider$mass)+0.2) + (coef(mod1)[3] + coef(mod1)[9])*mean(eider$upwelling, na.rm = T))
# increase: 0.237

# diff
odd4.2/odd4
# 1.068
# Change does not depend on level of age

## Change in probability for 0.2 unit change in mass ##
# Hold upwelling at mean and evaluate for each age category

## Age 1
# Probability of infection at mean(mass)
p.mean.a1 <- plogis(coef(mod1)[1] + coef(mod1)[2]*mean(eider$mass, na.rm = TRUE) + coef(mod1)[3]*mean(eider$upwelling, na.rm = T))

# Probability of infection at mean(mass) + 0.2
p.mean.a1.2 <- plogis(coef(mod1)[1] + coef(mod1)[2]*(mean(eider$mass, na.rm = TRUE)+0.2) + coef(mod1)[3]*mean(eider$upwelling, na.rm = T))

p.mean.a1.1 <- plogis(coef(mod1)[1] + coef(mod1)[2]*(mean(eider$mass, na.rm = TRUE)+1) + coef(mod1)[3]*mean(eider$upwelling, na.rm = T))

# Difference
p.mean.a1.2 - p.mean.a1
# 0.0163

# 1- unit increase in mass
p.mean.a1.1 - p.mean.a1

## Age 2
p.mean.a2 <- plogis(coef(mod1)[1]+coef(mod1)[4] + coef(mod1)[2]*mean(eider$mass, na.rm = TRUE) + (coef(mod1)[3] + coef(mod1)[7])*mean(eider$upwelling, na.rm = T))

# Probability of infection at mean(mass) + 0.2
p.mean.a2.2 <- plogis(coef(mod1)[1]+coef(mod1)[4] + coef(mod1)[2]*(mean(eider$mass, na.rm = TRUE) + 0.2) + (coef(mod1)[3] + coef(mod1)[7])*mean(eider$upwelling, na.rm = T))

# Difference
p.mean.a2.2 - p.mean.a2
# 0.015

## Age 3
p.mean.a3 <- plogis(coef(mod1)[1]+coef(mod1)[5] + coef(mod1)[2]*mean(eider$mass, na.rm = TRUE) + (coef(mod1)[3] + coef(mod1)[8])*mean(eider$upwelling, na.rm = T))

# Probability of infection at mean(mass) + 0.2
p.mean.a3.2 <- plogis(coef(mod1)[1]+coef(mod1)[5] + coef(mod1)[2]*(mean(eider$mass, na.rm = TRUE) + 0.2) + (coef(mod1)[3] + coef(mod1)[8])*mean(eider$upwelling, na.rm = T))

# Difference
p.mean.a3.2 - p.mean.a3
# 0.011

## Age 4
p.mean.a4 <- plogis(coef(mod1)[1]+coef(mod1)[6] + coef(mod1)[2]*mean(eider$mass, na.rm = TRUE) + (coef(mod1)[3] + coef(mod1)[9])*mean(eider$upwelling, na.rm = T))

# Probability of infection at mean(mass) + 0.2
p.mean.a4.2 <- plogis(coef(mod1)[1]+coef(mod1)[6] + coef(mod1)[2]*(mean(eider$mass, na.rm = TRUE) + 0.2) + (coef(mod1)[3] + coef(mod1)[9])*mean(eider$upwelling, na.rm = T))

# Difference
p.mean.a4.2 - p.mean.a4
# 0.01

############3
# Try ggplot
log.pl <- eider %>% ggplot(aes(x = upwelling, y = doublebrood, color = f_age)) + 
  geom_point(alpha = 0.5) +
  stat_smooth(formula = y ~ f_age*x + mass, method="glm", se = TRUE, method.args = list(family=binomial))

## Fitted model w/ confidence bands ##
# generate values at which to evaluate the model
# Holding mass constant here
newvalues <- with(eider, data.frame(upwelling = rep(seq(from = -600, to = 200, length.out = 100), 4), mass = mean(mass), f_age = factor(rep(1:4, each = 100))))

newvalues <- with(eider, data.frame(upwelling = rep(seq(from = min(upwelling, na.rm = TRUE), to = max(upwelling, na.rm = TRUE), length.out = 100), 4), mass = mean(mass), f_age = factor(rep(1:4, each = 100))))

predictions <- cbind(newvalues, predict(mod1, newdata = newvalues, type = "link", se = TRUE))

# Using normal approximation of confidence intervals here
predictions <- within(predictions, {
  PredictedProbs <- plogis(fit)
  loCI <- plogis(fit - (1.96 * se.fit))
  hiCI <- plogis(fit + (1.96 * se.fit))
})

pl <- ggplot(predictions, aes(x = upwelling, y = PredictedProbs)) + 
  geom_ribbon(aes(ymin = loCI, ymax = hiCI, fill = f_age), alpha = 0.2) + 
  geom_line(aes(colour = f_age), size = 1)

pl2 <- ggplot(predictions, aes(x = upwelling, y = PredictedProbs)) + 
  geom_ribbon(aes(ymin = loCI, ymax = hiCI), alpha = 0.2) + 
  geom_line(size = 1)

#plot logistic model
lengthr<-data.frame(length=seq(from=46.5, to=165, by=1))
Preds<-predict(mod1, newdata=lengthr, type="response")
plot(x=Boar$length, y=Boar$TB)
lines(lengthr$length, Preds)

#other attempts that didn't work
curve(predict(mod1,x=Boar$length,type="response"),add=TRUE)
lines(Boar$length, mod1$fitted, type="l", col="red")
summary(mod1$fitted)
curve(invlogit(coef(mod1)[1]+coef(mod1)[2]*x, add=TRUE))

##############################################
#Cod
ParasiteCod$fArea<-factor(ParasiteCod$Area)
ParasiteCod$fYear<-factor(ParasiteCod$Year)
mod2<-glm(Prevalence~fArea*fYear+Length, family=binomial, data=ParasiteCod)

boxplot(ParasiteCod$Length~ParasiteCod$Prevalence, xlab="Prevalence", ylab="length (cm)")
plot(Boar$TB~Boar$length, ylab="TB", xlab="length (cm)")

mod3<-glm(Prevalence~Length, family=binomial, data=ParasiteCod)

x<-predict(mod3)
y<-resid(mod3)
binnedplot(x,y)
binnedplot(ParasiteCod$Area, y)
plot(ParasiteCod$Area, y)

mod4<-glm(Prevalence~Length, family=binomial, data=CodNA)
