##Lab 7: Regression##
library(MASS)
library(car)
library(boot)

# lmass<-read.csv("~/Documents/Teaching/Data Analysis/2016/Labs/Lab 6/lab6dataset2.csv")
# Seldat<-read.csv("~/Documents/Teaching/Data Analysis/2016/Labs/Lab 6/lab6dataset1.csv")
# names(lmass)<-c("index", "length", "width", "energy")
# names(Seldat)<-c("index", "mass", "sel")

### Generate data
chicks <- rnorm(12, 8, 0.25)
Sel <- 1/chicks+rnorm(12,0.05,0.005)
Lab07dat1 <- data.frame(chick_mass_g = chicks, selenium_ugL = Sel)

plot(chicks, Sel)

write.csv(Lab07dat1, here("data", "Lab07dat1.csv"))

### Regression: chick mass, selenium
chicks <- lm(chick_mass_g ~ selenium_ugL, data = Lab07dat1)

plot(chicks)
#residuals are wild sel vs. mass regression...small dataset, outliers, non-normal

# Try Box-Cox
boxcox(chicks)
#no transformation identified by Box-Cox

summary(mod1)

chick.pl<-ggplot(Lab07dat1, aes(x = selenium_ugL, y = chick_mass_g))+
  geom_point()+
  geom_smooth(method = lm)

chick.boot <- Boot(chicks, R = 10000, labels=names(coef(chicks)), method = "case")
summary(chick.boot)
boot.ci(mod1boot, index=2)

#bootstrap indicates that bias from least squares estimates are: -0.06 for intercept and 0.28 for slope. Small compared to original estimates of 12.7 and -26.6

### Multiple regression: length, mass, energy ###
dat2 <- read.csv(here("data", "Lab07dat2.csv"))
names(dat2) <- c("index", "length_mm", "width_mm", "energy")

pairs(~length_mm + width_mm + energy, data = dat2)

dat2$widsq <- dat2$width_mm^2
pairs(~length_mm + width_mm + energy + widsq, data = dat2)

mod.lw <- lm(energy ~ length_mm + width_mm, data = dat2)
plot(mod.lw)
summary(mod.lw)
# U-shaped residuals

mod.lwsq <- lm(energy ~ length_mm + I(width_mm^2) + width_mm, data = dat2)
plot(mod.lwsq)
# ok- outliers
summary(mod.lwsq)

# Alternatively, square-root transform
mod.sq <- lm(sqrt(energy) ~ length_mm + width_mm, data = dat2)
plot(mod.sq)
# ok
summary(mod.sq)

#lab report should include either a 3D plot or plots of each bivariate relationship

