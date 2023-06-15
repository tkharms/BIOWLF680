### Lab 11 KEY: PCA w/  ###

library(here)
library(tidyverse)
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
library(FactoMineR)
library(factoextra)
library(corrplot)

## Norge lakes data ##
dat <- read.csv(here::here("data", "Lab11dat.csv"))

#dat1 <- read.csv(here::here("data", "lab6data.csv"))

pairs(~prodn + pH + DO + Temp + TDS + depth + flow + substr + veg + width + canopy + turb, data = dat)

## PCA ##
# princomp: spectral decomposition
prodPCA <- princomp(~pH + DO + Temp + TDS + depth + flow + substr + veg + width + canopy + turb, data = dat , cor=TRUE)

summary(prodPCA)  #print proportion variance explained by each axis
loadings(prodPCA)  #print factor loadings

plot(prodPCA)  # “scree” plot (convenient way of visualizing variance explained by components)

prodscores <- prodPCA$scores  #extract the component scores for each observation

plot(prodscores[,1], prodscores[,2], xlab="PC1", ylab="PC2")
plot(prodscores[,1], prodscores[,3], xlab="PC1", ylab="PC3")
plot(prodscores[,2], prodscores[,3], xlab="PC2", ylab="PC3")

# prcomp: singular value decomposition 
prodPCA2 <- prcomp(~pH + DO + Temp + TDS + depth + flow + substr + veg + width + canopy + turb, data = dat, center = TRUE, scale = TRUE)

summary(prodPCA2)
prodPCA2$rotation[,1:4]

### Plotting ###
## Scree plot
plot(prodPCA)

## Factor loadings
# extract results using functions from FactoMineR
var <- get_pca_var(prodPCA2)

# extract loadings (as % contribution)
contrib <- var$contrib

# plot loadings (using function from corrplot package)
corrplot(contrib, is.corr = FALSE)

## Biplots of PC scores ##
# Arrow length = percentage of total variation explained by the variable
# Arrow angle = factor loadings on components 1 & 2
ggbiplot(prodPCA2)


## Regression with PCs as predictors ##

# extract component scores for PCs 1 & 2 for each observation
prodscores2 <- prodPCA2$x[,1:3]

# Join PCA scores to observations
dat.pc <- dat %>% select(c(pH , DO , Temp , TDS , depth , flow , substr , veg , width , canopy , turb, prodn))
dat.pc <- dat.pc[complete.cases(dat.pc),]
dat.pc <- cbind(dat.pc, prodscores2)

# Exploratory plots
mod.pl <- dat.pc %>% ggplot(aes(x = PC1, y = prodn)) +
  geom_point() +
  stat_smooth(method = "lm")

mod.pl2 <- dat.pc %>% ggplot(aes(x = PC2, y = prodn)) +
  geom_point() +
  stat_smooth(method = "lm")

mod.pl3 <- dat.pc %>% ggplot(aes(x = PC3, y = prodn)) +
  geom_point() +
  stat_smooth(method = "lm")

ggsave(mod.pl2, path = here::here("plots"), file = "NO3pc2.pdf", width = 8, height = 8, units = "in")

# Regression
mod <- lm(prodn ~ PC1 + PC2 + PC3, data = dat.pc)
