### PCA example ###

library(here)
library(tidyverse)
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
library(FactoMineR)
library(factoextra)
library(corrplot)

## Norge lakes data ##
lakes <- read.csv(here("data", "Lect18_lakes.csv"))

pairs(~Wshedarea + Annl.Precip + Temprange + MAT + Runoff + NDVI + Slope + Elevation + pctForest  + pctBog + pctLake + pctAg + NO3 + DOC +  Ndep + pctLake.1, data = lakes)  

lakes <- lakes %>% filter(NO3 < 4250)

pairs(~Wshedarea + Annl.Precip + Temprange + MAT + Runoff + NDVI + Slope + Elevation + pctForest  + pctBog + pctLake + pctAg + NO3 + DOC +  Ndep + pctLake.1, data = lakes)  

## PCA ##
# princomp: spectral decomposition
lakesPCA <- princomp(~Wshedarea + Annl.Precip + Temprange + MAT + Runoff + NDVI + Slope + Elevation + pctForest  + pctBog + pctLake + pctAg + Ndep, data = lakes , cor=TRUE)

summary(lakesPCA)  #print proportion variance explained by each axis
loadings(lakesPCA)  #print factor loadings

plot(lakesPCA)  # “scree” plot (convenient way of visualizing variance explained by components)

lakesscores <- lakesPCA$scores  #extract the component scores for each observation

plot(lakesscores[,1], lakesscores[,2], col=lakes$index, xlab="PC1", ylab="PC2")

# prcomp: singular value decomposition 
lakesPCA2 <- prcomp(~Wshedarea + Annl.Precip + Temprange + MAT + Runoff + NDVI + Slope + Elevation + pctForest  + pctBog + pctLake + pctAg + Ndep, data = lakes, center = TRUE, scale = TRUE)

summary(lakesPCA2)
lakesPCA2$rotation[,1:4]

### Plotting ###
## Scree plot
plot(lakesPCA)

## Factor loadings
# extract results using functions from FactoMineR
var <- get_pca_var(lakesPCA2)

# extract loadings (as % contribution)
contrib <- var$contrib

# plot loadings (using function from corrplot package)
corrplot(contrib, is.corr = FALSE)

## Biplots of PC scores ##
# Arrow length = percentage of total variation explained by the variable
# Arrow angle = factor loadings on components 1 & 2
ggbiplot(lakesPCA2)




## Regression with PCs as predictors ##

# extract component scores for PCs 1 & 2 for each observation
lakesscores2 <- lakesPCA2$x[,1:2]

# Join PCA scores to observations
lakes.dat <- lakes %>% select(c(NO3, Wshedarea, Annl.Precip, Temprange, MAT, Runoff, NDVI, Slope,  Elevation, pctForest, pctBog, pctLake, pctAg, Ndep))
lakes.pca <- lakes.dat[complete.cases(lakes.dat),]
lakes.pca <- cbind(lakes.pca, lakesscores2)

# Exploratory plots
mod.pl <- lakes.pca %>% ggplot(aes(x = PC1, y = log(NO3))) +
                        geom_point() +
                        stat_smooth(method = "lm")

mod.pl2 <- lakes.pca %>% ggplot(aes(x = PC2, y = log(NO3))) +
  geom_point() +
  stat_smooth(method = "lm")

ggsave(mod.pl2, path = here::here("plots"), file = "NO3pc2.pdf", width = 8, height = 8, units = "in")

# Regression
mod <- lm(log(NO3) ~ PC1 + PC2, data = lakes.pca)



