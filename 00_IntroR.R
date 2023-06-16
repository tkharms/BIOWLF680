### Intro to R ###

# Demonstration of basic data management and plotting functions

library(tidyverse)
library(here)

## Resources:
## Github training:
# https://nceas.github.io/training-git-intro/getting-started-with-git-rstudio.html

## Intro to R:
# https://datacarpentry.org/R-ecology-lesson/01-intro-to-r.html
# https://datacarpentry.org/R-ecology-lesson/

## Tips:
# ??function name: calls documentation

# large data files or many data files to upload/download?: read and write to Google Drive using googledrive package

# install new libraries: install.packages("nameofpackage")

# StackExchange is a tremendous resource

# Possibility for ChatGPT to help with writing coe

### Read in data ###
dat <- read.csv(here("data", "Lab1data.csv"), header = TRUE)

# head(dat) or tail(dat): view first or last 5 rows
# View(dat): display data

### Explore the data structure ###
# unique values of categorical columns
unique(dat$species)

# Quantitative summary of continuous variables
summary(dat)

# Quick plot
plot(dat$length_mm, dat$mass_g, col = factor(dat$species))

### Data management ###

## Using tidyverse functions ##
## Drop a column
dat_nomass <- dat %>% select(-mass_g)

## Filter rows
weasels <- dat %>% filter(species == "weasel")

## Pivot from wide to long format
dat_long <- dat %>% pivot_longer(c(length_mm, mass_g), names_to = "type", values_to = "value")

## Make a new column
dat <- dat %>% mutate(BMI = mass_g/length_mm^2)

## Summary table 
dat_sum <- dat %>% group_by(species) %>%
                   mutate(n = n()) %>%
                   summarize(across(length_mm:BMI, list(mean = mean, stdev = sd), na.rm = TRUE))
  
### Plotting ###
## Using ggplot ##

## Scatterplot
ggplot(dat, aes(x = length_mm, y = mass_g)) +
  geom_point(aes(color = species))

## Boxplot
ggplot(dat, aes(x = species, y = length_mm)) +
  geom_boxplot()

# Make this pretty
ggplot(dat, aes(x = species, y = length_mm)) +
  geom_boxplot() +
  ylab("length (mm)") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "red", linewidth = 2),
        axis.text = element_text(size = 24),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 24),
        strip.background = element_blank(),
        strip.text = element_text(size = 24))
        
## Facetted plotting
LM.pl <- dat %>% ggplot(aes(x = length_mm, y = mass_g)) +
                    geom_point() +
                    facet_wrap(~species, scales = "free")
  
# Save plot
ggsave(LM.pl, path = "plots", file = "length_mass.pdf", width = 20, height = 10, units = "in")
