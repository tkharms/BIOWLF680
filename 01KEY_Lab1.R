### Cook data for Lab 1 ###

library(here)
library(tidyverse)

weasel_length <- rlnorm(50, mean = 5, sd = 0.2)
weasel_mass <- rnorm(50, mean = 75, sd = 10)
ermine_length <- rlnorm(50, mean = 5.5, sd = 0.2)
ermine_mass <- rnorm(50, mean = 100, sd = 12)

weasels <- cbind(species = rep("weasel", 50), length_mm = weasel_length, mass_g = weasel_mass)
ermines <- cbind(species = rep("ermine", 50), length_mm = ermine_length, mass_g = ermine_mass)
Lab1data <- data.frame(rbind(weasels, ermines))

# Cook in some NAs
Lab1data <- Lab1data %>% mutate(length_mm = as.numeric(as.character(length_mm))) %>%
                         mutate(mass_g = as.numeric(as.character(mass_g))) %>%
                         mutate(length_mm = ifelse(length_mm >= 231 & length_mm <= 234, NA, length_mm))

write.csv(Lab1data, here("data", "Lab1data.csv"), row.names = FALSE)

Lab1data <- read.csv(here("data", "Lab1data.csv"))

