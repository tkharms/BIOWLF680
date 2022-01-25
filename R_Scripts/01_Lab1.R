### Lab 1: Intro to R ###
## Intro to data import/export
## Basic plotting functions

## Inputs:
    # Lab1data.csv: assignment data
      # Length and mass measurements of 2 species of small mammals

## Outputs:
    # Summary statistics table
    # Box plot

library(here)
library(tidyverse)

dat <- read.csv(here("data", "Lab1data.csv"))

plot(dat$length_mm, dat$mass_g)

LW.pl <- ggplot(dat, aes(x= length_mm, y = mass_g)) +
  geom_point(color = "blue") +
  ylab("body mass (g)")

dir.create("plots")

ggsave(LW.pl, path = here("plots"), file = "length_weight_scatter.pdf", width = 10, height = 8, units = "in")

