### Lab 3: Resampling ###

# Permutation tests
# Bootstrapped quantities

library(here)
library(tidyverse)
library(boot)
library(coin)

## Bootstrapped 95% confidence interval using the boot package ##
# Use to bootstrap a confidence interval about any statistic (even those lacking a theoretical distribution)
# Cook some data
rdat <- rpois(100, 8)

# Define a function to calculate the statistic of interest (example: median)
med = function(rdat, i){median(rdat[i])}

# Apply the boot.ci function to resample the data 1000 times, and produce estimates of 95% CI from the resampled distribution
rdat.CI <- boot.ci(boot(rdat, med, R = 1000))

# An approach for plotting your bootstrapped confidence intervals using ggplot2 #
# Extract normal CI from boot.ci object
lower.CI <- rdat.CI$normal[,2]
upper.CI <- rdat.CI$normal[,3]
mdn <- median(rdat)

# Convert random vector to dataframe for plotting
rdat.df <- data.frame(var = rdat)

CI.pl <- rdat.df %>% ggplot(aes(y = mdn, x = 1)) +
geom_point(size = 10) +
geom_errorbar(aes(ymin = lower.CI, ymax = upper.CI), width = 0.1, size = 0.5)

## Permutation t-test (2 sample), "by hand" ##
# Import data
foxdata <- read.csv(here("data", "Lect4_foxdata.csv"))

# Set number of (re)samples
R <- 5000

# Make a container to hold permuted differences between means and include a place for the observed difference
mn.diff <- numeric(R + 1)

# Observed difference between means
mn.diff[1] <- mean(foxdata$no.fox) - mean(foxdata$fox)

# Restructure the data so that both groups are in a single vector
# Using a bit of tidyverse here
poolfox <- foxdata %>% pivot_longer(c(fox, no.fox), values_to = "NPP") %>%
                       select(NPP) %>%
                       data.frame()

# Permute mean differences
for(i in 2:length(mn.diff)){
  index <- sample(1:50, size = 25, replace = TRUE) #sample function performs the resampling
  foxperm <- poolfox$NPP[index]
  nofoxperm <- poolfox$NPP[-index]
  mn.diff[i] <- mean(foxperm) - mean(nofoxperm)
}

# Histogram of permuted differences
hist(mn.diff, main ="differences of permuted means")

# Show the observed difference
abline(v = mn.diff[1], col="red", lwd=5)

# Calculate P-value (two-tailed) as the proportion of permutations with a mean difference greater/less than observed
# Note use of absolute values here. This yields the probability for a 2-tailed test (i.e., probability of observing a mean differences greater than or less than the observed).
mean(abs(mn.diff) >= abs(mn.diff[1]))

## Use functions in the coin package to carry out permutation tests ##
# Restructure the data into two columns: one for categories and one for NPP (response variable)
fox.long <- foxdata %>% pivot_longer(c(fox, no.fox), values_to = "NPP", names_to = "cat")

oneway_test(fox.long$NPP ~ as.factor(fox.long$cat), alternative="two.sided", distribution = approximate(nresample = 5000))

## F test of equal variance between two groups ##
# Calculate ratio of variances of the two groups
Fvar <- var(foxdata$no.fox)/var(foxdata$fox)

# Compare ratio to the F-distribution. 
# Null hypothesis: ratio is equal to 1
2*pf(Fvar, length(foxdata$no.fox)-1, length(foxdata$fox)-1)
# P-value is >> 0.05. Fail to reject the null. Variances are equal.

# Same test, using var.test function
var.test(NPP ~ cat, fox.long, 
         alternative = "two.sided")
