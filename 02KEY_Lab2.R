### Lab 2: Distributions ###
## Cook data for Lab 02
## KEY

library(here)

# DistA = Poisson
distA <- rpois(100, 25)

# DistB = lognormal
distB <- rlnorm(100, 1, 0.5)

Lab02data <- data.frame(cbind(dat = rbind(matrix(distA), matrix(distB)), dist = rbind(matrix(rep("distA", 100)), matrix(rep("distB", 100)))))

names(Lab02data) <- c("dat", "dist")

write.csv(Lab02data, here("data", "Lab02data.csv"), row.names = FALSE)

#########################
Lab2data <- read.csv(here("data", "Lab02data.csv"))

curve(dnorm(x, 1, 0.5))

hist(distA, breaks = 25)
hist(distB, breaks = 25)

# Parameters of the distributions
Lab2data %>% group_by(dist) %>%
             summarize(across(dat, list(mn = mean, md = median, SD = sd)))

########################
### Fun with probabilities ###
## Normal dist
NPP.mn <- 125
NPP.var <- 100
NPP.SD <- sqrt(NPP.var)

# 3 SD greater than the mean
NPP.3SD <- NPP.mn + 3*NPP.SD

# probability of 3 SD > NPP.mn under normal distribution
pnorm(NPP.3SD, NPP.mn, NPP.SD, lower.tail = FALSE)
#0.00135

# 95% of the NPP data described by parameters above
qnorm(0.95, NPP.mn, NPP.SD)
# 141

## Poisson dist
cloud.lam <- 8
cloud.obs <- 2

ppois(cloud.obs, cloud.lam)
# 0.01375