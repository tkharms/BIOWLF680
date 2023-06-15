### KEY: Lab 12 Bayes ###

library(rstan)
library(here)
library(gdata)
library(bayesplot)
library(brms)

### Data
ninv <- read.csv(here::here("data", "Invasive count data.csv"))
roads <- read.csv(here::here("data", "Invasive road data.csv"))
        
dat <- full_join(ninv, roads, by = "Study_area") 

write.csv(dat, here::here("data", "Lab12dat.csv"))

pairs(~Disturbance + Number_exotics + Road_density, col = dat$Study_area, data = dat)

# regional invasives pool increases as asymptotic function of road density 
curve(x/(0.5 + x))

curve(x + x^2)

curve(0.5*exp(2*x), xlim = c(0, 0.5))

curve(dnorm(x, 0.5, 1), xlim=c(-10, 10))

curve(dt(x, 3, 15.8), xlim=c(-10, 10))

## Fit model using default priors
# Skipping the modeling of regional intercepts as a function of road density
prior1 <- prior(normal(2,10), nlpar = "b1") +
          prior(normal(5,10), nlpar = "b2")

inv.df <- brm(bf(Number_exotics ~ b1*Disturbance + b2*Disturbance^2, b1 + b2 ~1 + (1|Study_area), nl = TRUE),
                  family = brmsfamily('poisson'),
                  data = dat, prior = prior1,
               iter = 5000,
               chains = 4, cores = 4)

inv.df2 <- brm(bf(Number_exotics ~ b1*exp(b2*Disturbance), b1 + b2 ~1 + (1|Study_area), nl = TRUE),
              data = dat, prior = prior1,
              iter = 5000,
              chains = 4, cores = 4)

inv.df2 <- brm(bf(Number_exotics) ~ Disturbance +  (1|Study_area)), 
               data = dat, 
               iter = 5000,
               chains = 4, cores = 4)

inv.df3 <- brm(bf(Number_exotics ~ Disturbance + (1|Study_area)),
               family = brmsfamily('poisson'),
               data = dat, 
               iter = 5000,
               chains = 4, cores = 4)

prior2 <- prior(normal(1, 1), class = "Intercept")

inv.df4 <- brm(bf(Number_exotics ~ Disturbance + (1|Study_area)),
               family = brmsfamily('poisson'),
               data = dat, 
               prior = prior2,
               iter = 5000,
               chains = 4, cores = 4)

inv.df <- brm(bf(Number_exotics ~ Disturbance + Disturbance^2 + (1|Study_area),
                 family = brmsfamily('poisson'), nl = TRUE), data = dat, 
              iter = 10000,
              chains = 4, cores = 4)

## Check divergent priors
pairs(inv.df3)

## Check automated priors
get_prior(Number_exotics ~ Disturbance + (1|Study_area),
          family = brmsfamily('poisson'),
          data = dat)
pp_check(inv.df3)


## Extract annotated Stan code
# This helps you gain understanding of Stan syntax. brms can handle many common model structures, but you will need to use Stan for more complex models.
stancode(inv.df3)

## Check posteriors
summary(inv.df3)
plot(inv.df3)

plot(conditional_effects(inv.df3), points = TRUE)

## Try a normal prior on the intercept. This is slightly more constrained than the uniform prior applied previously.
pr1 <- prior(normal(0, 1), class = "b")

# Refit model with updated prior
rich.df.norm <- brm(bf(Richness ~ I(Year-2007),
                       family = brmsfamily('poisson')), 
                    prior = pr1,
                    data = dat,
                    iter = 1000,
                    chains = 4, cores = 4)

# Check prior
pp_check(rich.df.norm)

## Check posteriors
summary(rich.df.norm)
plot(rich.df.norm)

## Bayesian R2
bayes_R2(rich.df.norm)