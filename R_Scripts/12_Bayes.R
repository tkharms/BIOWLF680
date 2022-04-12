### Bayesian models ###

library(rstan)
library(here)
library(gdata)
library(bayesplot)
library(brms)

### Test model ###
## run this set of commands to check that Stan and RStan have properly installed and to understand the structure of a Stan model

# This is the Stan code specifying the model and priors. This could be written outside of R and called in. Here we place all of the Stan code in "" and assign it as an object in R.
# Stan uses // rather than # for comments

eightschools <- "
data {
int<lower=0> J; // number of schools
real y[J]; // estimated treatment effects
real<lower=0> sigma[J]; // s.e. of effect estimates
}
parameters {
real mu; // mean effect for schools
real<lower=0> tau; // variance
real eta[J]; // individual school effect
}
transformed parameters { // theta is a function of our parameters
real theta[J];
for (j in 1:J)
theta[j] = mu + tau * eta[j];
}
model {
target += normal_lpdf(eta | 0, 1); // eta ~ N(0,1)
target += normal_lpdf(y | theta, sigma); // y ~ N(theta, sigma^2), theta(mu, tau, eta)
}"

# Set seed is not needed. Just useful for checking that we all get the same output.
set.seed(0)

## Data
# Here we are inputting data manually as a list. More typically our data would be stored as an object already.
schools_dat <- list(J = 8,
                    y = c(28, 8, -3, 7, -1, 1, 18, 12),
                    sigma = c(15, 10, 16, 11, 9, 11, 10, 18))

## The stan command runs the model
# model_code refers to the object containing the Stan code
# iter = number of MCMC steps
# warmup = # MCMC steps used to tune the MCMC sampler (these are automatically dropped from calculations of the posterior)
# chains = number of instances of the MCMC sampler to run. For typical applications, use 4 chains. More chains will increase the run time.
fit <- stan(model_code = eightschools,
            data = schools_dat, iter = 10000, warmup = 100, chains = 4)

## Summary of posteriors
# print produces a summary across all chains
# Includes mean, se, and quantiles of the distributions
# n_eff = effective number of independent draws from the posterior. Should be at least the same as the number of MCMC samples. Values greater than the number of MCMC samples indicates Stan is producing estimates better than would be obtained from independent samples.
# Rhat = potential scale reduction factor. It is a metric of MCMC convergence that compares the within chain variance to variance across the chains. Well-mixed chains will have Rhat near 1. Do not proceed with analysis if Rhat > 1.05. Run more MCMC steps.
print(fit)

## Summarize posteriors for each chain
# # Results printed for each chain. They should arrive at similar values
summary(fit)

# Dot & whisker plot of posteriors. Can specify which parameters to plot using the pars argument. Here plotting first 10 parameters.
plot(fit, ci_level = 0.95, outer_level = 0.999)

# Plot posteriors as densities
plot(fit, show_density = TRUE, ci_level = 0.95, outer_level = 0.999)

# Posterior for single parameter, plotted as density. Note use of "pars =" to select a single parameter 
plot(fit, show_density = TRUE, pars = "tau", ci_level = 0.95, outer_level = 0.999)

# Visualize MCMC chains. Look for complete mixing.
traceplot(fit)

##############################################################
### Example: simple linear regression ###
## Data = sea ice extent in Northern hemisphere, 1979-2017 ##
dat <- read.csv(here::here("data", "Lect19_seaice.csv"))

# Plot the data
dat %>% ggplot(aes(x = year, y = extent_north)) +
        geom_point()

### Stan program ###
## Prepare input data for Stan
# Stan requires data as lists

# Let's rescale year to years since 1979
x <- I(dat$year - 1978)
y <- dat$extent_north
N <- length(dat$year)
  
sea_data <- list(N = N, x = x, y = y)

## Write Stan model
# 1) data
  # Declare the data types and dimensions

# 2) Parameters
  # Indicate parameters to be fit

# 3) Model
  # Contains priors. Here we have not specified any priors. The default prior is uniform(-inf, +inf)
  # Note that this Stan model statement contains no priors. Without specifying otherwise, Stan applies the default prior (uniform(-Inf, Inf) as the prior to all parameters.

seamod <- "
data {
 int < lower = 0 > N; // Sample size
 vector[N] x; // Predictor
 vector[N] y; // Response
}

parameters {
 real alpha; // Intercept
 real beta; // Slope (regression coefficient)
 real < lower = 0 > sigma; // Error scale constrained to greater than 0
}

model {
 y ~ normal(alpha + x * beta , sigma); // likelihood
}

generated quantities {
} // The posterior predictive distribution"

fit <- stan(model_code = seamod, data = sea_data, warmup = 500, iter = 10000, chains = 4, cores = 4, thin = 1)


# Visualize MCMC chains. Look for complete mixing.
traceplot(fit)

## Posteriors ##
print(fit)

## Summarize posteriors for each chain
summary(fit)

# Dot & whisker plot of posteriors
plot(fit, ci_level = 0.95, outer_level = 0.999)

# Posterior densities
plot(fit, show_density = TRUE, ci_level = 0.95, outer_level = 0.999)

# Posterior density of beta
plot(fit, show_density = TRUE, pars = "beta", ci_level = 0.95, outer_level = 0.999)

## Fit model with normal (but still diffuse) priors ##
seamod_norm <- "
data {
 int < lower = 0 > N; // Sample size
 vector[N] x; // Predictor
 vector[N] y; // Response
}

parameters {
 real alpha; // Intercept
 real beta; // Slope (regression coefficient)
 real < lower = 0 > sigma; // Error scale constrained to greater than 0
}

model {
 alpha ~ normal(0, 10); // Prior on intercept
 beta ~ normal(0, 10); // Prior on slope
 y ~ normal(alpha + x * beta , sigma); // likelihood
}

generated quantities {
} // The posterior predictive distribution"

## !!Always plot the priors before proceeding!! ##
curve(dnorm(x, mean = 0, sd = 10), xlim = c(-30,30))

fit.norm <- stan(model_code = seamod_norm, data = sea_data, warmup = 500, iter = 10000, chains = 4, cores = 2, thin = 1)

# Visualize MCMC chains. Look for complete mixing.
traceplot(fit.norm)

## BIOL 680 students: Try setting a stronger prior on one or both parameters. ##

### Graphical comparisons of modeled parameters ###
## Plot result of OLS lm
mod.lm <- lm(extent_north ~ I(year-1978), data = dat)

post1 <- rstan::extract(fit)
post2 <- rstan::extract(fit.norm)

plot(y ~ x, pch = 20)

# Plot results of first 500 samples from the posterior
for (i in 1:500) {
  abline(post1$alpha[i], post1$beta[i], col = "gray", lty = 1)
}

# Add mean of posterior from fit (uniform priors)
abline(mean(post1$alpha), mean(post1$beta), col = 6, lw = 2)

# Add mean of posterior from fit.norm (normally distributed priors)
abline(mean(post2$alpha), mean(post2$beta), col = 7, lw = 2)

# Add lm fit
abline(mod.lm, col = 2, lty = 2, lw = 3)

### GLM example using brms ###
## Plant species richness at Toolik
## Data
dat <- read.csv(here::here("data", "toolik_richness.csv"))

rich.pl <- dat %>% filter(Treatment == "CT") %>%
                      ggplot(aes(x = Year, y = Richness, color = as.factor(Block))) +
                        geom_point()

## Check automated priors
get_prior(Richness ~ I(Year-2007), family = 'poisson', data = dat)
pp_check(rich.df)

## Fit model using default priors
rich.df <- brm(bf(Richness ~ I(Year-2007),
                        family = brmsfamily('poisson')), data = dat,
                     iter = 1000,
                     chains = 4, cores = 4)

## Extract annotated Stan code
# This helps you gain understanding of Stan syntax. brms can handle many common model structures, but you will need to use Stan for more complex models.
stancode(rich.df)

## Check posteriors
summary(rich.df)
plot(rich.df)

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

## Plot fitted model
plot(conditional_effects(rich.df.norm), points = TRUE)

## Bayesian R2
bayes_R2(rich.df.norm)
