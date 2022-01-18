### Lab 2: Probability distribution/density functions ###

## Inputs:
# Lab02data.csv: assignment data


library(here)
library(moments)

### Density functions (d) ###
## returns density (height) of probability distribution at specified value. 

# Default parameters vary by distribution. Default for normal: mean=0 and SD=1.
dnorm(0) 

dnorm(0, mean=10, sd=20) 
#returns density of probability distribution at 0, with specified mean and sd

### Probability functions (p) ###
## Returns probability (area under the probability density curve) that the value of a random variable is *less* than the supplied number. 

pnorm(0) 
# As with dnorm, when no additional arguments are supplied, R assumes mean=0 and SD=1 for a normal distribution

pnorm(0, mean=10, sd=20) 
#returns probability that the value of a random variable is less than 0 when drawn from a normal distribution defined by mean=10 and SD=20

# Lower tail (probability < supplied value) is the default. For probability of a value greater than the supplied value, set lower.tail = FALSE
pnorm(0, mean=10, sd=20, lower.tail = FALSE) 

### Quantiles (q) ###
## Returns observation value at the specified probability, determined by the cumulative distribution function. Inverse of pnorm. 
qnorm(0.01) 

qnorm(0.01, mean=10, sd=20)

### Random numbers (r) ###
## Generates vector of specified length drawn randomly from the specified distribution.
rnorm(10) 


### Visualizing distributions ###

## Probability density curve ##
## Densities from specified distribution plotted across a discrete range of observations

# Create a vector including all values from -20 to 20 by increments of 0.1
x <- seq(-20, 20, by=0.1) 

# Calculate density at each x from specified distribution
y <- dnorm(x) 

# Probability density curve
plot(x,y) 

# Specify the mean and standard deviation 
z <- dnorm(x, mean=5, sd=1)
plot(x, z)

## Alternative way to visualize probability density curves
curve(dnorm(x, 5,1), 
      xlim = c(-20, 20))

## Cumulative distribution function ##
## Cumulative distribution function across values specified by x
# Generate vector of probabilities  
y <- pnorm(x)
plot(x,y) 

## Quantile plot ##
## Quantiles for a vector of probabilityes
p <- seq(0,1, by = 0.05)
y <- qnorm(p) 
plot(p,y)

y <- qnorm(p, mean=5, sd=1)
plot(p,y)

## (Normal) probability plot ##
# Values specified by the vector y are plotted against expected values drawn from a normal distribution with mean and standard deviation of vector y.
qqnorm(x) 

# Add a 1:1 line to the normal probability plot. Examining this plot is useful for determining whether a sample was drawn from a normal distribution.
qqline(x) 

### Histograms ###
## Base R version
# Generate data
a <- rnorm(100, mean=10, sd=20)

# Breaks controls the bin width. xlim sets the upper and lower bounds of the x-axis
hist(a, xlim=c(-100, 100), breaks = 20) 

## ggplot function for histogram
# Requires input data as a data.frame
aa <- data.frame(cbind(matrix(a), matrix(a)))

ggplot(aa, aes(x = X1)) + 
  geom_histogram(binwidth = 5)

### Skew & kurtosis ###
## using "moments" package
b <- rnorm(30, mean = 10, sd = 1)
skewness(b)
kurtosis(b)

d <- rpois(30, lambda = 10)
skewness(d)
kurtosis(d)
