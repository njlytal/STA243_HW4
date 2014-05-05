#######################
###### PROBLEM 1 ######

setwd("~/Google Drive/STA 243/HW4")

# *** A ***
# Evaluate an integral x^2 dx from 0 to 1

func.a = function(samps)
{
    # Use f(x) = 1 = Unif(0,1)
    # and h(x) = x^2, where x ~ f(x)
    x = runif(samps, 0 , 1) 
    sample = (x)^2
    
    out = (1/samps)*sum(sample)
    out
}
func.a(1e6)

# *** B ***


# TAKE f(x,y) = 1/4, then take Uniform density for each int bounds.
# Since 1/4 * (4)*(1) = 1
# h(x,y) = 4*(x^2)*cos(x*y)

func.b = function(samps)
{
    x <- runif(samps, -2, 2)
    y <- runif(samps, 0, 1)
    sample = 4*(x^2)*cos(x*y)
    out = (1/samps)*sum(sample)
    out
}
func.b(1e6)


# *** C ***

# NOTE: Should get about 2.275, but currently gives ~ 7.5
# Not sure how to fix this yet...


# NOTE: Try with Exp(1) or with Gamma(5, 1) instead

GOAL: (3*gamma(5)/2^7) * exp(-(x^3)/4) + 2x)

# Splits expression into f(x) = Gamma density and h(x) = the rest
func.c <- function(samps)
{
    browser()
    # Use f(x) = ((1/4)^5)/gamma(5) * x^4*exp(-x/4) ~ Gamma(5, 0.25)
    # Use h(x) = (gamma(5))/((1/4)^5) * exp(-x^2/4)
    x <- rgamma(samps, 5, 0.25)
    sample <- (3/4) * (gamma(5))/((1/4)^5) * exp(-(x^2)/4))
    out <- (1/samps)*sum(sample)
    out
}

func.c(1e6)
