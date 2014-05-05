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


# *** C ***

# NOTE: Should get about 2.275, but currently gives ~ 7.5
# Not sure how to fix this yet...

# Splits expression into f(x) = Gamma density and h(x) = the rest
func.c <- function(samps)
{
    # Use f(x) = ((1/4)^5)/gamma(5) * x^4*exp(-x/4) ~ Gamma(5, 0.25)
    # Use h(x) = (gamma(5))/((1/4)^5) * exp(-x^2/4)
    x <- rgamma(samps, 5, 0.25)
    sample <- (3/4) * (gamma(5))/((1/4)^5) * exp(-(x^2)/4)
    out <- (1/samps)*sum(sample)
    out
}

func.c(1e6)
