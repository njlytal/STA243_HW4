#######################
###### PROBLEM 1 ######

# STATUS: FINISHED (may want to modify 1c, but it works)

setwd("~/Google Drive/STA 243/HW4")

# *** A ***
# Evaluate an integral x^2 dx from 0 to 1
# Should produce (1/3)...and it does!

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

# Should produce ~3.483...and it does!

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

# NOTE: Should get about 2.275...and it does!
# But the form is a bit awkward, and there's a LOT of variability.
# This is due to the huge range of the intergral (0 to infinity)
# Consider transforming x and changing integral bounds?

# OH Solutions (may not use): h(x) =  (3*gamma(5)/2^7) * exp(-(x^3)/4 + 2x)
# This is when you try with Exp(1) or with Gamma(5, 1) instead

# Splits expression into f(x) = Gamma density and h(x) = the rest
func.c <- function(samps)
{
    # Use f(x) = ((1/4)^5)/gamma(5) * x^4*exp(-x/4) ~ Gamma(5, 0.25)
    # Use h(x) = (3/4) * (gamma(5))/((1/4)^5) * exp((x/4) - (x^3/4))
    x <- rgamma(samps, 5, 0.25)
    sample <- (3/4) * (gamma(5)/((0.25)^5)) * exp((x/4) - (x^3/4))

    out <- (1/samps)*sum(sample)
    out
}

test = numeric(100)
for(i in 1:100)
{
    test[i] = func.c(5e5)
}
plot(density(test))
abline(v = 2.275)

func.c(1e6)
hist(test)
