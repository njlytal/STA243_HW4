#######################
###### PROBLEM 3 ######

# Let I = 1/sqrt(2*pi) int[1-2](exp(-(x^2)/2) dx)

# *** A ***

# Let h(x) = (1/(1+x)), U1,...,Un ~IID~ Unif[0,1]
# Estimate I using...
# I_MC = (1/n) sum(h(Ui)), n = 1500

# n = samples to take
func.a <- function(n = 1500)
{
    x <- runif(n, 0, 1)
    sample <- (1/(1+x))
    out <- (1/n)*sum(sample)
    out
}

func.a()
# Result: About 0.696.
# Actual: ln(2) = 0.693 - Seems to work fine

# *** B ***
# c(x) = (1+x) is a control variate

# Estimate I using...
# I_CV = (1/n) sum(h(Ui)) - b[(1/n)sum(c(Ui)) - E(c(U))]
# Must analytically calculate E(c(U)) and optimize for b
# Use Mon 5/5 Class notes to find b, then use this.

func.b <- function(n <- 1500, b <- 1)
{
    x <- runif(n, 0, 1)
    h.x <- (1/(1+x))
    c.x <- (1+x)
    e.c.u <- 1.5 # (Ranges from 1+0 to 1+1 with even prob, so 1.5)
    
    out <- (1/n)*sum(h.x) - b*((1/n)*sum(c.x) - e.c.u)
    out
}

func.b()

# Gives a similar range, but I fail to see how this helps at all.
# It seems to merely add more variability to the expression.

# *** C ***

# There's a particular process for estimating variance, detailed in notes
# May want to review/ask about this--I don't understand it quite yet.

# *** D ***
# Design a new estimator with smaller variance?

# We can do this by selecting MORE THAN ONE covariate!