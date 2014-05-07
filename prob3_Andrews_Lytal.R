#######################
###### PROBLEM 3 ######

# STATUS: Need to find optimal b, rest is done!
# **************************************************

# Let I = 1/sqrt(2*pi) int[1-2](exp(-(x^2)/2) dx)

# *** A ***

# Let h(x) = (1/(1+x)), U1,...,Un ~IID~ Unif[0,1]
# Estimate I using...
# I_MC = (1/n) sum(h(Ui)), n = 1500

# n = samples to take
# This estimates I_MC
func.a <- function(n = 1500)
{
    x <- runif(n, 0, 1)
    sample <- (1/(1+x))
    out <- (1/n)*sum(sample)
    out
}


# Result: About 0.696.
# Actual: ln(2) = 0.693 - Seems to work fine


# This tests the variance of func.a
# We want to improve on this in subsequent parts
test.a = numeric(1e4)
for(i in 1:1e4)
{
    test.a[i] = func.a()
}

var(test.a)




# *** B ***
# c(x) = (1+x) is a control variate

# Estimate I using...
# I_CV = (1/n) sum(h(Ui)) - b[(1/n)sum(c(Ui)) - E(c(U))]
# Must analytically calculate E(c(U)) and optimize for b
# Use Mon 5/5 Class notes to find b, then use this.

# mu_MC = log(2) # E[h(x)]
# theta_MC = 1.5 # E[c(Y)]

# Estimates the variance and covariance of h(X) and c(Y)
# n = samples
# h.avg = E[h(X)] = log(2)
# c.avg = E[c(Y)] = 1.5
var.cov <- function(n, h.avg = log(2), c.avg = 1.5)
{
    browser()
    x <- runif(n, 0, 1)
    h <- (1/(1+x))
    
    c <- (1+x)
    
    var = (1/(n-1))*sum((x - c.avg)^2)
    #cov = cov(h,c)
    cov = (1/((n-1)))*sum((h - h.avg)*(c - c.avg))
    out = data.frame(var, cov)
    out
}

v.c = var.cov(1500, log(2), 1.5)

# The optimal b is Cov(h(X),c(Y))/Var(c(Y))
b = v.c[2]/v.c[1]

# This estimates I_CV
# n = samples
# b = covariate value determined previously
func.b <- function(n = 1500, b)
{
    x <- runif(n, 0, 1)
    h.x <- (1/(1+x))
    c.x <- (1+x)
    e.c.u <- 1.5 # (Ranges from 1+0 to 1+1 with even prob, so 1.5)
    
    out <- (1/n)*sum(h.x) - b*((1/n)*sum(c.x) - e.c.u)
    out
}

func.b(1500, b)

# Tests the variance of I_CV
test.b = numeric(1e4)
for(i in 1:1e4)
{
    test.b[i] = func.a()
}

var(test.b)


# *** C ***

# There's a particular process for estimating variance, detailed in notes
# May want to review/ask about this--I don't understand it quite yet.

# *** D ***
# Design a new estimator with smaller variance?

# We can do this by selecting MORE THAN ONE covariate!