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
func.a <- function(n = 1500)
{
    x <- runif(n, 0, 1)
    sample <- (1/(1+x))
    out <- (1/n)*sum(sample)
    out
}

func.a()

test = numeric(1e4)
for(i in 1:1e4)
{
    test[i] = func.a()
}





# Result: About 0.696.
# Actual: ln(2) = 0.693 - Seems to work fine

# *** B ***
# c(x) = (1+x) is a control variate

# Estimate I using...
# I_CV = (1/n) sum(h(Ui)) - b[(1/n)sum(c(Ui)) - E(c(U))]
# Must analytically calculate E(c(U)) and optimize for b
# Use Mon 5/5 Class notes to find b, then use this.

mu_MC = log(2) # E[h(x)]
theta_MC = 1.5 # E[c(Y)]

variance = function(n = 1500, avg)
{
    x = runif(n, 1, 2)
    out = (1/(n-1))*sum((x - avg)^2)
    out
}

var.c.y <- variance(1500, 1.5)

var.cov <- function(n, h.avg, c.avg)
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

b = v.c[2]/v.c[1]



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


# *** C ***

# There's a particular process for estimating variance, detailed in notes
# May want to review/ask about this--I don't understand it quite yet.

# *** D ***
# Design a new estimator with smaller variance?

# We can do this by selecting MORE THAN ONE covariate!