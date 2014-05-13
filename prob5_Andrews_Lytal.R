#######################
###### PROBLEM 5 ######

# STATUS: Need to find part B (simple)
# Can't proceed without knowing Gibbs Sampler
# *******************************************************

# *** A ***

# NOTE: This just generates x_i for the Joint Distribution
# We'll need a different function for part c.

func.a <- function(n = 100, p = 0.3, lam = 2)
{
    r <- rbinom(n, 1, p)
    x <- rpois(n, lam*r)
    x
}
x = func.a()

plot(density(func.a()), main = "Random Sample for ZIP Model, p = 0.3, lambda = 2")
# *** B ***
# This is strictly Calculus, so no need for programming.

# *** C ***
# Need to calculate posterior before we can proceed

func.c <- function(samps = 1e5, n = 100, a = 1, b = 1, p.start = 0.3, lam.start = 2)
{
    # browser()
    x <- func.a(n = n, p = p.start, lam = lam.start)
    lam <- numeric(samps) # Records values of lam estimated in all iterations
    p <- numeric(samps) # Records values of p estimated in all iterations
    r <- numeric(n) # Records the r used in the current iteration
    
    # Initialize values
    p[1] = p.start
    lam[1] = lam.start
    r <- rbinom(n, 1, p.start)
    for(i in 2:samps)
    {   
        lam[i] = rgamma(1, a + sum(x), b + sum(r)) # Conditional of lambda
        p[i] = rbeta(1, 1 + sum(r), n + 1 - sum(r)) # Conditional of p
        # Conditional of r given the above newly generated lambda and p
        r = rbinom(n, 1, (p[i]*exp(-lam[i])) / ((p[i])*exp(-lam[i]) + (1-p[i])*(x == 0)))
    }
    data.frame(lam, p)
}

# This seems to work...
results = func.c(samps = 1e5)

plot(density(results[,1]), main = "Lambda Density")
plot(density(results[,2]), main = "P Density")
quantile(results[,1], c(0.025,0.975)) # (1.44517, 2.529160)
quantile(results[,2], c(0.025,0.975)) # (0.2650107, 0.4858813)
