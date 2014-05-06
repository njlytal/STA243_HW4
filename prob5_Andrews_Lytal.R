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
func.a()


# *** B ***
# This is strictly Calculus, so no need for programming.

# *** C ***
# Need to calculate posterior before we can proceed

func.c <- function()
{
    
    
}