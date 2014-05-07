#######################
###### PROBLEM 2 ######

# STATUS: Need to test each g(x), but main procedure is clear
# ************************************************************
# Need to review how h(x), f(x), and g(x) go together

# If estimating mu = Int(h(x) * f(x) dx), we use
# mu = Int(h(x) * (f(x)/g(x)) * g(x) dx), where
# g is a PDF such that g(x) > 0 whenever f(x) > 0

# In general
# f(x) <-
# h(x) <- indicator function

# Either sample conditionally in the given int. range 
# OR change all samples out of the given int. range to be 0


# So we sample from g(x) and place the resulting samples in
# the expression h(x)*f(x)/g(x)

# For nu <- 0.1

# g(x) <- N(1.5, 0.1^2)

IS2 <- function(n, nu)
{
    g.x = rnorm(n, 1.5, nu^2)
    for(i in 1:n)
    {
        if(g.x[i] > 2 | g.x[i] < 1)
        {
            g.x[i] = 1e6
        }
    }
    g.x = g.x[which(g.x != 1e6)]
    out = nu*exp(((g.x-1.5)^2)/(2*nu^2) - ((g.x^2)/2))
    out
}

nu1 = IS2(100, 0.1)
nu2 = IS2(100, 1)
nu3 = IS2(100, 10)


hist(nu1)
hist(nu2)
hist(nu3)


# Our goal should be ~ 0.136, but we're not seeing that yet...
