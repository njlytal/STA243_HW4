#######################
###### PROBLEM 2 ######

# ************************************************************
# If estimating mu = Int(h(x) * f(x) dx), we use
# mu = Int(h(x) * (f(x)/g(x)) * g(x) dx), where
# g is a PDF such that g(x) > 0 whenever f(x) > 0

# Either sample conditionally in the given int. range 
# OR change all samples out of the given int. range to be 0

# So we sample from g(x) and place the resulting samples in
# the expression h(x)*f(x)/g(x)

# For nu <- 0.1

# g(x) <- N(1.5, 0.1^2)


IS2 <- function(n, nu)
{
    g.x = rnorm(n, 1.5, nu^2) # Sample from g(x)
    # for(i in 1:n)
    # {
        # # If any values lie outside the integral range...
        # # Set as NA for future removal
        # if(g.x[i] > 2 | g.x[i] < 1)
        # {
            # g.x[i] = NA
        # }
    # }
    # g.x = g.x[!is.na(g.x)] # Removes values outside the int. range
    # This is h(x)*f(x)/g(x) - plug in values from g.x
    indicator <- (1 < g.x & g.x < 2)+0
    out = nu*exp(((g.x-1.5)^2)/(2*nu^2) - ((g.x^2)/2))*indicator
    out
}

nu1 = IS2(1e6, 0.1)
nu2 = IS2(1e6, 1)
nu3 = IS2(1e6, 10)

# Should be close when nu = 1, and not when the others. With small nu
# and nu3 doesn't work because the effective sample size is small,
# the nu1 does not explore the whole sample space. 

mean(nu1)
mean(nu2)
mean(nu3)

hist(nu1, main = "Values to be Averaged for nu = 0.1", xlab = "nu = 0.1", breaks = 50)
hist(nu2, main = "Values to be Averaged for nu = 1", xlab = "nu = 1", breaks = 50)
hist(nu3, main = "Values to be Averaged for nu = 10", xlab = "nu = 10", breaks = 50)


# We don't actually need to show these, they are not helpful. 
par(mfrow = c(3, 1))
plot(density(nu1))
abline(v = 0.136, col = "red")
plot(density(nu2))
abline(v = 0.136, col = "red")
plot(density(nu3))
abline(v = 0.136, col = "red")

# Our goal should be ~ 0.136, but we're not seeing that yet...




