#######################
###### PROBLEM 5 ######

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
abline(v = 2, col = "red")
plot(density(results[,2]), main = "P Density")
abline(v = 0.30, col = "red")
quantile(results[,1], c(0.025,0.975)) # (1.44517, 2.529160)
quantile(results[,2], c(0.025,0.975)) # (0.2650107, 0.4858813)

values <- c(0.5, 1, 1.5, 2, 5, 10)
a <- rep(values, rep(6, 6))
b <- rep(values, 6)
all.values <- cbind(a, b)

new.results <- sapply(1:nrow(all.values), function(i) func.c(samps = 1e5, n = 100, a = all.values[i, 1], b = all.values[i, 2], p.start = 0.3, lam.start = 2))

confidence.intervals <- lapply(1:ncol(new.results), function(i) cbind(rep(all.values[i, 1],1), rep(all.values[i, 2], 1), cbind(quantile(new.results[, i][[1]], c(0.025,0.975))[1],quantile(new.results[, i][[1]], c(0.025,0.975))[2],quantile(new.results[, i][[2]], c(0.025,0.975))[1], quantile(new.results[, i][[2]], c(0.025,0.975))[2])))

confidence.intervals <- do.call(rbind, confidence.intervals)
confidence.intervals <- cbind(confidence.intervals, (confidence.intervals[, 3] < 2 & confidence.intervals[, 4] > 2), (confidence.intervals[, 5] < 0.30 & confidence.intervals[, 6] > 0.30))
rownames(confidence.intervals) = NULL
colnames(confidence.intervals) = c("a", "b", "lambda 2.5%", "lambda 97.5%", "p 2.5%", "p 97.5%", "2 in ci", "0.3 in ci")


