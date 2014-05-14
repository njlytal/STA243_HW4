#######################
###### PROBLEM 6 ######

# STATUS: ALMOST FINISHED: Just test different thetas!
# *******************************************************

# Given x(t):
# 1) Generate Y_t ~ g(y)
# 2) Take:

# x(t+1) = Y_t with prob. min((f(Y_t)*g(x(t))) / (f(x(t))*g(Y_t)), 1)
#        = x(t) otherwise

# Proposal density = Gamma(t1, t2)

t1 = 1.5
t2 = 2


# Inverse Gaussian
f <- function(z, t1, t2)
{
    (z^-(3/2))*exp(-t1*z - (t2/z) + 2*sqrt(t1*t2) + log(sqrt(2*t2)))
}

# Proposal density (Gamma)
g <- function(x, s1, s2)
{
    dgamma(x, s1, s2)
}

# Independent Metropolis-Hastings Algorithm
# n = sample size
# t1, t2: The current theta values we're testing
# a = variable 1 proposal density
# b = variable 2 for proposal density
# theta1 and theta2 are fixed at 1.5 and 2 for all cases of f.x()

IMH <- function(n = 1000, t1, t2, a, b)
{
    set.seed(0)
    samps <- numeric(n)             # Define vector to contain samples
    samps[1] <- rgamma(1, a, b)   # Define first value of sample from proposal density
    
    for(i in 2:n)
    {
        x = samps[i-1]              # This is x(t), the previous x value
        y = rgamma(1, a, b)         # This is Y_t, the potential NEW value
        
        # Now define cutoff and probability of acceptance
        # u must be less than p.accept to accept a new value
        u = runif(1, 0, 1)
        p.accept = min( (f(y,t1,t2)*g(x,a,b))/(f(x,t1,t2)*g(y,a,b)), 1) 
        # p.accept = min( (f(y,t1,t2)*g(x,t1,t2))/(f(x,t1,t2)*g(y,t1,t2)), 1) 
        if(u < p.accept)
        {
            samps[i] = y # Accept the new value
        }else{
            samps[i] = x # Keep the old value
        }
    }
    samps
}


---

# test = IMH(n = 1e6, t1 = 1, t2 = 1)
test = IMH(n = 1e4, t1 = 1, t2 = 2, a = 1, b = 2)
    
mean(test)
mean(1/test)

plot(density(test), main = "Independence - Metropolis-Hastings with Gamma Proposed Density")

target.Ez = sqrt(2/1)
target.E1z = sqrt(1/2) + (1/(2*2))

abs(mean(test) - target.Ez)
abs(mean(1/test) - target.E1z)
# Try with MANY gamma combos: (1,1) and (1,0.5) work best


optMeans <- function(theta.1, theta.2){
	E.z <- sqrt(theta.2/theta.1)
	E.inv.z <- sqrt(theta.1/theta.2) + 1/(2*theta.2)
	cbind(E.z = E.z, E.inv.z = E.inv.z)
}

# Test all possible combinations of theta.1 and theta.2 where each can equal 0.5, 1, 1.5, 2, 5, 7, 10

values <- c(0.5, 1, 1.5, 2, 2.5, 3, 4, 5)

a <- rep(values, rep(8, 8))
b <- rep(values, 8)
all.values <- cbind(a, b)

results <- matrix(ncol = 6, nrow = nrow(all.values))
for(i in 1:nrow(all.values)){
	a <- all.values[i, 1]
	b <- all.values[i, 2]
	temp <- IMH(n = 1000, t1 = 1.5, t2 = 2, a = a, b = b)
	E.z <- mean(temp)
	target.E.z <- sqrt(2/1.5)
	E.inv.z <- mean(1/temp)
	target.E.inv.z <- sqrt(1.5/2) + 1/(2*2)
	abs.diff.E.z <- abs(E.z - target.E.z)
	abs.diff.E.inv.z <- abs(E.inv.z - target.E.inv.z)
	results[i, ] <- c(E.z, target.E.z, abs.diff.E.z, E.inv.z, target.E.inv.z, abs.diff.E.inv.z)
}

colnames(results) <- c("E.z", "target.E.z", "abs.diff.E.z", "E.inv.z", "target.E.inv.z", "abs.diff.E.inv.z")

results <- cbind(all.values, results)

results[which.min(results[, 5]), ]
results[which.min(results[, 8]), ]
