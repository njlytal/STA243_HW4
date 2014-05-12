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

# In the terms given in Problem 6...
# g(x) = Gamma(t1, t2)
# f(x) = Inv. Gaussian given

f <- function(z, t1, t2)
{
    (z^-(3/2))*exp(-t1*z - (t2/z) + 2*sqrt(t1*t2) + log(sqrt(2*t2)))
}

g <- function(x, s1, s2)
{
    dgamma(x, s1, s2)
}

# Independent Metropolis-Hastings Algorithm
# n = sample size
# t1 = variable 1 proposal density
# t2 = variable 2 for proposal density
# theta1 and theta2 are fixed at 1.5 and 2 for all cases of f.x()

IMH <- function(n = 1000, t1, t2)
{
    set.seed(0)
    samps <- numeric(n)             # Define vector to contain samples
    samps[1] <- rgamma(1, t1, t2)   # Define first value of sample
    
    for(i in 2:n)
    {
        x = samps[i-1]              # This is x(t), the previous x value
        y = rgamma(1, t1, t2)   # This is Y_t, the potential NEW value
        
        # Now define cutoff and probability of acceptance
        # u must be less than p.accept to accept a new value
        u = runif(1, 0, 1)
        p.accept = min( (f(y,1.5,2)*g(x,t1,t2))/(f(x,1.5,2)*g(y,t1,t2)), 1) 
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

test = numeric(100)
for(i in 1:100)
{
    test[i] = mean(IMH(t1 = 1.5, t2 = 2))
}


mean(test)
---

test = IMH(n = 1000000, t1 = 1, t2 = 1)

mean(test)
mean(1/test)

plot(density(test), main = "Independence - Metropolis-Hastings with Gamma Proposed Density")

target.Ez = sqrt(2/1.5)
target.E1z = sqrt(1.5/2) + (1/(2*2))


# Try with MANY gamma combos: (1,1) and (1,0.5) work best


optMeans <- function(theta.1, theta.2){
	E.z <- sqrt(theta.2/theta.1)
	E.inv.z <- sqrt(theta.1/theta.2) + 1/(2*theta.2)
	cbind(E.z = E.z, E.inv.z = E.inv.z)
}

# Test all possible combinations of theta.1 and theta.2 where each can equal 0.5, 1, 1.5, 2, 5, 7, 10

values <- c(0.5, 1, 1.5, 2, 5, 7, 10)

theta.1 <- rep(values, rep(7, 7))
theta.2 <- rep(values, 7)
all.values <- cbind(theta.1, theta.2)

results <- matrix(ncol = 6, nrow = nrow(all.values))
for(i in 1:nrow(all.values)){
	t1 <- all.values[i, 1]
	t2 <- all.values[i, 2]
	temp <- IMH(n = 1000, t1 = t1, t2 = t2)
	E.z <- mean(temp)
	target.E.z <- sqrt(t2/t1)
	E.inv.z <- mean(1/temp)
	target.E.inv.z <- sqrt(t1/t2) + 1/(2*t2)
	abs.diff.E.z <- abs(E.z - target.E.z)
	abs.diff.E.inv.z <- abs(E.inv.z - target.E.inv.z)
	results[i, ] <- c(E.z, target.E.z, abs.diff.E.z, E.inv.z, target.E.inv.z, abs.diff.E.inv.z)
}

colnames(results) <- c("E.z", "target.E.z", "abs.diff.E.z", "E.inv.z", "target.E.inv.z", "abs.diff.E.inv.z")

results <- cbind(all.values, results)


