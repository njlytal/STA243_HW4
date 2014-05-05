#######################
###### PROBLEM 6 ######

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
# f(x) = that nasty thing in the problem

f.x <- function(z, t1, t2)
{
    (z^-(3/2))*exp(-t1*z - (t2/z) + 2*sqrt(t1*t2) + log(sqrt(2*t2)))
}

# This is possibly something different, I'm just not sure what
g.x <- function(x, t1, t2)
{
    pgamma(x, t1, t2)
}

# Independent Metropolis-Hastings Algorithm
# n = sample size
# t1 = theta1
# t2 = theta2

IMH <- function(n = 1000, t1, t2)
{
    x <- numeric(n)             # Define vector to contain samples
    x[1] <- rgamma(1, t1, t2)   # Define first value of sample
    
    for(i in 2:n)
    {
        z = x[i-1]              # This is x(t), the previous x value
        y = rgamma(1, t1, t2)   # This is Y_t, the potential NEW value
        
        # Now define cutoff and probability of acceptance
        # u must be less than p.accept to accept a new value
        u = runif(1, 0, 1)
        p.accept = min( (f.x(y,t1,t2)*g.x(z,t1,t2))/(f.x(z,t1,t2)*g.x(y,t1,t2)), 1) 
        if(u < p.accept)
        {
            x[i] = y # Accept the new value
        }else{
            x[i] = z # Keep the old value
        }
    }
    x
}

test = numeric(100)
for(i in 1:100)
{
    test[i] = mean(IMH(t1 = 1.5, t2 = 2))
}
mean(test)
---

test = IMH(n = 1e5, t1 = 1.5, t2 = 2)
mean(test)



target.Ez = sqrt(t2/t1)
target.E1z = sqrt(t1/t2) + (1/(2*t2))


# NOTE: Currently yields about 0.722 for E(Z), when
# it should really be more like 1.155.