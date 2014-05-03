#######################
###### PROBLEM 1 ######

setwd("~/Google Drive/STA 243/HW4")

# *** A ***
# Evaluate an integral x^2 dx from 0 to 1



func.a = function(samps)
{
    # Use f(x) = 1 = Unif(0,1)
    # and h(x) = x^2, where x ~ f(x)
    x = runif(samps, 0 , 1) 
    sample = (x)^2
    
    out = (1/samps)*sum(sample)
    out
}
func.a(1e6)

# *** B ***


# *** C ***

func.c = function(samps)
{
    # Use f(x) = ((x^2)/4)*exp(-x *((x^2)/4)) ~ Exp((x^2)/4)
    # Use h(x) = 3(x^2)
    x = rexp(samps,(runif(1,-2,2)^2)/4)
    sample = 3*(x.b^2)
    out = (1/samps)*sum(sample)
    out
}

func.c(1e6)

