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

func.b = function(samps, size)
{
    

}