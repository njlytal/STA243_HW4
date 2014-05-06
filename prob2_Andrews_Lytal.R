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

# For nu <- 0.1

# g(x) <- N(1.5, 0.1^2)
