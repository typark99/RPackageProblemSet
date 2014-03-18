myY <- matrix(sample(1:20, 50, replace=TRUE), 50, 1) # A 50 by 1 vector of outcome variable
myX <- matrix(c(runif(50), runif(50), rnorm(50)), 50, 3) # A 50 by 3 matrix of covariates
moreReg(myY, myX)

myY <- matrix(sample(1:10, 50, replace=TRUE), 50, 1) # A 50 by 1 vector of outcome variable
myX <- matrix(c(runif(50), runif(50), rnorm(50), rnorm(50)), 50, 4) # A 50 by 3 matrix of covariates
moreReg(myY, myX)