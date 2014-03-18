## Create the data directory and save the example data set under the directory
set.seed(0520) # Set a seed for reproduction
myY <- matrix(sample(1:20, 50, replace=TRUE), 50, 1) # A 50 by 1 vector of outcome variable
myX <- matrix(c(runif(50), runif(50), rnorm(50)), 50, 3) # A 50 by 3 matrix of covariates
regressionsPackData <- cbind(myY,myX)
colnames(regressionsPackData) <- c("Y","X1","X2","X3")
dir.create(path="./regressionsPack/data") # Create the data directory under the package
save(regressionsPackData, file="./regressionsPack/data/regressionsPackData.rda") # Save a rda. file


## Create the demo directory 
dir.create(path="./regressionsPack/demo")
