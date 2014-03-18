## Load libraries and set working directory
library(devtools)
library(roxygen2)
setwd("~/GitHub/RPackageProblemSet") # Set my working directory

## At this point put the *.R files into the correct directories and edit the DESCRIPTION file

## Ensure that the R directory be in this order:

# regressionsPack.r
# Regressions.R
# runReg.R
# testReg.R
# MoreRegressions.R
# moreReg.R

## To do that, I added numbers to the file name such that

# 1regressionsPack.r
# 2Regressions.R
# 3runReg.R
# 4testReg.R
# 5MoreRegressions.R
# 6moreReg.R
# 7exampleData.R


## This can be run many times as the code is updates
current.code <- as.package("regressionsPack")
load_all(current.code) # Load all of the functions so you can use them
document(current.code) # Make the help files
check(current.code) # Run the R checks
install(pkg=current.code, local=TRUE) # Install the package








