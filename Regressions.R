#' A regression object 
#' 
#' Object of class \code{Regressions} are created by the \code{runReg} and \code{testReg} functions
#'
#' 
#' An object of the class `Regressions' has the following slots:
#' \itemize{
#' \item \code{Y} A depedent variable
#' \item \code{X} Covariates
#' \item \code{output} Output includes coefficients, R^2 values, tStat values, and pValue
#' }
#'
#' @author Taeyong Park: \email{typark99@gmail.com}
#' @aliases Regressions-class initialize,Regressions-method getRegressions,Regressions-method 
#' @rdname Regressions
#' @export
setClass(Class="Regressions", 
         slot = list(
           Y = "matrix", # Input
           X = "matrix", # Input
           output = "list" # Output: This will include coefficients and R.squared for runReg function and tStat, pValue, and sig for testReg function
         ),
         prototype = prototype(
           Y = matrix(nrow=0, ncol=0),
           X = matrix(nrow=0, ncol=0),
           output = list()
         )
)

#' @export
setMethod("initialize", "Regressions", 
          function(.Object, ...){
            value=callNextMethod()
            return(value)
          }
) 

#' @rdname Regressions
#' @export 
setGeneric("getRegressions", 
           function(object="Regressions")  {
             standardGeneric("getRegressions")
           }
)

#' @export
setMethod(f="getRegressions", 
          signature="Regressions", 
          function(object){ 
            return(output=object@output)
          }
) 


#' @export
setMethod(f="print",  # Since print is a built-in function we do not set a generic function, and define the print function used under the Regressions class  
          signature="Regressions",
          definition=function(x){
            cat("*** Start Print (Class-Regressions) ***", "\n", "\n")
            cat("1) Coefficients", "\n")
            for(i in 1:nrow(x@output$coefficients)){  # for loop enables to separate each model 
              cat("Model", i, ":", x@output$coefficients[i,], "\n")
            }
            cat("2) R.squared", "\n")
            for(i in 1:nrow(x@output$coefficients)){ # for loop enables to separate each model 
              cat("Model", i, ":", x@output$R.squared[i], "\n")
            }
            cat("\n","**** End Print (Class-Regressions) ****")
          }
)

#' @export
setMethod(f="plot",  # Since plot is a built-in function we do not set a generic function, and define the plot function used under the Regressions class  
          signature="Regressions",
          definition=function(x){  
            plot(1:nrow(x@output$coefficients), x@output$R.squared, # This will plot the value of r.squared for each model
                 pch="+", col="darkgreen",
                 xlim=c(0.5, nrow(x@output$coefficients)+0.5),
                 xlab="Model (the number of covariates)", ylab="R.squared",
                 main="The Value of R.squared across Models")
            }
)


# "show" is the default method used to show an object when its name is written in the console. 
# We thus define it by taking into account the size of the object: if there are too many trajectories, show post only part of them.
setMethod(f="show",
          signature="Regressions", 
          definition=function(object){
            cat("*** Start Show (Class-Regressions) *** \n", "\n")
            m=2 # This defines the maximum number of rows that we want to deal with 
            nrowShow <- min(m, nrow(object@output$coefficients)) # If nrow(x@output$coefficients) is greater than m, then the output will show only "m" size of object 
            cat("1) Coefficients", "\n")
            for(i in 1:nrowShow){  # for loop enables to separate each model 
              cat("Model", i, ":", object@output$coefficients[i,], "\n")
            }
            cat("2) R.squared", "\n")
            for(i in 1:nrowShow){ # for loop enables to separate each model 
              cat("Model", i, ":", object@output$R.squared[i], "\n")
            }
            cat("\n", "**** End Show (Class-Regressions) **** \n") 
          }
)
