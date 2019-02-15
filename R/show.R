#' @describeIn gfamodel Display the object, by printing. Print everay parameters of the gfamodel object, starting with the applicable analytical model type. Also print the analytical model parameters, the aquifer parameters and the test data. When parameters are not yet defined, prints 0 value.
#' @param object A gfamodel class object
#' @importFrom methods setMethod
setMethod(f="show",
          signature="gfamodel",
          definition=function(object) {
            cat(paste0("Model type: ",object@description))
            cat("\nAnalytical model parameters:","\n")
            n <- length(object@aspar$names)
            for(i in 1:n){
              cat(sprintf(" %s: %3.2e",object@aspar$names[i],object@aspar$val[i]),"\n")
            }
            cat("Hydraulic parameters:","\n")
            n <- length(object@aqpar$names)
            for(i in 1:n){
              cat(sprintf(" %s: %3.2e",object@aqpar$names[i],object@aqpar$val[i]),"\n")
            }
            cat("Test data:","\n")
            n <- length(object@testdata$names)
            for(i in 1:n){
              cat(sprintf(" %s: %3.2e",object@testdata$names[i],object@testdata$val[i]),"\n")
            }
          })

#' @describeIn drawdowns Display the object, by printing.
#' @param object A drawdowns class object
#' @importFrom methods setMethod
setMethod(f="show",
          signature="drawdowns",
          definition=function(object) {
            cat("Drawdowns:\nt\n")
            print(object@dd$t)
            cat("s\n")
            print(object@dd$s)
            cat("Derivatives:\ndt\n")
            print(object@ldiff$dt)
            cat("ds\n")
            print(object@ldiff$ds)
          })


