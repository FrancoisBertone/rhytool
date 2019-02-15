#' Class "gfamodel"
#'
#' @description This class is used to reprent the groundwater flow analytical models.
#'
#'@section Slots:
#'  \describe{
#'    \item{\code{model}:}{Matrix of class \code{"character"}, containing the keyword reprensenting the analytical model.}
#'    \item{\code{description}:}{Object of class \code{"character"}, containing the detailed description of the analytical model.}
#'    \item{\code{aspar}:}{Object of class \code{"data.frame"}, containing the list of the analytical parameters and the corresponding numerical values (when defined).}
#'    \item{\code{aqpar}:}{Object of class \code{"data.frame"}, containing the list of the aquifer parameters and the corresponding numerical values (when defined).}
#'    \item{\code{testdata}:}{Object of class \code{"data.frame"}, containing the list of the test data and the corresponding numerical values (when defined).}
#'  }
#' @name gfamodel-class
#' @exportClass gfamodel
#' @author François Bertone
#' @importFrom methods setClass
setClass("gfamodel",
         representation(model="character",description="character",
                        aspar="data.frame",
                        aqpar="data.frame",
                        testdata="data.frame"),
         prototype = list(model=c(),description=c(),aspar=c(),aqpar=c(),testdata=c())
        )

#         , validity = function(object){
#if(!is.element(object@model, c("ths","thc","thn","htj","blt","grg","war"))) {  #,pcw, pca, pcb
#  return("Unknown model")
#}
#return(TRUE)
#}
