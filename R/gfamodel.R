#' Create gfamodel objects
#'
#' Create objects of class gfamodel, i.e. objects that hold the information relative to a known groundwater flow analytical model
#' @name gfamodel
#' @param model A keyword or a gfamodel object. When a keyword is given, it should be one of a known model from the following list: "ths", "thc", "thn", "htj", "blt", "war" or "grg". See details
#' @param aspar Parameters for the analytical solution for model computation. It can be obtained from a guess fuction, adjusted manually, or obtained from a fit function
#' @param aqpar Aquifer's parameters for the selected model (such as Transmissivity and Storage coefficient for the Theis model). See details
#' @param testdata If provided, information about pumping conditions used for calculating aquifer parameters \verb{aqpar} from analytical solution parameters \verb{aspar} or analytical solution parameters \verb{aspar} from aquifer parameters \verb{aqpar}
#' @details If model is set from keywords, the options to date are:
#' \itemize{
#'     \item "ths" for a Theis (1935) solution. Analytical solution parameters are: \verb{a}, the slope of Jacob Straight Line; and \verb{t0}, intercept with the horizontal axis for s = 0. When the model is constructed from aquifer parameters and test data, \verb{aqpar} should be a vector of 2 numeric (Transmissivity and Storativity), and \verb{testdata} a vector of 2 numerics (Pumping rate and Distance to the pumping well).
#'     \item "thc" for a confined aquifer with a boundary effect using the Theis (1941) solution. Analytical solution parameters are: \verb{a}, the slope of Jacob Straight Line; \verb{t0}, intercept of the first segment of straight line; and \verb{ti} the time of intersection between the 2 straight lines. When the model is constructed from aquifer parameters and test data, \verb{aqpar} should be a vector of 3 numeric (Transmissivity, Storativity, Distance to image well), and \verb{testdata} a vector of 2 numerics (Pumping rate, Distance to the pumping well).
#'     \item "thn" for a confined aquifer with an impermeable boundary with the Theis solution. Analytical solution parameters are: \verb{a}, the slope of Jacob Straight Line; \verb{t0}, intercept of the first segment of straight line; and \verb{ti} the time of intersection between the 2 straight lines. When the model is constructed from aquifer parameters and test data, \verb{aqpar} should be a vector of 3 numeric (Transmissivity, Storativity, Distance to image well), and \verb{testdata} a vector of 2 numerics (Pumping rate, Distance to the pumping well).
#'     \item "pcw" for the Papadopulos and Cooper (1967) solution for well with well-bore storage effect. Analytical solution parameters are: \verb{a} slope of late time straight line, \verb{t0} intercept of late time straight line, \verb{Cd} dimensionless well-bore storage coefficient. When the model is constructed from aquifer parameters, \verb{aqpar} should be a vector of 1 numeric (aquifer transmissivity T). The \verb{testdata} parameters are compulsory for the construction of the gfamodel object, as a vector of 3 numerics (pumping rate Q, Radius rw of well screen, Radius rc of well casing).
#'     \item "htj" for a confined aquifer with leakage with the Hantush and Jacob (1955) model. Analytical solution parameters are: \verb{a}, the slope of Jacob Straight Line; and \verb{t0}, intercept with the horizontal axis for s = 0; and \verb{r/B}. When the model is constructed from aquifer parameters and test data, \verb{aqpar} should be a vector of 3 numeric (Transmissivity, Storativity, Aquitard conductivity), and \verb{testdata} a vector of 3 numerics (Pumping rate, Distance to the pumping well, Thickness of the aquitard).
#'     \item "blt" for a Boulton (1963) model for unconfined aquifer. Analytical solution parameters are: slope\verb{a} of the late time Jacob's straight line"; intercept time \verb{t0} with the horizontal axis for s = 0; intercept \verb{t1}; and empirical parameter \verb{phi} that trigger the delay. When the model is constructed from aquifer parameters and test data, \verb{aqpar} should be a vector of 3 numeric (Transmissivity, Storativity, Drainage Porosity), and \verb{testdata} a vector of 2 numerics (Pumping rate and Distance to the pumping well).
#'     \item "grg" for a Gringarten and Ramey (1974) model for infinite conductivity fracture in a confined aquifer. Analytical solution parameters are slope \verb{a} and intercept \verb{t0}. When the model is constructed from aquifer parameters and test data, \verb{aqpar} should be a vector of 2 numeric (Transmissivity and Sxf2), and \verb{testdata} a numeric (Pumping rate)
#'     \item "war" for the Warren and Root (1965) solution. Analytical solution parameters are: the slope \verb{a} of the Jacob straight line, the time \verb{t0} when the horizontal axis intercept with the early time asymptote, the time \verb{t1} when the horizontal axis intercept with the late time asymptote, the time \verb{tm} of the minimum of the derivative. When the model is constructed from aquifer parameters and test data, \verb{aqpar} should be a vector of 4 numeric (Transmissivity, Storativity Sf, Storativity Sm, Interporosity flow lamda), and \verb{testdata} a vector of 2 numerics (Pumping rate and Distance to the pumping well).
#'     \item "grf" for the Barker (1988) general radial flow model. Analytical solution parameters are: \verb{a} equivalent of the slope of Jacob model, \verb{t0} equivalent of the time of Jacob model, \verb{n} the fractional flow dimension. When the model is constructed from aquifer parameters, \verb{aqpar} should be a vector of 3 numeric (equivalent transmissivity Tn, equivalent storativity Sn, flow dimension n). The \verb{testdata} parameters are compulsory for the construction of the gfamodel object, as a vector of 3 numerics (pumping rate Q, distance r to the pumping well, radius rw of the pumping well).
#'     }
#' @details If \verb{model} is an existing gfamodel, \verb{testsdata} parameters should be given: the \verb{aspar} of the resulting object is then calculated if \verb{model} have valid \verb{aspar}, or the \verb{aqpar} of the resulting model is calculated if model have valid \verb{aspar}.
#' @references   Renard, Philippe (2017). Hytool: an open source matlab toolbox for the interpretation of hydraulic tests using analytical solutions. Journal of Open Source Software, 2(19), 441, \href{http://joss.theoj.org/papers/10.21105/joss.00441}{doi:10.21105/joss.00441}
#' @return An object of class gfamodel the type of selected \verb{model}, the corresponding description, and model parameters of type \verb{aqpar} or \verb{aspar}.
#'     If \verb{model} is a gfamodel object, the returned gfamodel object is constructed from this object and in priority order either the analytical solution parameters (if avilable) or the aquifer parameters. A \verb{testsdata} parameter should then be provided.
#' @examples
#' # Load samples test data
#' data("pumptest")
#' # Construct initial guess of a Hantush and Jacob model for the htj_ds1 data set
#' mod1 <- guess.gfamodel(gfamodel("htj"), drawdowns(pumptest$htj_ds1))
#' # If the first guess is not satisfactory, adjust the model manually
#' mod1 <- gfamodel("htj", aspar = c(5.3,0.3,4e5))
#' # perform automatic fitting and plot result
#' mod2 <- fit.gfamodel(mod1, dd = drawdowns(pumptest$htj_ds1))
#' plot(pumptest$htj_ds1, model = mod2)
#' print(mod2)
#' @seealso \code{\link{fit.gfamodel}}, \code{\link{guess.gfamodel}}
#' @export
gfamodel <- function(model, aspar=NULL, aqpar=NULL, testdata=NULL){
  if(class(model)=="gfamodel") {
    if(is.null(testdata)){
      stop("Missing test data parameters.")
    }
    if(!is.null(aspar)){
      warning("Ignored analytical solution parameters aspar.")
    }
    if(!is.null(aqpar)){
      warning("Ignored aquifer parameters aqpar.")
    }
    aspar <- model@aspar$val
    aqpar <- model@aqpar$val
    if(aspar[1]!=0){
      aqpar=NULL
    }
    else if(aqpar[1]!=0) {
      aspar=NULL
    }
    model <- model@model
  }
  if(!is.element(model, c("ths","thc","thn","pcw","htj","blt","grg","war", "grf", "grc"))) {
    stop("Unknown model.")
  }
  fn <- paste0(".",model,"_details")
  details <- do.call(fn,list())
  if(!is.null(testdata)){
    n <- length(details[[4]])
    if(n>length(testdata)) {
      testdata <- c(testdata,rep(NA,(n-length(testdata)))) # Here much better to stop, all parameters are needed
    }
    if(!is.null(aspar)){
      fn <- paste0(".",model,"_computeparams")
      aqpar <- do.call(fn,list(aspar,testdata))
    }
    else if(!is.null(aqpar)){
      fn <- paste0(".",model,"_computeadjustparams")
      aspar <- do.call(fn,list(aqpar,testdata))
    }
    if(model=="grf"){
      assign("GRFWELLDIMENSIONLESSRADIUS", testdata[2]/testdata[3], envir = .GlobalEnv)
    }
  }
  else {
    testdata <- rep(0, length(details[[4]]))
    if(model=="grf"){
      if(!is.object("GRFRADIUS")){
        stop("Test data should be set when creating a model of 'grf' type")
      }
    }
  }
  if(is.null(aspar)){
    aspar <- rep(0, length(details[[2]]))
  }
  if(is.null(aqpar)){
    aqpar <- rep(0, length(details[[3]]))
  }
  return(new("gfamodel", model = model, description = details[[1]], aspar=data.frame(names=details[[2]], val=aspar), aqpar=data.frame(names=details[[3]], val=aqpar), testdata=data.frame(names=details[[4]],val=testdata)))
}

#' First guess for the analytical solution parameters
#'
#' Function that guess intial analytical solution parameters for fitting a gfamodel to a drawdowns object
#' @param x A gfamodel object. See details.
#' @param dd An drawdowns object used for extracting a first guess.
#' @details  Only model type of the entry \verb{x} object is used. If available, in the analytical solution parameters of the \verb{x} object are ignored.
#' @return An object of class gfamodel of the same type than the entry \verb{x} object with new analytical solution parameters. If available in the entry \verb{x} object, aquifer parameters and test data are copied in the constructed object.
#' @export
guess.gfamodel <- function( x, dd) {
  if(!class(x)=="gfamodel" || !class(dd)=="drawdowns"){
    stop("Incorect arguments type.")
  }
  fn <- paste0(".",x@model,"_gss")
  p <- do.call(fn,list(dd@dd$t, dd@dd$s))
  if(x@testdata$val[1]!=0){
    newmodel <- gfamodel(x@model, aspar=p, testdata=x@testdata$val)
  }
  else {
    newmodel <- gfamodel(x@model,aspar =p)
  }
  return(newmodel)
}

#' Fit parameters of the analytical solution
#'
#' Function that fit the parameters of the analytical solution of a gfamodel object to the drawdowns describes in a drawdowns object.
#' @param x A gfamodel object. See details.
#' @param dd A drawdowns object used for fitting the analytical parameters of the returned gfamodel object.
#' @details  The analytical solution parameters of the \verb{x} object are used as initial parameters. The general-purpose optimization function \code{\link{optimx}} is used. Result from the best fitting from 3 methods ("Nelder-Mead","nlminb", "L-BFGS-B") is used.
#' @return An object of class gfamodel of the same model type as the \verb{x} object in argument. Analytical solution parameters \verb{aspar} are adjusted to fit the \verb{dd} object in argument. If test data parameters \verb{testdata} are given in the \verb{x} object, it is copied in the return object and the aquifer parameters \verb{aqpar} are computed.
#' @seealso \code{\link{optimx}}
#' @importFrom optimx optimx
#' @importFrom utils head
#' @export
fit.gfamodel <- function( x, dd) {
  fn <- paste0(".",x@model,"_dim")
  opt.fn <- function(b,mydata){
    sum((mydata$s-do.call(fn,list(b,mydata$t)))^2)
  }
  suppressWarnings(cap.optx <- optimx::optimx(par=x@aspar$val, fn=opt.fn, mydata=dd@dd, method=c("Nelder-Mead","nlminb", "L-BFGS-B"), control=list(save.failures=TRUE, maxit=2000)))
  res <- summary(cap.optx, order = "value")
  print(utils::head(res))
  n <- nrow(res)
  for(i in 1:n){
    new.aspar <- as.vector(as.matrix(res[i, 1:length(x@aspar$val)]))
    if(length(which(new.aspar<=0))==0){
      return(gfamodel(x@model, aspar=new.aspar, testdata = x@testdata$val))
    }
  }
  stop("Impossible fitting with non-negative parameters. Please change initial parameters.")
}

