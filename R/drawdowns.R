#' @importFrom methods setClass
setClass("drawdowns",
         representation(dd="data.frame",ldiff="data.frame"),
         prototype = list(dd=data.frame(t=NULL,s=NULL),ldiff=data.frame(dt=NULL,ds=NULL)),
         validity = function(object){
           if((length(object@dd$t) != length(object@dd$s)) || ((length(object@dd$t) != length(object@dd$s)))) {
             return("Inconsistent length of vectors")
           }
           return(TRUE)
         })
#' Create drawdowns objects
#'
#' Function that creates drawdowns objects, i.e. objects that hold the information necessary for being adjusted to an analytic groundwater flow model (class gfamodel) for the interpretation of a constant rate or variable rate pumping test. It fundamentally contains time (or reduced time) and drawdowns (or reduced drawdowns). It can be constructed from variable flow rate test and/or for recovery period.
#' @param t numeric vector or matrix; time from the begining of the pumping test unless Argarwal type is selected. In this case, time from the begining of the recovery period.
#' @param s numeric vector representing the drawdown measured for the corresponding time. If s is null and t a matrix, s is set to the second column of the t matrix. If s is NULL and t a vector, s is calculated from the gfamodel parameter model.
#' @param fn a character string setting the nature of the treatment applied to the original t and s values. See details.
#' @param q numeric or numeric matrix; ignored if fn = NULL, otherwise, q is a parameter giving the pumping rate. In case of constant pumping rate, value of the pumping rate. In case of variable pumping rate, a double columns matrix, each row containg time (since the begining of the test) at which the period ends and flow rate during the period.
#' @param model object of class gfamodel used to calculate the s value for every t data. If s is not null, model is ignored.
#' @param sample logical. If TRUE, data set is sampled at regular intervals on a logarithmic scale (points are extracted from the data set). A number of samples can be indicated in the list of parameters with the nval keyword.
#' @param miscDdOptions list with named arguments that provide additional control over the treatment applied to the original t and s values. For example: list(defunc = "std"). If the list is empty, drawdowns uses default values. The following parameters can be set:
#' \describe{
#'     \item{nval}{maximum number of resulting data points when sampling raw data is activated.}
#'     \item{defunc}{nature of the function used for calculating the derivative. See Details.}
#'     \item{df}{additional parameter for the function used for calculating the derivative. For spline function, represents the degree of freedom. See Details.}
#'     \item{filter.width}{a positive integer defining the window width used for robust regression filter of the derivative values. Default value is 7. See \code{\link{robreg.filter}}.}
#'     \item{filter.method}{a character string containing the method to be used for robust regression filter of the derivative values. Possible options are: "DR" for Deepest Regression, "LMS" for Least Median of Squares regression, "LQD" for Least Quartile Difference regression, "LTS" for Least Trimmed Squares regression, "MED" for Median, "RM" for Repeated Median regression. Default method is "RM". See \code{\link{robreg.filter}}.}
#'     }
#' @details The fn parameter is used to generate drawdowns either from raw drawdown data (s) or from the gfamodel clas object model. When NULL, raw original values of s are used. Available keywords are:
#' \itemize{
#'     \item "birsoy", the Birsoy and Summers (1980) time is calculated based on the q series of pumping rates. q is there a a double columns matrix with column 1 giving the time (since the begining of the test) at which the period ends, and column 2 giving the flow rate during the period.
#'     \item "agarwal", result is the equivalent Agarwal (1980) time for recovery tests.
#'           Agarwal has shown in 1980 that recovery test can be interpreted with the same solutions than pumping test if one interprets the residual drawdown sr = s(t) - s(end of pumping) as a function of an equivalent time that is computed from the pumping rates history. The theory is based on the superposition principle.
#'           t should be the time since the begining of the recovery period. q can be either a unique value for the duration of the pump test (constant pumping rate test) or a double columns matrix with column 1 giving the time (since the begining of the test) at which the period ends, and column 2 giving the flow rate during the period (variable pumping rates).
#'     \item "variablerate", the drawdowns are calculated for the specified dates t, based on the model parameters (aquifer parameters aqpar should be given as well as test data testdata with pumping rate Q=1) and on the pumping rate data q. q is there a double columns matrix with column 1 giving the time (since the begining of the test) at which the period ends, and column 2 giving the flow rate during the period (variable pumping rates).
#'     }
#' @details The function to be used for the computation of the derivative points can be set in the miscDdOptions parameters list, with the defunc keyword. The default value is the spilne function. Available options are:
#' \itemize{
#'     \item "spline", this function approximates logarithmic derivative with spline, usign the smooth.spline function of the stats package. Use df keyword to set the degree of freedom (see \code{\link{smooth.spline}}). Default value for df is 2/3 of the number of data points.
#'     \item "std", this function approximates logarithmic derivative with centered differences.
#'     \item "bourdet", this function approximates logarithmic derivative with Bourdet's formula. Use df keyword to set the distance parameter of the Bourdet function. Default value is 2.
#'     \item "horne", this function approximates logarithmic derivative with the Horne's formula.
#'     }
#' @return An object of class drawdowns containing time or reduced time and drawdowns or reduced drawdowns ready for adjustement with a gfamodel
#' @seealso \code{\link{smooth.spline}},  \code{\link{gfamodel}}, \code{\link{robreg.filter}}
#' @references Renard, Philippe (2017). Hytool: an open source matlab toolbox for the interpretation of hydraulic tests using analytical solutions. Journal of Open Source Software, 2(19), 441, \doi{10.21105/joss.00441}.
#' @references Agarwal, R.G., 1980. A new method to account for producing time effects when drawdown type curves are used to analyse pressure buildup and other test data. Proceedings of the 55th Annual Fall Technical Conference and Exhibition of the Society of Petroleum Engineers. Paper SPE 9289, \doi{10.2118/9289-MS}
#' @references Birsoy YK, Summers WK (1980) Determination of aquifer parameters from step tests and intermittent pumping data. Ground Water 18(2):137???146, \doi{10.1111/j.1745-6584.1980.tb03382.x}.
#' @examples
#' # Generate drawdowns object for a standard series of t and s
#' MyDd <- drawdowns(t = pumptest$ths_ds1[,1], s = pumptest$ths_ds1[,2])
#' MyDd <- drawdowns(t = pumptest$ths_ds1[,1:2])
#'
#' # Generate drawdowns object for a series of date knowing
#' # aquifer parameters (T = 1E-3 m2/s and S = 5E-4) and
#' # pumping conditions (pumping rate 3.6E-1 m3/s and distance 50 m)
#' MyDd <- drawdowns(t = (10^seq(2,5,length=30)),
#'            model = gfamodel("ths", aqpar = c(1E-3, 5E-4),
#'            testdata = c(3.6E-1, 50)))
#'
#' # Generate drawdowns object for a large series of t and s after sampling
#' MyDd <- drawdowns(pumptest$thc_ds2[,1:2], sample = TRUE,
#'            miscDdOptions = list(nval = 70))
#'
#' # Generate drawdowns object from recovery data for a constante rate pumping test
#' MyDd <- drawdowns(pumptest$agt_ds1, fn = "agarwal", q = 240*60,
#'           miscDdOptions = list(df = 10, defunc="spline"))
#'
#' # Generate normalised drawdowns object from variable rate pumping test
#' q<- cbind(c(1800, 4800, 7800),c(500/24/60/60, 700/24/60/60, 600/24/60/60))
#' MyDd <- drawdowns(pumptest$tmr_ds1, fn="birsoy", q=q)
#' @importFrom methods new
#' @export
drawdowns <- function(t, s=NULL, q=NULL, fn=NULL, model=NULL, sample=FALSE, miscDdOptions=list()) {
  if(!is.null(s)){
    # Both arguments t and s are set. Convert to vector and check length
    s <- as.data.frame(s)[,1]
    t <- as.data.frame(t)[,1]
    if(length(t)!=length(s) || !is.numeric(t)  || !is.numeric(s)){
      stop("Vectors t and s should be the same length and numeric values")
    }
  }else if(dim(as.data.frame(t))[2]>=2){
    # S is not set and t is a matrix or list or data.frame. Convert t to 2 vectorz t and s, and check length
    s <- as.data.frame(t)[,2]
    t <- as.data.frame(t)[,1]
    if(length(t)!=length(s) || !is.numeric(t)  || !is.numeric(s)){
      stop("Vectors t and s should be the same length and numeric values")
    }
  }else if(!is.null(model)){
    # S is not set, t is a vector, and model is set. Construct s from model.
    t <- as.data.frame(t)[,1]
    if(is.null(fn)){
      dimfn <- paste0(".",model@model,"_dim")
      s <- do.call(dimfn,list(model@aspar$val, t))
      miscDdOptions$defunc <- "std"
    }else if(fn=="variablerate"){
      s <- .vrt(t, q, model)
      miscDdOptions$defunc <- "std"
      fn=NULL
      q=NULL
    }
  }else{
    stop("Missing agrument s or missing agrument model")
  }
  x <- t[which(!is.na(s) & s >= 1e-7 & !is.na(t) & t > 0)]
  y <- s[which(!is.na(s) & s >= 1e-7 & !is.na(t) & t > 0)]
  i <- sort(x, index.return=TRUE)$ix
  y <- y[i]
  x <- x[i]
  res <- cbind(x, y)
  if(is.null(miscDdOptions$defunc)){
    miscDdOptions$defunc <- "spline"
  }
  else if(!is.element(miscDdOptions$defunc, c("spline","bourdet","std","fb"))) {  ## , "horne" ?? corriger
      warning("Unknown derivative function defunc. Default value used.")
    miscDdOptions$defunc <- "spline"
    }
  defunc <- paste0(".ldiff_",miscDdOptions$defunc)
  if(is.null(miscDdOptions$df)){
    miscDdOptions$df = NULL
  }
  if(is.null(fn)) {
    if(!is.null(q)){
      warning("Argument q ignored")
    }
  }
  else if(fn=="birsoy"){
    if(is.null(q)){
      stop("Missing agrument q")
    }
    res = .birsoy(x,y,q)
  }
  else if(fn=="agarwal"){
    if(is.null(q)){
      stop("Missing agrument q")
    }
    res = .agarwal(x,y,q)
  }
  else{
    stop("Unknown function fn")
  }
  if(sample){
    ns <- miscDdOptions$nval
    if(class(ns)!="numeric" || ns < 10){
      warning("Invalid or missing nval argument for the sampling method. Default value used.")
      ns = NULL
    }
    res = .hysampling(res[,1], res[,2], ns)
  }
  ldres <- do.call(defunc,list(x=res[,1], y=res[,2], df=miscDdOptions$df, nknots = miscDdOptions$nknots, filter.width=miscDdOptions$filter.width, filter.method=miscDdOptions$filter.method))
  return(new("drawdowns", dd=data.frame(t = res[,1], s = res[,2]), ldiff=data.frame(dt = ldres[,1], ds = ldres[,2])))
}

.hysampling <- function(x1,y1,nval) {
  if(is.null(nval)){
    nval=100
  }
  x <- x1[which(!is.na(y1) & !is.na(x1) & x1 != 0)]
  y <- y1[which(!is.na(y1) & !is.na(x1) & x1 != 0)]
  end <- length(x)
  xs <- 10 ^ seq(log10(x[1]),log10(x[end]),length=(nval+1))
  ys <- rep(NA,length(xs))
  for(i in 1:(nval+1)) {
    dist <- sqrt((x-xs[i])^2)
    mn <- min(dist)
    j <- which(dist==mn)
    xs[i] <- x[j]
    ys[i] <- y[j]
  }
  ys <- ys[!duplicated(xs)]
  xs <- xs[!duplicated(xs)]
  return(cbind(xs,ys))
}

.birsoy <- function(t, s, q) {
  pe <- rep(1,length(t))
  lq <- pe
  l <- length(q[,1])
  for(i in seq(l,1,-1)){
    pe[which(t<=q[i,1])] <- i
  }
  lq<-q[pe,2]
  # dq = vector of the pumping rate increments for each period
  dq <- c(q[1,2],diff(q[,2]))
  # st = starting time of the pumping periods
  st <- c(0,q[,1])
  st[length(st)] <- NA
  # tb = Birsoy and Summers equivalent time
  nt <- length(t)
  tb <- rep(1,nt)
  nq <- length(q[,1])
  for (j in 1:nt){
    n <- pe[j]
    for (i in 1:n){
      tb[j] <- tb[j]*(t[j]-st[i])^(dq[i]/q[n,2])
    }
  }
  i <- order(tb)
  tb <- sort(tb)
  # Reduced drawdown
  sb <- s[i]/lq[i]
  return(cbind(tb,sb))
}

.agarwal <- function(tr,sr,q){
  n <- dim(q)[1]
  if ( is.null(n) ){
    ta <-  q*tr/(q+tr)
  } else {
    tp <- q[,1]
    qp <- q[,2]
    ta <- tp[n-1]/(tr+tp[n-1])^(qp[1]/(qp[(n-1)]-qp[(n)]))
    for (j in 2:(n-1)){
      ta <- ta*((tp[n-1]-tp[j-1])/(tr+tp[n-1]-tp[j-1]))^((qp[j]-qp[j-1])/(qp[n-1]-qp[n]))
    }
    ta <- ta*tr
  }
  sa <- sr[which(!duplicated(ta))]
  ta <- ta[which(!duplicated(ta))]
  return(cbind(ta,sa))
}

#' @importFrom stats smooth.spline
.ldiff_spline <- function(x, y, df=NULL, nknots = 5, ...) {
  # Approximate logarithmic derivative with Spline
  end <- length(x)
  if(is.null(df)){
    df = round(end*2/3)
  }
  smp <- smooth.spline(x, y, df= df, keep.data = TRUE, all.knots=FALSE, nknots = nknots)
  xi<-smp$x
  yi<-smp$y
  end <- length(xi)
  xd <- xi[2:(end-1)]
  end <- length(yi)
  yd <- xd*(yi[3:end]-yi[1:(end-2)])/(xi[3:end]-xi[1:(end-2)])
  xd <- xd[which(yd>0)]
  yd <- yd[which(yd>0)]
  return(cbind(xd,yd))
}

.ldiff_bourdet <- function(x, y, df=2, ...) {
  # approximate logarithmic derivative with Bourdet's formula
  if(is.null(df)){
    df = 2
  }
  else {
    df <- round(log10(df))
  }
  x <- x[which(x>0)]
  y <- y[which(x>0)]
  end <- length(x)
  if( (end-2*df+1)<3 || (1+df+2)>(end-df)){
    stop("Distance df for Bourdet function is too high.")
  }
  dx <- diff(log(x))
  end <- length(dx)
  dx1 <- dx[1:(end-2*df+1)]
  dx2 <- dx[(2*df):end]
  dy <- diff(y)
  end <- length(dy)
  dy1 <- dy[1:(end-2*df+1)]
  dy2 <- dy[(2*df):end]
  end <- length(x)
  xd <- x[(1+df):(end-df)]
  yd <- (dx2*dy1/dx1+dx1*dy2/dx2)/(dx1+dx2)
  return(cbind(xd,yd))
}

### Pas bon. d1+d2+d3 trop petit!!!!!!!!!!!!!!!!!!!!!!!!!!!
.ldiff_horne <- function(x, y, ...) {
  # Approximate logarithmic derivative with Horne formula
  x <- x[which(x>0)]
  y <- y[which(x>0)]
  end <- length(x)
  t1 <- x[1:(end-2)]
  t2 <- x[2:(end-1)]
  t3 <- x[3:end]
  s1 <- y[1:(end-2)]
  s2 <- y[2:(end-1)]
  s3 <- y[3:end]
  dy <- diff(y)
  d1 = (log(t2/t1)*s3) /      (log(t3/t2)*log(t3/t1))
  d2 = (log(t3*t1/t2^2)*s2) / (log(t3/t2)*log(t2/t1))
  d3 = (log(t3/t2)*s1) /      (log(t2/t1)*log(t3/t1))
  xd <- d1+d2+d3
  yd <- t2
  return(cbind(xd,yd))
}

.ldiff_std <- function(x, y, ...) {
  x <- x[which(x>0)]
  if(length(x)<2) return(c(NULL,NULL))
  y <- y[which(x>0)]
  end <- length(x)
  dx <- diff(x)
  dy <- diff(y)
  xd <- sqrt(x[1:(end-1)]*x[2:end])
  yd <- xd*dy/dx
  return(cbind(xd,yd))
}


#' @importFrom robfilter robreg.filter
.ldiff_fb <- function(x, y, filter.width=NULL, filter.method=NULL, ...) {
  if(is.null(filter.width)) filter.width=5
  if(is.null(filter.method)) filter.method="LTS"
  x <- x[which(x>0)]
  if(length(x)<2) return(c(NULL,NULL))
  y <- y[which(x>0)]
  end <- length(x)
  x <- as.vector(unlist(robreg.filter(x, width=filter.width, method=filter.method[1])$level))
  dx <- diff(x)
  dy <- diff(y)
  xd <- sqrt(x[1:(end-1)]*x[2:end])
  xd <- as.vector(unlist(robreg.filter(xd, width=filter.width, method=filter.method[1])$level))
  yd <- xd*dy/dx
  res <- cbind(xd, yd)
#  res <- .ldiff_spline(x, y)
#  res[,2] <- as.vector(unlist(robreg.filter(res[,2], width=filter.width, method=filter.method[1])$level))
  return(res)
}


.vrt <- function(t.original, q, model){
  if(is.null(q)){
    stop("For variable rate drawdowns calculation, q should be set.")
  }
  if(dim(q)[2]<2){
    stop("For variable rate drawdowns calculation, q should be a double cloumns matrix.")
  }
  if(is.null(model@testdata) || is.null(model@aqpar)){
    stop("For variable rate drawdowns calculation, aquifer parameters and test data (with Q=1) should be given.")
  }
  aspar <- model@aspar$val
  if(model@aspar$val[1]==0){
    asparfn <- paste0(".",model@model,"_computeadjustparams")
    testdata <- model@testdata$val
    testdata[1] <- 1
    aspar <- do.call(asparfn,list(model@aqpar$val, testdata))
  }
  dimfn <- paste0(".",model@model,"_dim")
  t <- unique(sort(c(t.original,q[,1])))
  q.at.t <- approx(q,xout=t,method="constant", f=1, rule=2)$y
  q.at.t[ which(t > max(q[,1]))] <- 0
  a <- t%*%t(rep(1, length(t)))
  b <- rbind(rep(0,length(t)), a[-length(t),])
  c<- t(a)-b
  c[lower.tri(c)] <- 0
  s <- do.call(dimfn,list(aspar, as.vector(t(c))))
  s <- matrix(s, ncol=length(t), byrow = TRUE)
  s <- s*c(q[1,2], diff(q.at.t))
  s <- colSums(s, na.rm = TRUE)
  s <- s[which(is.element(t,t.original))]
  s[which( s < 1e-7 )] <- 1e-7
  return(s)
}
