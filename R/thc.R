.thc_details <- function(useless=NULL) {
  a <- "Theis (1935) model with a constant head boundary"
  b <- c("slope a","intercept t0","intercept ti")
  c <- c("Transmissivity T (L^2/T)","Storativity S (-)","Distance to image well ri (L)")
  d <- c("Pumping rate Q (L^3/T)", "Distance r to the pumping well (L)")
  return(list(a,b,c,d))
}

.thc_computeparams <- function(adp,data) {
  if(length(adp)<3) {
    stop("Incorrect length for the adp vector. Length of 3 expected.")
  }
  if(length(adp)>3) {
    warning("Incorrect length for the adp vector. Length of 3 expected. Extra parameters ignored.")
  }
  if(length(data)<2) {
    stop("Incorrect length for the data vector. Length of 2 expected.")
  }
  if(length(data)>2) {
    warning("Incorrect length for the data vector. Length of 2 expected. Extra parameters ignored.")
  }
  Tr <- 0.1832339*data[1]/adp[1]
  S <- 2.2458394*Tr*adp[2]/data[2]^2
  ri <- sqrt(2.2458394*Tr*adp[3]/S)
  return(c(Tr,S,ri))
}

.thc_computeadjustparams <- function(p,data) {
  if(length(p)<3) {
    stop("Incorrect length for the p vector. Length of 3 expected.")
  }
  if(length(p)>3) {
    warning("Incorrect length for the p vector. Length of 3 expected. Extra parameters ignored.")
  }
  if(length(data)<2) {
    stop("Incorrect length for the data vector. Length of 2 expected.")
  }
  if(length(data)>2) {
    warning("Incorrect length for the data vector. Length of 2 expected. Extra parameters ignored.")
  }
  a <- 0.1832339*data[1]/p[1]
  t0 <- p[2]*data[2]^2/(2.2458394*p[1])
  ti <- (p[2]*p[3]^2)/(2.2458394*p[1])
  return(c(a,t0,ti))
}

.thc_gss <- function(t, s) {
  res <- .ldiff_spline(t,s,df=20)
  td <- res[which(res[,2]>0),1]
  d <- res[which(res[,2]>0),2]
  sl <- max(s)
  du <- max(d)
  i <- which.max(d)
  tu <- td[i]
  i <- which(t>=tu)
  ta <- t[i[1]]
  sa <- s[i[1]]
  end <- length(td)
  ts <- td[end]
  ds <- d[end]
  # Dimensionless distance
  rd <- .thc_ird(sl/du)
  # Slope of Jacob's straight line
  a <- sl/2/log10(rd)
  # Origin of jacob straight line
  if (rd<50) {
    t0 <- 2*tu*log(rd)/(0.5625*(rd^2-1))
  }
  else {
    t0 <- ta*10^(-sa/a)
  }
  # Origin of the second line
  t1 <- rd^2*t0
  # Time of intersection of the two lines
  ti <- t1^2/t0
  return(c(a,t0,ti))
}

#' @importFrom stats approx
NULL

.thc_ird <- function( frd ) {
#  require(stats)
  if (frd < 2.71828182850322) {
    warning("Problem in the inversion of Rd: thc_ird","")
    rd <- 1.000001
  }
  else {
    rd <- exp(frd/2)
    if (rd>50) {
      y <- seq(1,60,by=0.05)
      y[(1)] <- 1.000001
      x <- .thc_frd(y)
      rd <- stats::approx(x,y,xout=frd,rule = 2)$y
    }
  }
  return(rd)
}

.thc_frd <- function( rd ) {
  if(rd[1]==1) {
    rd <- 1.00001
  }
  rd2 <- rd^2
  rd21 <- rd2-1
  frd <- log(rd2)/(rd21*rd^(-2*rd2/rd21))
  return(frd)
}

.thc_dim <- function( p, t) {
  s <- .ths_dim(c(p[1],p[2]),t) - .ths_dim(c(p[1],p[3]),t)
  return(s)
}

.thc_std <- function(){
  return(gfamodel("thc", aqpar=c(1e-4,1e-4, 40), testdata = c(0.1, 10)))
}
