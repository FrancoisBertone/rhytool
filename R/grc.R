.grc_details <- function(useless=NULL) {
  a <- "Gringarten and Ramey (1974) model for infinite conductivity fracture in a confined aquifer"
  b <- c("slope a (L)","intercept t0 (T)","intercept ti")
  c <- c("Transmissivity T (L^2/T)","Sxf2 (L^2)","Distance to image well ri (L)")
  d <- c("Pumping rate Q (L^3/T)")
  return(list(a,b,c,d))
}

.grc_computeparams <- function(adp,data) {
  if(length(adp)<3) {
    stop("Incorrect length for the adp vector. Length of 2 expected.")
  }
  if(length(adp)>3) {
    warning("Incorrect length for the adp vector. Length of 2 expected. Extra parameters ignored.")
  }
  if(length(data)<1) {
    stop("Incorrect length for the data vector. Length of 1 expected.")
  }
  if(length(data)>1) {
    warning("Incorrect length for the data vector. Length of 1 expected. Extra parameters ignored.")
  }
  Tr <- data[1]/(4*pi*adp[1])
  Sxf2 <- adp[2]/Tr
  ri <- 10 ### To be set
  return(c(Tr,Sxf2, ri))
}

.grc_computeadjustparams <- function(p,data) {
  if(length(p)<3) {
    stop("Incorrect length for the p vector. Length of 2 expected.")
  }
  if(length(p)>3) {
    warning("Incorrect length for the p vector. Length of 2 expected. Extra parameters ignored.")
  }
  if(length(data)<1) {
    stop("Incorrect length for the data vector. Length of 1 expected.")
  }
  if(length(data)>1) {
      warning("Incorrect length for the data vector. Length of 1 expected. Extra parameters ignored.")
  }
  a <- data[1]/(4*pi*p[1])
  t0 <- p[2]/p[1]
  ti <- t0 ### To be set
  return(c(a,t0, ti))
}

.grc_dim <- function( p, t, ...) {
  s <- .grg_dim(c(p[1],p[2]),t) - .grg_dim(c(p[1],p[3]),t)
  return(s)
}

.grc_gss <- function( t, s, ...) {
  end <- length(t)
  n <- round(end*0.2)
  m <- round(end*0.6)
  o <- round(end*0.8)
  t1 <- t[n:m]
  s1 <- s[n:m]
#  mod <- lm(formula = s1 ~ t1)
#  pj <- c(mod$coefficients[2],mod$coefficients[1])
  pj <- .jcb_gss(t1,s1)
  s2 <- s[o:end]
  mod <- lm(formula = s2 ~ 1)
  ti <- (mod$coefficients[1] - pj[1])/pj[2]
  return(c(pj,1E5))
}

.grc_std <- function(){
  return(gfamodel("grc", aqpar=c(1e-4,2e-1, 10), testdata = c(0.1)))
}

