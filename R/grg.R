.grg_details <- function(useless=NULL) {
  a <- "Gringarten and Ramey (1974) model for infinite conductivity fracture in a confined aquifer"
  b <- c("slope a (L)","intercept t0 (T)")
  c <- c("Transmissivity T (L^2/T)","Sxf2 (L^2)")
  d <- c("Pumping rate Q (L^3/T)")
  return(list(a,b,c,d))
}

.grg_computeparams <- function(adp,data) {
  if(length(adp)<2) {
    stop("Incorrect length for the adp vector. Length of 2 expected.")
  }
  if(length(adp)>2) {
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
  return(c(Tr,Sxf2))
}

.grg_computeadjustparams <- function(p,data) {
  if(length(p)<2) {
    stop("Incorrect length for the p vector. Length of 2 expected.")
  }
  if(length(p)>2) {
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
  return(c(a,t0))
}

#' @importFrom stats pnorm
NULL

.grg_dim <- function( p, t, ...) {
  a <- p[1]
  t0 <- p[2]
  u <- t/t0
  erf <- 2 * pnorm((1/(2*sqrt(u))) * sqrt(2)) - 1
  suppressWarnings( s <- a*(2*sqrt(pi*u)*erf+expint_E1(1/(4*u))) )
  return(s)
}

.grg_gss <- function( t, s, ...) {
  end <- length(t)
  n <- round(end/4)
  t <- t[n:end]
  s <- s[n:end]
  pj <- .jcb_gss(t,s)
  return(pj)
}

.grg_std <- function(){
  return(gfamodel("grg", aqpar=c(1e-4,2e-1), testdata = c(0.1)))
}

