.blt_details <- function(useless=NULL) {
  a <- "Boulton (1963) model for unconfined aquifer"
  b <- c("slope a of the late time Jacob's straight line","intercept time t0 with the horizontal axis for s = 0","intercept t1","phi, empirical parameter that trigger the delay")
  c <- c("Transmissivity T (L^2/T)","Storativity S (-)", "Drainage Porosity")
  d <- c("Pumping rate Q (L^3/T)", "Distance r to the pumping well (L)")
  return(list(a,b,c,d))
}

.blt_computeparams <- function(adp,data) {
  if(length(adp)<3) {
    stop("Incorrect length for the aspar vector. Length of 3 expected.")
  }
  if(length(adp)>3) {
    warning("Incorrect length for the aspar vector. Length of 3 expected. Extra parameters ignored.")
  }
  if(length(data)<2) {
    stop("Incorrect length for the testdata vector. Length of 2 expected.")
  }
  if(length(data)>2) {
    warning("Incorrect length for the testdata vector. Length of 2 expected. Extra parameters ignored.")
  }
  Tr <- 0.1832339*data[1]/adp[1]
  S <- 2.2458394*Tr*adp[2]/data[2]^2
  omegad=2.2458394*Tr*adp[3]/data[2]^2-S
#  Ri=2*sqrt(Tr*data[3]/omegad)
  return(c(Tr,S,omegad))
}

.blt_computeadjustparams <- function(p,data) {
  if(length(p)<3) {
    stop("Incorrect length for the aqpar vector. Length of 3 expected.")
  }
  if(length(p)>3) {
    warning("Incorrect length for the aqpar vector. Length of 3 expected. Extra parameters ignored.")
  }
  if(length(data)<2) {
    stop("Incorrect length for the testdata vector. Length of 2 expected.")
  }
  if(length(data)>2) {
    warning("Incorrect length for the testdata vector. Length of 2 expected. Extra parameters ignored.")
  }
  a <- 0.1832339*data[1]/p[1]
  t0 <- p[2]*data[2]^2/(2.2458394*p[1])
  t1 <- ((p[3]+p[2])*data[2]^2)/(2.2458394*p[1])
  return(c(a,t0,t1, 1E-4))
}

.blt_dim <- function( p, t, ...) {
  td <- 0.445268*t/p[2]
  sd <- 0.868589*p[1]*.blt_dls(p[2]/(p[2]+p[3]),2*p[4]*p[2],td)
  return(sd)
}

.blt_dls <- function( x1, x2, t) {
  s <- .stefhest("blt",t, x1, x2)
  return(s)
}

.blt_lap<- function( x, p) {
  s <- besselK(sqrt( p + x[2]*p/(x[1]*(p+x[2]))),0 )/p
  return(s)
}

.blt_gss <- function( t, s, ...) {
  end <- length(t)
  p <- rep(NA,4)
  p[2] <- t[1]
  n <- round(end/3)
  t <- t[n:end]
  s <- s[n:end]
  pj <- .jcb_gss(t,s)
  p[1] <- pj[1]
  p[3] <- pj[2]
  p[4] <- 1E-4
  return(p)
}

.blt_std <- function(){
  return(gfamodel("blt", aqpar=c(1e-4,1e-4, 1e-7), testdata = c(0.3, 10)))
}

