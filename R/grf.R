.grf_details <- function(useless=NULL) {
  a <- "Barker (1988) general radial flow model"
  b <- c("a (L), equivalent of the slope of Jacob model","t0 (T), equivalent of the time of Jacob model","n (-), the fractional flow dimension")
  c <- c("Equivalent transmissivity Tn (L^2/T)","Equivalent storativity Sn (-)", "Flow dimension n")
  d <- c("Pumping rate Q (L^3/T)","Distance r to the pumping well (L)","Radius rw of the pumping well (L)")
  return(list(a,b,c,d))
}

.grf_computeparams <- function(adp,data) {
  if(length(adp)<3) {
    stop("Incorrect length for the aspar vector. Length of 3 expected.")
  }
  if(length(adp)>3) {
    warning("Incorrect length for the aspar vector. Length of 3 expected. Extra parameters ignored.")
  }
  if(length(data)<3) {
    stop("Incorrect length for the testdata vector. Length of 1 expected.")
  }
  if(length(data)>3) {
    warning("Incorrect length for the testdata vector. Length of 1 expected. Extra parameters ignored.")
  }
  Tn <- log(10)*data[1]/4/pi/adp[1]
  Sn <- 2.2458394*Tn*adp[2]/data[3]^2
  return(c(Tn,Sn,adp[3]))
}

.grf_computeadjustparams <- function(p,data) {
  if(length(p)<3) {
    stop("Incorrect length for the aqpar vector. Length of 3 expected.")
  }
  if(length(p)>3) {
    warning("Incorrect length for the aqpar vector. Length of 3 expected. Extra parameters ignored.")
  }
  if(length(data)<3) {
    stop("Incorrect length for the testdata vector. Length of 2 expected.")
  }
  if(length(data)>3) {
    warning("Incorrect length for the testdata vector. Length of 2 expected. Extra parameters ignored.")
  }
  a <- log(10)*data[1]/4/pi/p[1]
  t0 <- (data[3]^2)*p[2]/2.2458394/p[1]
  n <- p[3]
  return(c(a,t0,n))
}

.grf_dim <- function( p, t, ...) {
  a <- p[1]
  t0 <- p[2]
  td <- t/2.2458/t0
  sd <- 0.868588963806504*a*.grf_dls(p[3], GRFWELLDIMENSIONLESSRADIUS, td)
  return(sd)
}

.grf_dls <- function( x1, x2, t) {
  s <- .stefhest("grf",t, x1, x2)
  return(s)
}

.grf_lap<- function( x, p) {
  n <- x[1]
  rd <- x[2]
  sp <- sqrt(p)
  s <- rd^(2-n)*(rd^2*p/4)^(n/4-0.5)*besselK(rd*sp, n/2-1)/p/gamma(n/2)
  return(s)
}

.grf_gss <- function( t, s, ...) {
  p <- .ths_gss(t,s)
  p[2] <- p[2]*(1/GRFWELLDIMENSIONLESSRADIUS)^2
  p[3] <- 2
  return(p)
}

.grfinf2_std <- function(){
  model <- gfamodel("grf", aqpar=c(1e-4,1e-1, 1.5), testdata = c(0.3, 0.2, 10))
  model@description <- paste0(model@description," with dimension n < 2")
  return(model)
}

.grfsup2_std <- function(){
  model <- gfamodel("grf", aqpar=c(1e-4,1e-1, 2.5), testdata = c(0.3, 0.2, 10))
  model@description <- paste0(model@description," with dimension n > 2")
  return(model)
}
