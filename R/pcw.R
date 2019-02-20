.pcw_details <- function(useless=NULL) {
  a <- "Papadopulos and Cooper (1967) solution for well with well-bore storage effect"
  b <- c("slope a of late time straight line","intercept t0 of late time straight line","dimensionless well-bore storage coefficient Cd")
  c <- c("Transmissivity T (L^2/T)")
  d <- c("Pumping rate Q (L^3/T)", "Radius of well screen (m)", "Radius of the casing (m)")
  return(list(a,b,c,d))
}

.pcw_computeparams <- function(adp,data) {
  if(length(adp)<3) {
    stop("Incorrect length for the adp vector. Length of 3 expected.")
  }
  if(length(adp)>3) {
    warning("Incorrect length for the adp vector. Length of 3 expected. Extra parameters ignored.")
  }
  if(length(data)<3) {
    stop("Incorrect length for the data vector. Length of 3 expected.")
  }
  if(length(data)>3) {
    warning("Incorrect length for the data vector. Length of 3 expected. Extra parameters ignored.")
  }
  Q <- data[1]
  rw <- data[2]
  rc <- data[3]
  a <- adp[1]
  t0 <- adp[2]
  Cd <- adp[3]
  Tf <- 0.1832339*Q/a
  return(c(Tf))
}

#### TO BE CORRECTED
.pcw_computeadjustparams <- function(p,data) {
  if(length(p)<1) {
    stop("Incorrect length for the p vector. Length of 1 expected.")
  }
  if(length(p)>1) {
    warning("Incorrect length for the p vector. Length of 1 expected. Extra parameters ignored.")
  }
  if(length(data)<3) {
    stop("Incorrect length for the data vector. Length of 3 expected.")
  }
  if(length(data)>3) {
    warning("Incorrect length for the data vector. Length of 3 expected. Extra parameters ignored.")
  }
  Q <- data[1]
  rw <- data[2]
  rc <- data[3]
  Tf <- p[1]

  a <- 0.1832339*Q/Tf
  t0 <- NA#Sf*r^2/(2.245839*Tf)
  Cd <- NA#(Sm+Sf)*r^2/(2.245839*Tf)
  return(c(a,t0,Cd))
}


.pcw_dim <- function( p, t, ...) {
  a <- p[1]
  t0 <- p[2]
  Cd <- p[3]
  td <- 0.445268*t/t0
  sd <- .pcw_dls(Cd,td)
  s <- 0.868588963806504*a*sd # initially 0.868589
  return(s)
}

.pcw_dls <- function( x, td) {
  sd <- .stefhest('pcw',td, x[1])
  return(sd)
}

.pcw_lap<- function( x, p) {
  # x[1] = Cd
  # p = Laplace parameter
  sp <- sqrt(p)
  k0 <- besselK(sp, 0)
  sd <- k0/(p*(sp*besselK(sp,1)+p*x[1]*k0))
  return(sd)
}

.pcw_gss <- function( t, s, ...) {
  res<-.ldiff_spline(t,s)
  td <- res[which(res[,2]>0),1]
  d <- res[which(res[,2]>0),2]
  end <- length(d)
  if(d[end]>0){
    a <- log(10)*d[end]
    t0 <- t[length(t)]*exp(-s[length(s)]/d[end])
  } else {
    p <- .ths_gss(t,s)
    a <- p[1]
    t0 <- p[2]
  }
  if( t0 <= 0){
    t0 <- 1e-5
  }
  sp <- s[which(s>0)]
  tp <- t[which(s>0)]
  Cd <- 0.8905356*d[end]/sp[1]*tp[1]/t0
  return(c(a, t0, Cd))
}

.pcw_std <- function(){
  return(gfamodel("pcw", aspar=c(2.6, 5e-1, 5e1), testdata = c(8e-3, 0.11, 2.4)))
}
