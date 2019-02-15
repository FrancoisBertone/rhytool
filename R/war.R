.war_details <- function(useless=NULL) {
  a <- "Warren and Root (1965) solution for double porosity flow"
  b <- c("slope a of Jacob Straight Line","intercept t0 with the horizontal axis for the early time asymptote","intercept t1 with the horizontal axis for the late time asymptote", "time tm of the minimum of the derivative")
  c <- c("Transmissivity T (L^2/T)","Storativity Sf (-)","Storativity Sm (-)","Interporosity flow lamda (-)")
  d <- c("Pumping rate Q (L^3/T)", "Distance to the pumping well (L)")
  return(list(a,b,c,d))
}

.war_computeparams <- function(adp,data) {
  if(length(adp)<4) {
    stop("Incorrect length for the adp vector. Length of 3 expected.")
  }
  if(length(adp)>4) {
    warning("Incorrect length for the adp vector. Length of 4 expected. Extra parameters ignored.")
  }
  if(length(data)<2) {
    stop("Incorrect length for the data vector. Length of 2 expected.")
  }
  if(length(data)>2) {
    warning("Incorrect length for the data vector. Length of 2 expected. Extra parameters ignored.")
  }
  Q <- data[1]
  r <- data[2]
  a <- adp[1]
  t0 <- adp[2]
  t1 <- adp[3]
  tm <- adp[4]
  Tf <- 0.1832339*Q/a
  Sf <- 2.245839*Tf*t0/r^2
  Sm <- 2.245839*Tf*t1/r^2-Sf
  sigma <- (t1-t0)/t0
  lambda <- 2.2458394*t0*log(t1/t0)/tm
  return(c(Tf,Sf,Sm,lambda))
}

.war_computeadjustparams <- function(p,data) {
  if(length(p)<4) {
    stop("Incorrect length for the p vector. Length of 4 expected.")
  }
  if(length(p)>4) {
    warning("Incorrect length for the p vector. Length of 4 expected. Extra parameters ignored.")
  }
  if(length(data)<2) {
    stop("Incorrect length for the data vector. Length of 2 expected.")
  }
  if(length(data)>2) {
    warning("Incorrect length for the data vector. Length of 2 expected. Extra parameters ignored.")
  }
  Q <- data[1]
  r <- data[2]
  Tf <- p[1]
  Sf <- p[2]
  Sm <- p[3]
  lambda <- p[4]

  a <- 0.1832339*Q/Tf
  t0 <- Sf*r^2/(2.245839*Tf)
  t1 <- (Sm+Sf)*r^2/(2.245839*Tf)
  tm <- 2.2458394*t0*log(t1/t0)/lambda
  return(c(a,t0,t1,tm))
}


.war_dim <- function( p, t, ...) {
  a <- p[1]
  t0 <- p[2]
  t1 <- p[3]
  tm <- p[4]
  td <- 0.445268*t/t0
  sigma <- (t1-t0)/t0
  lambda <- 2.2458394*t0*log(t1/t0)/tm
  sd <- .war_dls(c(sigma,lambda),td)
  s <- 0.868588963806504*a*sd # initially 0.868589
  return(s)
}

.war_dls <- function( x, td) {
  sd <- .stefhest('war',td, x[1], x[2])
  return(sd)
}

.war_lap<- function( x, p) {
  s <- x[1]
  l <- x[2]
  sd <- 1/p*besselK(sqrt(p+(l*s*p)/(s*p+l)),0)
  return(sd)
}

.war_gss <- function( t, s, ...) {
  res<-.ldiff_spline(t,s)
  td <- res[which(res[,2]>0),1]
  d <- res[which(res[,2]>0),2]
  end <- length(d)
  dd <- mean(d[(end-3):end])
  a <- log(10)*dd
  t0 <- t[1]/exp(s[1]/dd)
  end <- length(t)
  t1 <- t[end]/exp(s[end]/dd)
  tm <- td[which.min(d)]
  return(c(a, t0, t1, tm))
}

.war_std <- function(){
  return(gfamodel("war", aqpar=c(4e-05,3e-06, 5e-05, 4e-02), testdata = c(3.58e-02, 20)))
}
