.thn_details <- function(useless=NULL) {
  a <- "Theis (1935) model with a no flow boundary"
  b <- c("slope a","intercept t0","intercept ti")
  c <- c("Transmissivity T (L^2/T)","Storativity S (-)","Distance to image well ri (L)")
  d <- c("Pumping rate Q (L^3/T)", "Distance r to the pumping well (L)")
  return(list(a,b,c,d))
}

.thn_computeparams <- function(adp,data) {
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

.thn_computeadjustparams <- function(p,data) {
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

.thn_gss <- function( t, s) {
  #Automatic identification of the "control" points
  res <- .ldiff_spline(t,s) #First log derivative
  td <- res[which(res[,2]>0),1]
  d <- res[which(res[,2]>0),2]
  resd<-.ldiff_spline(td,d) #second log derivative
  tdd <- resd[which(resd[,2]>0),1]
  sdd <- resd[which(resd[,2]>0),2]
  #Calculation of the parameters of the model
  i <- which.max(sdd)
  ti <- tdd[i]
  #Slope of Jacob's straight line
  end <- length(d)
  a <- d[end]*2.3/2
  #Origin of jacob straight line
  end <- length(t)
  t0 <- 10^((a*log10(t[end]*t[end]/ti)-s[end])/a)
  return(c(a,t0,ti))
}

.thn_dim <- function(p,t) {
  s <- .ths_dim(c(p[1],p[2]),t) + .ths_dim(c(p[1],p[3]),t)
  return(s)
}

.thn_std <- function(){
  return(gfamodel("thn", aqpar=c(1e-4,1e-4, 40), testdata = c(0.1, 10)))
}

