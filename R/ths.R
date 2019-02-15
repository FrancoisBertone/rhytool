.ths_details <- function(useless=NULL) {
  a <- "Theis (1935) model"
  b <- c("slope a","intercept t0 (T)")
  c <- c("Transmissivity T (L^2/T)","Storativity S (-)")
  d <- c("Pumping rate Q (L^3/T)", "Distance r between the observation and the pumping well (L)")
  return(list(a,b,c,d))
}

.ths_computeparams <- function(adp,data) {
  if(length(adp)<2) {
    stop("Incorrect length for the adp vector. Length of 2 expected.")
  }
  if(length(adp)>2) {
    warning("Incorrect length for the adp vector. Length of 2 expected. Extra parameters ignored.")
  }
  if(length(data)<2) {
    stop("Incorrect length for the data vector. Length of 2 expected.")
  }
  if(length(data)>2) {
    warning("Incorrect length for the data vector. Length of 2 expected. Extra parameters ignored.")
  }
  Tr <- 0.1832339*data[1]/adp[1]
  S <- 2.2458394*Tr*adp[2]/data[2]^2
  if(length(data)>=3) {
    Ri <- 2*sqrt(Tr*data[3]/S)
    return(c(Tr,S,Ri))
  }
  return(c(Tr,S))
}

.ths_computeadjustparams <- function(p,data) {
  if(length(p)<2) {
    stop("Incorrect length for the p vector. Length of 2 expected.")
  }
  if(length(p)>2) {
    warning("Incorrect length for the p vector. Length of 2 expected. Extra parameters ignored.")
  }
  if(length(data)<2) {
    stop("Incorrect length for the data vector. Length of 2 expected.")
  }
  if(length(data)>2) {
    warning("Incorrect length for the data vector. Length of 2 expected")
  }
  a <- 0.1832339*data[1]/p[1]
  t0 <- (p[2]*data[2]^2)/(2.2458394*p[1])
  return(c(a,t0))
}

#' @importFrom expint expint_E1
NULL

.ths_dim <- function(p,t) {
#  require(expint)
  td <- 0.5625*p[2]/t
  suppressWarnings(s <- p[1]/log(10)*expint_E1(td))
  d <- p[1]/log(10)*exp(-td)
  return(s)
}

.ths_gss <- function(t, s) {
  end <- length(t)
  n <- round(end/3)
  t<-t[n:end]
  s<-s[n:end]
  p <- .jcb_gss(t,s)
  return(p)
}

.jcb_gss <- function(t,s) {
  g <- cbind(log10(t),rep(1,length(t)))
  p <- (solve(t(g)%*%g)%*%t(g))%*%s
  a <- p[1]
  c <- p[2]
  t0 <- 10^(-c/a)
  p[2] <- t0
  return(as.vector(p))
}

.ths_std <- function(){
  return(gfamodel("ths", aqpar=c(1e-4,1e-4), testdata = c(0.1, 10)))
}

