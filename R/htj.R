.htj_details <- function(useless=NULL) {
  a <- "Hantush and Jacob (1955) model"
  b <- c("slope a","intercept t0","r/B")
  c <- c("Transmissivity T (L^2/T)","Storativity S (-)","Aquitard conductivity (L/T)")
  d <- c("Pumping rate Q (L^3/T)", "Distance r to the pumping well (L)","Thickness of the aquitard (L)")
  return(list(a,b,c,d))
}

.htj_computeparams <- function(adp,data) {
  if(length(adp)<3) {
    stop("Incorrect length for the adp vector. Length of 3 expected.")
  }
  if(length(adp)>3) {
    warning("Incorrect length for the adp vector. Length of 3 expected. Extra parameters ignored.")
  }
  if(length(data)<3) {
    stop("Incorrect length for the data vector. Length of 2 expected.")
  }
  if(length(data)>3) {
    warning("Incorrect length for the data vector. Length of 2 expected. Extra parameters ignored.")
  }
  Tr <- 0.1832339*data[1]/adp[1]
  S <- 2.2458394*Tr*adp[2]/data[2]^2
  B <- adp[2]/adp[(3)]
  Ka <- Tr*data[3]/B^2
  return(c(Tr,S,Ka))
}

.htj_computeadjustparams <- function(p,data) {
  if(length(p)<3) {
    stop("Incorrect length for the aquifer parameters aqpar vector. Length of 3 expected.")
  }
  if(length(p)>3) {
    warning("Incorrect length for the aquifer parameters aqpar vector. Length of 3 expected. Extra parameters ignored.")
  }
  if(length(data)<3) {
    stop("Incorrect length for the pumping data testdata vector. Length of 2 expected.")
  }
  if(length(data)>3) {
    warning("Incorrect length for the pumping data testdata vector. Length of 2 expected. Extra parameters ignored.")
  }
  a <- 0.1832339*data[1]/p[1]
  t0 <- p[2]*data[2]^2/(2.2458394*p[1])
  rB <- data[2]/sqrt(p[1]*data[3]/p[3])
  return(c(a,t0,rB))
}

.htj_lap <- function( x, p){
  a <- x[1]
  s <- besselK(sqrt(a^2+p),0)/p
  return(s)
}

.htj_dls <- function( x, td) {
  s <- .stefhest("htj", td, x[1])
  return(s)
}

.htj_dim <- function( p, t) {
  a <- p[1]
  t0 <- p[2]
  rd <- p[3]
  s <- .htj_dls(rd,0.445268*t/t0)
  s <- 0.868589*a*s
  if(length(t)!=length(s)){
    stop("Ca ne marche pas!!!")
  }
  return(s)
}

.htj_gss <- function( t, s) {
  end <- length(t)
  n <- floor(max(end)/3)
  earlydd <- drawdowns(t[1:n], s[1:n])
  p <- .ths_gss( t[1:n], s[1:n] )
  gthsmod <- gfamodel("ths", aspar=p)
  thsmod <- fit.gfamodel(gthsmod, earlydd )
  p <- thsmod@aspar$val
  sm <- s[end]
  p[3] <- exp(-sm/p[1]*2.3/2+0.1)
  if (p[3] > 1) {
    p[3] <- -log(sm/p[1]*2.3/2)
  }
  return(p)
}

.htj_std <- function(){
  return(gfamodel("htj", aqpar=c(1e-4,5e-4, 1e-6), testdata = c(0.1, 10,3)))
}

