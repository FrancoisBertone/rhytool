.stefhest <- function(model, t, x1, x2=NA, N=12){
  V <- rep(NA, N)
  for(i in 1:N) {
    vi_dummy <- 0
    for(k in trunc((i+1)/2):min(i,N/2)) {
      if(k==1) {
        a <- 1
      }
      else {
        a <- prod(1:(k-1))
      }
      if(k==i) {
        b <- 1
      }
      else {
        b <- prod(1:(i-k))
      }
      if((2*k)==i) {
        c <- 1
      }
      else {
        c <- prod(1:(2*k-i))
      }
      if(k==N/2) {
        d <- 1
      }
      else {
        d <- prod(1:(N/2-k))
      }
      vi_dummy <- vi_dummy + (k^(N/2)*prod(1:(2*k)))/(d*prod(1:k)*a*b*c)
    }
    V[i] <- (-1)^((N/2)+i)*vi_dummy
  }
  i <- c(1:N)
  end <- length(t)
  p <- (i*log(2))%*%t(1/t)
  fn <- paste0(".",model,"_lap")
  up <- do.call(fn,list(c(x1,x2),p))
  VV <- matrix(V,N,end,byrow=FALSE)
  su <- VV*up
  s <- log(2)/t*apply(su,2,sum)
  return(s)
}
