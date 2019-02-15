#' Plot typical response curves
#'
#' Plot one or several typical response to pumping curve(s) for known groundwater flow analytical models.
#' @param mod character or list of characters. A keyword or list of keywords from the list of known analytical model: "ths","thc", "thn", "blt", "war","htj". Can also be a gfamodel class object.
#' @param cex.legend numeric. factor for sizing the legend.
#' @param log character string which contains "x" if the x axis is to be logarithmic, "y" if the y axis is to be logarithmic and "xy" or "yx" if both axes are to be logarithmic.
#' @details  If \verb{mod} is a gfamodel class object, only the model type of the \verb{mod} object is used for plot. If available in the \verb{mod} object, analytical solution parameters are ignored.
#' @seealso \code{\link{gfamodel}}
#' @importFrom graphics text close.screen par screen split.screen strwidth
#' @export
curve.gfamodel <- function( mod = list("ths","thc", "thn", "blt", "war","htj", "grg", "grf"), cex.legend=0.8, log="xy") {
  def.par <- graphics::par(no.readonly = TRUE)
  if(class(mod)=="gfamodel"){
    mod <- mod@model
  }
  mod <- as.list(c(aa="ths", mod))
  mod<-mod[-1]
  if(!is.na(match("grf", mod))){
    indice <- grep("grf", mod)
    mod <- mod[-indice]
    mod <- c(mod, list("grfinf2", "grfsup2"))
  }
  typicalsize <- c(0.2,0.25,0.35,0.6,0.7,0.9,1.1)
  l <- length(mod)
  nblines <- ceiling(l/2)
  LEGENDSIZE <- typicalsize[nblines]
  lh <- 1/(nblines+LEGENDSIZE)
  par(mar=c(0.3, 0.8, 0.3, 0.8), bg="white")
  mat<-matrix(c(0, 1, 0, lh*LEGENDSIZE), nrow=1)
  if(l==1){
    mat <- rbind(mat,c(0,1,lh*LEGENDSIZE, 1), c(0,1,lh*LEGENDSIZE, 1))
  } else {
    for(i in 1:nblines){
      mat <- rbind(mat,c(0.5,1,lh*LEGENDSIZE+(i-1)*lh, lh*LEGENDSIZE+i*lh), c(0,0.5,lh*LEGENDSIZE+(i-1)*lh, lh*LEGENDSIZE+i*lh) )
    }
  }
  split.screen(mat)
  string <- "Standard groundwater flow analytical models in log-log presentation (plain line is describing the drawdowns, and dotted line is describing the derivative of the drawdown):"
  indice <- ""
  if(l > 1) indice <- paste0(letters,")")
  #draw typical curves
  for(i in 1:l) {
    screen(2*(nblines+1)-i)
    dimfn <- paste0(".",mod[[i]],"_std")
    gfamod <- do.call(dimfn, list())
    plot(gfamod, legend=FALSE,title=FALSE, log=log)
    if(l>1) legend("topleft", indice[i], bty="n")
    string <- paste(string, indice[i], paste0(gfamod@description,";"))
  }
  # Write description
  screen(1)
  substr(string, nchar(string), nchar(string)) <- "."
  plot(-1, -1, xlim=c(0,1), ylim=c(0,1), axes=F, xlab="", ylab="")
  repeat{
    nbchar <- floor(1.05*nchar(string)/strwidth(string, units ="user", cex=cex.legend))
    s <- paste(strwrap(string, width = nbchar), collapse = "\n")
    h <- strheight(s, units ="user", cex=cex.legend)
    cex.legend <- cex.legend * 0.99
    if(h < 1){
      cex.legend <- cex.legend / 0.99
      break
    }
  }
  text(-0.04,0.5, s, adj=0, cex=cex.legend)
  graphics::close.screen(all = TRUE)
  graphics::par(def.par)
}




