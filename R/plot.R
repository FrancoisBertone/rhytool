#' Class "drawdowns"
#'
#' Function that plots drawdowns objects.
#' @param x drawdowns object to be plotted
#' @param model gfamodel object to be superposed to \verb{x}. Data are calculated for the \verb{x} time series. if NULL no model is plotted.
#' @param log character string which contains "x" if the x axis is to be logarithmic, "y" if the y axis is to be logarithmic and "xy" or "yx" if both axes are to be logarithmic.
#' @param col colors vector for the x data, model data, x derivative and model derivative respectively. If there are fewer colors than 4, color are recycled.
#' @param derivative logical. If TRUE the derivatives for \verb{x} argument, and for \verb{model} argument if any, are ploted.
#' @param legend logical. If TRUE a lengend is printed.
#' @param leg.pos single keyword setting the location of the legend, from the list "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right" and "center".
#' @importFrom graphics title axis grid lines points
#' @details Plot drawdowns and derivative of drawdown curves for the \verb{x} object. If a the \verb{model} argument is set, supperpose a line for the corresponding drawdowns and derivative.
#' @export
setMethod(f="plot",
          signature="drawdowns",
          definition=function(x, model=NULL, log="xy", col=c("blue4","aquamarine4","deepskyblue3","darkolivegreen3"), derivative=TRUE, legend=TRUE, leg.pos="bottomright", unit.slope=FALSE, ylab="Drawdown",xlab="Time") {
            longcol <- rep(col,4)
            td <- x@ldiff$dt[which(x@ldiff$ds>0)]
            sd <- x@ldiff$ds[which(x@ldiff$ds>0)]
            end <- length(x@dd$t)
            MyCol="#666666"
            if(derivative) {
              ymin = min(c(x@dd$s),sd)
              ymax = max(c(x@dd$s),sd)
            }
            else {
              ymin = min(x@dd$s)
              ymax = max(x@dd$s)
            }
            plot(x@dd$t,x@dd$s,log=log,type="p",pch=21, cex=0.9, col=col[1],xaxt = "n", xlab="", ylab="", ylim=c(ymin,ymax), fg=MyCol,fg= MyCol, col.lab= MyCol, col.axis=MyCol, mgp=c(1.7,0.6,0))
            title(line=1.6, col.lab = MyCol, ylab=ylab,xlab=xlab, cex.lab=1.2)
            i.max <- 1 + trunc(log10(max(x@dd$t)))
            axis(1, at=(10^(0:i.max)), col.axis=MyCol, labels=TRUE, tick=TRUE, mgp=c(1.7,0.6,0))
            grid(col = MyCol, lty = "dotted", lwd = 0.8)
            lty=c(0)
            pch=c(1)
            lwd=c(1)
            col=longcol[1]
            legcar=c("Drawdown (measure)")
            if(!is.null(model)){
              tplot <- 10^seq(log10(x@dd$t[1]),log10(x@dd$t[end]),len=50)
              calc <- drawdowns(tplot, model=model, miscDdOptions=list(defunc="std"))
              dsc <- calc@ldiff$ds
              tdc <- calc@ldiff$dt
              if( mean(sd)<0 ) {
                sd <- -sd
                dsc <- -dsc
              }
              tdc <- tdc[which(dsc>0)]
              dsc <- dsc[which(dsc>0)]
              lines(calc@dd$t,calc@dd$s,type="l", col=longcol[3],lwd=3.0)
              legcar=c(legcar, "Drawdown (model)")
              lty=c(lty, 1)
              pch=c(pch, NA)
              lwd=c(lwd, 2)
              col=c(col,longcol[3])
            }
            if(derivative) {
              legcar=c(legcar, "Derivative from measured drawdown")
              lty=c(lty, 0)
              pch=c(pch, 3)
              lwd=c(lwd, 1)
              col=c(col,longcol[2])
              points(td,sd, type="p",pch=3, cex=0.9, col=longcol[2])
              if(!is.null(model)){
                points(tdc,dsc, type="l",lwd=3.0, col=longcol[4])
                legcar=c(legcar, "Derivative from model drawdown")
                lty=c(lty, 1)
                pch=c(pch, NA)
                lwd=c(lwd, 2)
                col=c(col,longcol[4])
              }
            }
            if(unit.slope){
              .plot.unit.slope(x@dd$t,x@dd$s)
            }
            if(legend){
              legend(leg.pos,lty=lty, pch=pch,lwd=lwd, cex=0.8,legend=legcar, inset=0.02, col=col, bg="white")
            }
            })

#' @importFrom NISTunits NISTradianTOdeg
.plot.unit.slope <- function(t,s){
  x1 <- 10^(log10(t[1]) + (log10(t[2])-log10(t[1]))/2)
  y1 <- 10^(log10(s[1]) + (log10(s[2])-log10(s[1]))/2)
  B = log10(y1)-log10(x1)
  x <- c(min(t),max(t))
  lines(x, y=(x*(10^B)), lty="dotdash", cex=0.7)
  pin<-par("pin")
  usr<-par("usr")
  unit.x<-pin[1]/(usr[2]-usr[1])
  unit.y<-pin[2]/(usr[4]-usr[3])
  angle= NISTradianTOdeg(atan(unit.y/unit.x))
  text(t[4], t[4]*(10^B), label="Unit slope line", srt=angle, pos=4, offset = -1.5)
}


#' Class "gfamodel"
#'
#' Function that plots gfamodel objects.
#' @param x gfamodel object to be plotted
#' @param title logical. Indicates if the analytical model type is to be printed
#' @param legend logical. Indicates if the legend type is to be printed
#' @param log character string which contains "x" if the x axis is to be logarithmic, "y" if the y axis is to be logarithmic and "xy" or "yx" if both axes are to be logarithmic.
#' @importFrom graphics title legend lines
#' @details Plot standard drawdowns and derivative of drawdown curves for the analytical model set in the \verb{x} object.
#' @export
setMethod(f="plot",
          signature="gfamodel",
          definition=function(x, title=TRUE, legend=TRUE, log="xy") {
            dd.obj <- drawdowns(t = (10^seq(1,5,length=40)), model = x) # model)
            plot(dd.obj@dd,log=log,type="l",lwd=1.3, col="gray46",ann=FALSE, xaxt = "n", yaxt="n", ylim=c( max(5e-1, min(dd.obj@ldiff$ds)) , max(dd.obj@dd$s) ))
            lines(dd.obj@ldiff, lty=5,lwd=1.3, col="gray46")
            if(legend) {
              legend("topleft", legend=c("drawdowns", "derivative of the drawdown"), lty=c(1,5), bty="n")
            }
            if(title){
              title(main=x@description)
            }
          })
