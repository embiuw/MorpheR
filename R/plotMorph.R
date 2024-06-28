#' \code{plotMorph} Plot body dimensions from single image.
#' @param morph.dat Output from \code{readMorph}. 
#' @param add Logical, whether or not a new plot of body dimensions should be created (default) 
#' or added to existing plot. 
#' @param drawWidths Logical, whether or not to draw width lines.
#' @param asLines Logical, whether or not to draw body outlines (default) or only width points.
#' @param col Color for outlines or points. By default, these are randomly assigned.
#' @param lwd Line width for outlines.
#' @param xLim Specify limits in the length direction (default is to set automatically). 
#' @param lwd Logical, whether or not to plot axes.
#' @details This function creates a plot of body dimensions from a single image. 
#' @return Plot of body dimensions from single image.
#' @family Whale morphometrics functions
#' @seealso \code{\link{readMorph}} To read morphometrics data from a single image.
#' @seealso \code{\link{readMorphs}} To read morphometrics data from several images of the same individual.
#' @seealso \code{\link{sumMorphs}} Data frame of summary statistics.
#' @seealso \code{\link{plotMorphs}} Plot body dimensions form multiple images of the same individual.
#' @seealso \code{\link{plotMultiMorphs}} Plot body dimensions form multiple images of several individual.

#' @author Martin Biuw
#' @examples
#' @export

plotMorph <- function(morph.dat, add=F, drawWidths=F, asLines=T, 
                      col='random', lwd=2, xLim=NA, plotAxes=T) {
  if(col=='random') {
    lCol <- randomcoloR::randomColor(1)
  } else {
    lCol  <- col
  }
  if(!add) {
    if(is.na(xLim)) {
      xLim <- morph.dat$dimensions$length
    }
    
    par(mar=c(par('mar')[c(1,2)], 1, 1))
    plot(c(0, morph.dat$dimensions$widths$positions, 1)*morph.dat$dimensions$length,
         c(0, morph.dat$dimensions$widths$widths/2, 0), asp=1, type='n',
         xlab='Total length (m)', ylab='Width (m)', xlim=c(0, xLim), axes=plotAxes)
  }
  
  if(drawWidths) {
    lCol.alpha <- col2rgb(lCol)/256 
    lCol.alpha <- rgb(lCol.alpha[1],
                      lCol.alpha[2],
                      lCol.alpha[3], 
                      0.3)
    for(i in 1:length(morph.dat$dimensions$widths$positions)) {
      lines(rep(morph.dat$dimensions$widths$positions[i]*morph.dat$dimensions$length, 2),
            morph.dat$dimensions$widths$widths[i]*c(-0.5, 0.5), col=lCol.alpha)
    }
  }  
  if(asLines) {
    lines(c(0, morph.dat$dimensions$widths$positions, 1)*morph.dat$dimensions$length,
          c(0, morph.dat$dimensions$widths$widths/2, 0), lwd=lwd, col=lCol)
    
    lines(c(0, morph.dat$dimensions$widths$positions, 1)*morph.dat$dimensions$length,
          c(0, -morph.dat$dimensions$widths$widths/2, 0), lwd=lwd, col=lCol)
  } else {
    points(c(0, morph.dat$dimensions$widths$positions, 1)*morph.dat$dimensions$length,
           c(0, morph.dat$dimensions$widths$widths/2, 0), pch=21, bg=lCol, cex=0.7)
    
    points(c(0, morph.dat$dimensions$widths$positions, 1)*morph.dat$dimensions$length,
           c(0, -morph.dat$dimensions$widths$widths/2, 0), pch=21, bg=lCol, cex=0.7)
  }
  
  abline(h=0, lty=2)
}
