#' \code{plotMorphs} Plot body dimensions from several images of single individual.
#' @param morphs Output from \code{readMorphs}.
#' @param drawWidths Logical, whether or not to draw width lines.
#' @param plotMean Logical, whether or not to also plot the mean outline.
#' @param lwd Line width for mean outline.
#' @param mCol Color for mean outline.
#' @param plotAxes Logical, whether or not to draw axes.
#' @param max.len Used to specify length axis extent (default is to set automatically). 
#' @details This function creates a plot of body dimensions from several images of the same individual. 
#' @return Plot of body dimensions from several images.
#' @family Whale morphometrics functions
#' @seealso \code{\link{readMorph}} To read morphometrics data from a single image.
#' @seealso \code{\link{readMorphs}} To read morphometrics data from several images of the same individual.
#' @seealso \code{\link{sumMorphs}} Data frame of summary statistics.
#' @seealso \code{\link{plotMorph}} Plot body dimensions from single image.
#' @seealso \code{\link{plotMultiMorphs}} Plot body dimensions form multiple images of several individual.

#' @author Martin Biuw
#' @examples
#' @export

plotMorphs <- function(morphs, drawWidths=F, plotMean=T, lwd=1, 
                       mCol=1, plotAxes=T, max.len='default') {
  if(max.len=='default') {
    xLim <- max(unlist(lapply(morphs[-length(morphs)], function(x) x$dimensions$length)))
  } else {
    xLim=max.len
  }
  plotMorph(morphs[[1]], xLim=xLim, plotAxes=plotAxes, asLines=F)
  for(i in 2:(length(morphs)-1)) plotMorph(morphs[[i]], add=T, drawWidths=drawWidths, asLines=F)
  if(plotMean) {
    lines(c(0, morphs$meanDimensions$widths$positions, 1) * morphs$meanDimensions$length[1],
          0.5*c(0, morphs$meanDimensions$widths$meanWidths, 0), lwd=lwd, col=mCol)
    lines(c(0, morphs$meanDimensions$widths$positions, 1) * morphs$meanDimensions$length[1],
          -0.5*c(0, morphs$meanDimensions$widths$meanWidths, 0), lwd=lwd, col=mCol)
  }  
}

