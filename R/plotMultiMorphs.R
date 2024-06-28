#' \code{plotMultiMorphs} Plot body dimensions from several images of several individuals.
#' @param whale.list Character string representing outputs from \code{readMorphs} from several individuals.
#' @param nCol Number of columns in figure lattice.
#' @param nRow Number of rows in figure lattice.
#' @details This function creates a plot lattice of body dimensions from several images of several individuals. 
#' @return Plot of body dimensions from several images of several individuals.
#' @family Whale morphometrics functions
#' @seealso \code{\link{readMorph}} To read morphometrics data from a single image.
#' @seealso \code{\link{readMorphs}} To read morphometrics data from several images of the same individual.
#' @seealso \code{\link{sumMorphs}} Data frame of summary statistics.
#' @seealso \code{\link{plotMorph}} Plot body dimensions from single image.
#' @seealso \code{\link{plotMorphs}} Plot body dimensions form multiple images of the same individual.

#' @author Martin Biuw
#' @examples
#' @export

plotMultiMorphs <- function(whale.list=c('BP_01_24', 'BP_02_24', 'BP_03_24'),
                            nCol=1, nRow=2) {
  
  whale.names <- whale.list
  eval(parse(text=paste('whale.list <- list(', paste(whale.list, collapse=', '), ')')))
  par(mfrow=c(nRow, nCol), mar=rep(1, 4), oma=c(3,3,1,1))
  
  max.len <- max(unlist(lapply(whale.list, function(i) {
    max(unlist(lapply(i, function(x) x$dimensions$length)))
  })))
  
  if(nCol==1) {
    y.on <- c(1:length(whale.list))
    x.on <- length(whale.list)
  } else {
    y.on <- c(1, 1+(nRow*c(1:nCol)))
    x.on <- c((((nRow-1)*nCol)+1):length(whale.list))
  }
  
  for(i in 1:length(whale.list)) {
    plotMorphs(whale.list[[i]], max.len=max.len, plotAxes=F)
    # box()
    abline(v=par('usr')[1])
    abline(h=par('usr')[3])
    if(i %in% x.on) {
      axis(1, xpd=NA)
    } else {
      axis(1, labels=F)
    }
    
    if(i %in% y.on) {
      axis(2, xpd=NA)
    } else {
      axis(2, labels=F)
    }
    text(max.len/2, par('usr')[4], whale.names[i], pos=1)
  }
  mtext(side=1, line=1.5, outer=T, 'Length (m)')
  mtext(side=2, line=1.5, outer=T, 'Width (m)')
}

