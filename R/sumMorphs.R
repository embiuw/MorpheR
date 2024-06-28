#' \code{sumMorphs} Data frame of summary statistics.
#' @param morphList List of outputs from running \code{readMorphs} on several individuals. 
#' @param Species Character string of species name.
#' @details This function creates a data frame with summary statistics from several individuals.
#' Suitable for use as a table for papers/reports etc.
#' @return Data frame of summary statistics of body dimensions from several individuals.
#' @family Whale morphometrics functions
#' @seealso \code{\link{readMorph}} To read morphometrics data from a single image.
#' @seealso \code{\link{readMorphs}} To read morphometrics data from several images of the same individual.
#' @seealso \code{\link{plotMorph}} Plot body dimensions from single image.
#' @seealso \code{\link{plotMorphs}} Plot body dimensions form multiple images of the same individual.
#' @seealso \code{\link{plotMultiMorphs}} Plot body dimensions form multiple images of several individual.

#' @author Martin Biuw
#' @examples
#' @export

sumMorphs <- function(morphList, Species=NA) {
  morph <- data.frame(
    ID=unlist(lapply(morphList, function(x) unlist(strsplit(names(x)[1], '_DJI'))[1])),
    Species=rep(Species, length(morphList)))
  
  morph <- cbind(morph, do.call('rbind', lapply(morphList, function(x) {
    x$meanDimensions$length
  })))
  
  names(morph)[length(morph)-c(1,0)] <- c('muLength', 'sdLength')
  
  
  morph <- cbind(morph, do.call('rbind', lapply(morphList, function(x) {
    whichMax <- which.max(x$meanDimensions$widths$meanWidths)
    x$meanDimensions$widths[whichMax,]
  })))
  
  morph <- cbind(morph, do.call('rbind', lapply(morphList, function(x) {
    x$meanDimensions$volume
  })))
  
  names(morph)[length(morph)-c(1,0)] <- c('muVolume', 'sdVolume')
  
  morph
}

