#' \code{readMorphs} Read morphometrics data from 
#' several drone images of the same individual
#' @param folder Folder where morphometrics data are stored. 
#' By default, this interactively opens a folder selection window.
#' @param filenames Files to open. By default, this opens a file selection window of the selected folder.
#' @details This function reads morphometrics output files from 
#' the Morphometrix2 package: https://github.com/ZappyMan/MorphoMetriX-V2.
#' THe function reads output from several images of the same individual
#' @return A list with morphometrics objects extracted from the files, 
#' plus a summary object \item{\code{meanDimensions}}
#' @family Whale morphometrics functions
#' @seealso \code{\link{readMorph}} To read morphometrics data from a single image.
#' @seealso \code{\link{plotMorph}} Plot body dimensions from single image.
#' @seealso \code{\link{plotMorphs}} Plot body dimensions form multiple images of the same individual.
#' @seealso \code{\link{plotMultiMorphs}} Plot body dimensions form multiple images of several individual.
#' @seealso \code{\link{sumMorphs}} Create data frame of summary statistics for body dimensions of single individual.

#' @author Martin Biuw
#' @examples
#' @export

readMorphs <- function(folder='interactive',
                       filenames='interactive') {
  require(Hmisc)
  odir <- getwd()
  if(folder=='interactive') {
    setwd(choose.dir(getwd()))
  } else {
    setwd(folder)
  }
  filelist <- dir()
  
  if(filenames[1]=='interactive') {
    filenames <- choose.files()
  } 
  
  morphs <- list()
  
  for(i in 1:length(filenames)) {
    morphs[[i]] <- readMorph(getwd(), filenames[i])
  }
  
  ##  names(morphs) <- unlist(lapply(filenames, function(x) DescTools::SplitPath(x, last.is.file = T)$filename))
  names(morphs) <- unlist(lapply(filenames, function(x) gtools::split_path(x)[1]))
  
  ##  weights <- unlist(lapply(morphs, function(x) tail(x$meta, 1)))
  ##  weights <- as.numeric(gsub('GT ', '', weights))
  ##  weights <- max(weights)-weights+1
  
  meanDimensions <- list()
  
  ##  meanDimensions$length <- c(Mean=wtd.mean(unlist(lapply(morphs, function(x) x$dimensions$length)), w=weights),
  ##                             SD=sqrt(wtd.var(unlist(lapply(morphs, function(x) x$dimensions$length)), w=weights)))
  
  meanDimensions$length <- c(Mean=mean(unlist(lapply(morphs, function(x) x$dimensions$length))),
                             SD=sd(unlist(lapply(morphs, function(x) x$dimensions$length))))
  
  width.mat <- do.call('rbind', lapply(morphs, function(x) x$dimensions$widths$widths))
  
  stand.mat <- do.call('rbind', lapply(morphs, function(x) x$dimensions$widths$standard))
  
  # meanDimensions$widths <- data.frame(positions=morphs[[1]]$dimensions$widths$positions,
  #                                     meanWidths=apply(width.mat, 2, wtd.mean, w=weights),
  #                                     sdWidths=sqrt(apply(width.mat, 2, wtd.var, w=weights)),
  #                                     meanStandard=apply(stand.mat, 2, wtd.mean, w=weights),
  #                                     sdStandard=sqrt(apply(stand.mat, 2, wtd.var, w=weights)))
  # 
  # meanDimensions$area <- c(Mean=wtd.mean(unlist(lapply(morphs, function(x) x$dimensions$area)), w=weights),
  #                            SD=sqrt(wtd.var(unlist(lapply(morphs, function(x) x$dimensions$area)), w=weights)))
  # 
  # meanDimensions$volume <- c(Mean=wtd.mean(unlist(lapply(morphs, function(x) x$dimensions$volume)), w=weights),
  #                          SD=sqrt(wtd.var(unlist(lapply(morphs, function(x) x$dimensions$volume)), w=weights)))
  
  
  meanDimensions$widths <- data.frame(positions=morphs[[1]]$dimensions$widths$positions,
                                      meanWidths=apply(width.mat, 2, mean),
                                      sdWidths=apply(width.mat, 2, sd),
                                      meanStandard=apply(stand.mat, 2, mean),
                                      sdStandard=apply(stand.mat, 2, sd))
  
  meanDimensions$area <- c(Mean=mean(unlist(lapply(morphs, function(x) x$dimensions$area))),
                           SD=sd(unlist(lapply(morphs, function(x) x$dimensions$area))))
  
  meanDimensions$volume <- c(Mean=mean(unlist(lapply(morphs, function(x) x$dimensions$volume))),
                             SD=sd(unlist(lapply(morphs, function(x) x$dimensions$volume))))
  
  morphs$meanDimensions <- meanDimensions
  setwd(odir)
  morphs
}
