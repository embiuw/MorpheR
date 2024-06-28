#' \code{readMorph} Read morphometrics data form a single drone image
#' @param folder Folder where morphometrics data are stored. 
#' By default, this interactively opens a folder selection window.
#' @param filename File to open. By default, this opens a file selection window of the selected folder.
#' @param length.name Name of variable for total body length. default is 'L'.
#' @param units Units of measurement. By default metric.
#' @details This function reads a morphometrics output file from the Morphometrix2 package:
#' https://github.com/ZappyMan/MorphoMetriX-V2
#' @return A list with morphometrics objects extracted from the file:
#' \item{\code{meta}}{Data frame of metadata pertaining to the image}
#' \item{\code{dimensions}}{List of dimensional data, 
#' including length, widths, as well as estimated area and volume.}
#' @family Whale morphometrics functions
#' @seealso \code{\link{readMorphs}} To read morphometrics data from several images of the same individual.
#' @seealso \code{\link{plotMorph}} Plot body dimensions from single image.
#' @seealso \code{\link{plotMorphs}} Plot body dimensions form multiple images of the same individual.
#' @seealso \code{\link{plotMultiMorphs}} Plot body dimensions form multiple images of several individual.
#' @seealso \code{\link{sumMorphs}} Create data frame of summary statistics for body dimensions of single individual.

#' @author Martin Biuw
#' @examples
#' @export

readMorph <- function(folder='interactive',
                      filename='interactive', 
                      length.name='L',
                      units='Meters') {
  options(warn=-1)
  library(magick)
  odir <- getwd()
  if(folder=='interactive') {
    setwd(choose.dir(getwd()))
  } else {
    setwd(folder)
  }
  filelist <- dir()
  csv.files <- filelist[grep('.csv', filelist)]
  png.files <- filelist[grep('.png', filelist)]
  
  if(filename=='interactive') {
    morph.dat <- file.choose()      
    morph.png <- gsub('.csv', '-measurements.png', morph.dat)
  } else {
    morph.dat <- filename     
    morph.png <- gsub('.csv', '-measurements.png', morph.dat)
  }  
  
  img <- image_read(morph.png)
  print(img)
  morph.dat <- readLines(morph.dat)
  options(warn=0)
  
  val.names <- unlist(strsplit(morph.dat[1], ','))
  meta <- morph.dat[grep('Metadata', morph.dat)]
  meta <- lapply(meta, function(x) unlist(strsplit(x, ','))[c(1,2)])
  meta[[length(meta)+1]] <- c('Units', units)
  meta <- data.frame(do.call('rbind', meta))
  names(meta) <- val.names[c(1,2)]
  
  dimensions <- lapply(grep(units, morph.dat), function(x) {
    unlist(strsplit(morph.dat[x], ','))[c(1,2)]
  })
  
  dimensions <- data.frame(do.call('rbind', dimensions))
  names(dimensions) <- val.names[c(1,2)]
  
  dimensions[,2] <- as.numeric(dimensions[,2])
  
  positions <- gsub('L_w', '', dimensions[-1,1])
  
  ##  positions <- dimensions[1,2] * (0.01*as.numeric(positions))
  positions <- 0.01*as.numeric(positions)
  
  dimensions <- list(length=dimensions[1,2],
                     widths=data.frame(positions=positions,
                                       widths=dimensions[-1,2]))
  
  if(meta$Value[match('Mirror Side', meta$Object)] != 'None') {
    dimensions$widths$widths <- dimensions$widths$widths * 2
  }  
  
  morph.dat <- list(meta=meta, 
                    dimensions=dimensions)
  
  morph.dat$dimensions$widths$standard <- morph.dat$dimensions$widths$widths/morph.dat$dimensions$length
  
  area <- volume <- 0 
  
  for(i in 1:(nrow(morph.dat$dimensions$widths)-1)) {
    if(i==1) {
      area <- area + (morph.dat$dimensions$widths$widths[1] * 
                        (diff(morph.dat$dimensions$widths$positions)[1]*
                           morph.dat$dimensions$length))
      volume <- volume + ((pi*(morph.dat$dimensions$widths$widths[1]/2)^2)*
                            (diff(morph.dat$dimensions$widths$positions)[1]*
                               morph.dat$dimensions$length))
      
    } else {
      area <- area + (mean(morph.dat$dimensions$widths$widths[c(i-1, i)]) * 
                        (diff(morph.dat$dimensions$widths$positions)[1]*
                           morph.dat$dimensions$length))
      volume <- volume + (pi*((mean(morph.dat$dimensions$widths$widths[c(i-1, i)])/2)^2))*
        (diff(morph.dat$dimensions$widths$positions)[1]*
           morph.dat$dimensions$length)
    }
  }
  
  if(diff(tail(morph.dat$dimensions$widths$widths, 2))>0) {
    area <- area + (mean(morph.dat$dimensions$widths$widths[c(i-2, i)]) * 
                      2*((diff(morph.dat$dimensions$widths$positions)[1]*
                            morph.dat$dimensions$length)))
    volume <- volume + (pi*((mean(morph.dat$dimensions$widths$widths[c(i-2, i)])/2)^2))*
      2*((diff(morph.dat$dimensions$widths$positions)[1]*
            morph.dat$dimensions$length))
  } else {
    area <- area + (mean(tail(morph.dat$dimensions$widths$widths, 2)) * 
                      (diff(morph.dat$dimensions$widths$positions)[1]*
                         morph.dat$dimensions$length))
    volume <- volume + (pi*((tail(mean(morph.dat$dimensions$widths$widths))/2)^2))*
      (diff(morph.dat$dimensions$widths$positions)[1]*
         morph.dat$dimensions$length)
    
    area <- area + (morph.dat$dimensions$widths$widths[nrow(morph.dat$dimensions$widths)] * 
                      (diff(morph.dat$dimensions$widths$positions)[1]*
                         morph.dat$dimensions$length))
    
    volume <- volume + ((pi*(morph.dat$dimensions$widths$widths[nrow(morph.dat$dimensions$widths)]/2)^2)*
                          (diff(morph.dat$dimensions$widths$positions)[1]*
                             morph.dat$dimensions$length))
  }
  
  morph.dat$dimensions$area <- area
  morph.dat$dimensions$volume <- volume
  setwd(odir)
  morph.dat
}
