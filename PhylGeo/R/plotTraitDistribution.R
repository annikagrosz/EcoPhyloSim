#' @title Trait plots
#' @description Plots trait-histograms, trait ~ Environment relationship and the spatial distribution of the different traits
#' @param simu Simulation output of the class "Phylosim", usually consisting out of several lists. Needs to contain at least the three trait matrices ($traitMat;$compMat;$neutMat) and the environment matrix ($envMat) 
#' @param which.simulation defines which simulation run to choose in case more than one simulation is saved within the simu object. The default is the last one.
#' @param type defines wether the histogram only should be plotted standalone or with the trait ~ Environment relationship and the spatial distribution. Latter is done by type = "all". Default is "hist"
#' @examples 
#' 
#'#Load data 
#'data(simu.neutral.global)
#'data(simu.neutral.local)
#'data(simu.envcom.local)
#'
#'  --------------------------------------------------------
#'  
#'#neutral model with global dispersal
#'plotTraitDistribution(simu=simu.neutral.global)
#'plotTraitDistribution(simu=simu.neutral.global, type ="all")
#'  --------------------------------------------------------
#'  
#'#neutral model with local dispersal
#'plotTraitDistribution(simu=simu.neutral.local)
#'plotTraitDistribution(simu=simu.neutral.local, type = "all")
#'
#'  --------------------------------------------------------
#'  
#'#evironment and competition model with local dispersal
#'plotTraitDistribution(simu=simu.envcom.local)
#'plotTraitDistribution(simu=simu.envcom.local, type = "all")
#'
#'
#' @export

plotTraitDistribution <- function (simu, type = "hist", which.simulation = NULL){
  
  if (is.null(which.simulation)) which.simulation = length(simu$Output) 
  dat <- simu$Output[[which.simulation]]
  
  names <- unique(as.numeric(dat$specMat))
  names <- unique(as.numeric(dat$specMat))
  p   <- seq(from = 0, to = 1 ,length.out = length(names))
  red <- sample(p, size = length(names))
  green <- sample(p, size = length(names))
  blue <- sample(p, size = length(names))
  for(i in 1:length(names)){
      cols[i] <- rgb(red[i],green[i],blue[i],0.5)}
  colmat <- dat$specMat
  for(i in 1:length(names)){
    colmat[colmat==names[i]] <- cols[i]}
  
  if(type == "all"){
    par(mfrow=c(3,3))
    par(mar = c(5, 4, 2, 2))
    
    plot(dat$traitMat~dat$envMat, col = colmat, ylab="Trait", xlab="Environment"  )
    plot(dat$compMat~dat$envMat, col = colmat, ylab= "", xlab="Environment")
    plot(dat$neutMat~dat$envMat, col = colmat, ylab= "", xlab="Environment")
    
    
    image(dat$traitMat, xaxt="n", yaxt="n", ylab="Spatial Distribution",useRaster =T,col = grey(seq(0, 1, length = 256)))
    image(dat$compMat, xaxt="n", yaxt="n",useRaster =T,col = grey(seq(0, 1, length = 256)))
    image(dat$neutMat, xaxt="n", yaxt="n",useRaster =T,col = grey(seq(0, 1, length = 256)))
  }else{par(mfrow=c(1,3))}
  
  
    
   
    
    ymax <- max(hist(dat$traitMat,breaks=100, plot =F)$counts)
    xmax <- max(dat$traitMat) 
    xmin <- min(dat$traitMat)
    hist(dat$traitMat[dat$specMat==names[1]], xlim=c(xmin,xmax), ylim =c(0,ymax))
    for(i in 2:length(names)){
      hist(dat$traitMat[dat$specMat==names[i]], add =T, col = cols[i])}    
    
    ymax <- max(hist(dat$compMat,breaks=100, plot =F)$counts)
    xmax <- max(dat$compMat) 
    xmin <- min(dat$compMat)
    hist(dat$compMat[dat$specMat==names[1]], xlim=c(xmin,xmax), ylim =c(0,ymax))
    for(i in 2:length(names)){
      hist(dat$compMat[dat$specMat==names[i]], add =T, col = cols[i])} 
    
    ymax <- max(hist(dat$neutMat,breaks=100, plot =F)$counts)
    xmax <- max(dat$neutMat) 
    xmin <- min(dat$neutMat)
    hist(dat$neutMat[dat$specMat==names[1]], xlim=c(xmin,xmax), ylim =c(0,ymax))
    for(i in 2:length(names)){
      hist(dat$neutMat[dat$specMat==names[i]], add =T, col = cols[i])} 
    
  }
  
  


