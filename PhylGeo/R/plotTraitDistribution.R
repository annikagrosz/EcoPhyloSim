#' @title Trait plots
#' @description Plots trait-histograms, trait ~ Environment relationship and the spatial distribution of the different traits
#' @param simu Simulation output of the class "Phylosim", usually consisting out of several lists. Needs to contain at least the three trait matrices ($traitMat;$compMat;$neutMat) and the environment matrix ($envMat) 
#' @param which.simulation defines which simulation run to choose in case more than one simulation is saved within the simu object. The default is the last one.
#' @export

plotTraitDistribution <- function (simu, type = "hist", which.simulation = NULL){
  
  if (is.null(which.simulation)) which.simulation = length(simu) - 1
  
  dat <- simu[[which.simulation]]
  
  if (type == "hist"){
    par(mfrow = c(3,3))
    par(mar = c(5, 4, 2, 2))
    hist(dat$traitMat, breaks = 100,xlab="Environmenttrait", main = "Environmenttrait", ylab = "Frequency")
    hist(dat$compMat, breaks = 100,xlab = "Competitiontrait",main = "Competitiontrait", ylab="" )
    hist(dat$neutMat, breaks = 100,xlab="Neutraltrait", main = "Neutraltrait", ylab="")
    
    plot(dat$traitMat~dat$envMat, col = dat$specMat, ylab="Trait", xlab="Environment"  )
    plot(dat$compMat~dat$envMat, col = dat$specMat, ylab= "", xlab="Environment")
    plot(dat$neutMat~dat$envMat, col = dat$specMat, ylab= "", xlab="Environment")
    
    
    image(dat$traitMat, xaxt="n", yaxt="n", ylab="Spatial Distribution")
    image(dat$compMat, xaxt="n", yaxt="n")
    image(dat$neutMat, xaxt="n", yaxt="n")
  }else if(type == "phylo"){
    require(phytools)
    
    #     contMap(simu$phylogeny, x, res=100, fsize=NULL, ftype=NULL, lwd=4, legend=NULL,
    #             lims=NULL, outline=TRUE, sig=3, type="phylogram", direction="rightwards", 
    #             plot=TRUE, ...)
    #     
    #     x<-data.frame(species = paste("s", as.vector(simu$specMat)) , )
    #     
    #     trait.plot(tree, dat, cols, lab=names(cols), str=0:1, class=NULL,
    #                type="f", w=1/50, legend=length(cols) > 1, cex.lab=.5,
    #                font.lab=3, cex.legend=.75, margin=1/4,
    #                check=TRUE, quiet=FALSE)
    #     
  }
}

