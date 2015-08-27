#' @title Trait plots
#' @description Plot traits of simulation outputs
#' @param landscape the species landscape
#' @param phylogeny the corresponding (extant) phylogeny
#' @param plot defines what to plot. "both" plots the landscape and phylogeny side-by-side
plotTraitDistribution <- function (simu, type = "hist"){
  
  if (type == "hist"){
    oldpar <- par(mfrow = c(2,2), mar = c(3,3,3,3))
    hist(simu$traitMat)
    hist(simu$envMat)
    hist(simu$compMat)
    hist(simu$neutMat)
    par(oldpar)
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

