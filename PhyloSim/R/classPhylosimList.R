
#' @title Plot all objects in an object of class PhylosimList
#' @description Plots the phylogeny tree ,the spatially abundance of the evolved species and their traits.
#' @param simu Simulation output of \code{\link{runSimulationBatch}}

#' @details The tiplabels of the phylogeny tree are colored in correspondency to the spatial abundance plot

#' @export

plot.PhylosimList <- function(simu){
  for(i in 1:(length(simu)-1)){
    plotSpatialPhylo(simu[[i]])
    cat ("Press [enter] to continue")
    line <- readline()
  } 
  plotSpatialPhylo(simu[[length(simu)]])
}


#print.phylosimlist 
#' @title Prints summary for objects of class PhyloSim
#' @description Prints parameter settings, runtime and species Richness.
#' @param simu Simulation output of \code{\link{runSimulation}}
#' @export

print.PhylosimList <- function(simu){
  cat("Summary for class PhylosimList",  "\n")
  cat("------------------------------------", "\n","------------------------------------", "\n")
  cat("# Simu", "X", "Y", "Dispersal", "Density", "Environment", "#Species","#Runs","\n")
  cat("------------------------------------", "\n")
  cat()
  for(i in 1:length(simu)){
    cat("#",i," ",simu[[i]]$Model$x, simu[[i]]$Model$y," ",simu[[i]]$Model$dispersal,"     ",
        as.numeric(simu[[i]]$Model$density),"      ", 
        as.numeric(simu[[i]]$Model$environment),"      ", specRich(simu[[i]]), sum(simu[[i]]$Model$runs), "\n")
  }
}


# TODO summary.PhylosimList
