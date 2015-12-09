## Class functions for class Phylosim


#plot.phylosim

#' @title Plot objects of class PhyloSim
#' @description Plots the phylogeny tree ,the spatially abundance of the evolved species and their traits.
#' @param simu Simulation output of \code{\link{runSimulation}}

#' @details The tiplabels of the phylogeny tree are colored in correspondency to the spatial abundance plot

#' @export

plot.PhyloSim <- function(simu){
  plotSpatialPhylo(simu)
}


#summary.phylosim
# TODO: This is still similar to print

#' @title Summary for objects of class PhyloSim
#' @description Gives summary about parameter settings, runtime and species Richness.
#' @param simu Simulation output of \code{\link{runSimulation}}
#' @export

summary.PhyloSim <- function(simu){
  
  if(length(simu$Model$runs)==1){
    runsarg<-simu$Model$runs
  } else{
    runsarg<- "c("
    for(i in 1:(length(simu$Model$runs)-1)){
      runsarg<- paste(runsarg,simu$Model$runs[[i]],",")
    }
    runsarg<- paste(runsarg, simu$Model$runs[[length(simu$Model$runs)]], ")" )
  }
  
  cat("Summary for class Phylosim",  "\n")
  cat("# Call: runSimulation(x = ", simu$Model$x, ", y = ", simu$Model$y, ", dispersal = '", 
      simu$Model$dispersal,"'", ", runs =", runsarg, ", density = ", simu$Model$density, 
      ", environment = ", simu$Model$environment, ", specRate = ",simu$Model$specRate,")", "\n", sep="")
  cat("------------------------------------", "\n", sep="")
  cat("Time elapsed: ", simu$Model$runtime,"seconds", "\n", sep="")
  cat("Number of Species in Metacommunity: ", specRich(simu), sep="")
  }



#print.phylosim 
#' @title Prints summary for objects of class PhyloSim
#' @description Prints parameter settings, runtime and species Richness.
#' @param simu Simulation output of \code{\link{runSimulation}}
#' @export

print.PhyloSim <- function(simu){
  
  if(length(simu$Model$runs)==1){
    runsarg<-simu$Model$runs
  } else{
    runsarg<- "c("
    for(i in 1:(length(simu$Model$runs)-1)){
      runsarg<- paste(runsarg,simu$Model$runs[[i]],",")
    }
    runsarg<- paste(runsarg, simu$Model$runs[[length(simu$Model$runs)]], ")" )
  }
  
  cat("Summary for class PhyloSim",  "\n")
  cat("# Call: runSimulation(x = ", simu$Model$x, ", y = ", simu$Model$y, ", dispersal = '", 
      simu$Model$dispersal,"'", ", runs =", runsarg, ", density = ", simu$Model$density, 
      ", environment = ", simu$Model$environment, ", specRate = ",simu$Model$specRate,")", "\n", sep="")
  cat("------------------------------------", "\n", sep="")
  cat("Time elapsed: ", simu$Model$runtime,"seconds", "\n", sep="")
  cat("Number of Species in Metacommunity: ", specRich(simu), sep="")
  
}


