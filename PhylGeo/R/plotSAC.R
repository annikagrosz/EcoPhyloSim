
#' @title Species Area Curve
#' @description Plots the species area curve for a given community. 
#' @param simu Simulation output of the class "Phylosim", usually consisting out of several lists. Needs at least the spatial distribution of the species stored in a matrix ($specMat)
#' @param which.simulation Integer, determines which simulation should be used. Only useful if your Phylosim object contains more than one result.
#' @param area a single value or a vector determining the size(s) of the subplots that for which areas are calculated. If not provided, 10 sizes logarithmically will be chosen
#' @param rep The number of repetitions per size to calculate the mean 
#' @param plot Logical determining whether to plot the SAC or not
#' @return A list containing the mean species richness for each size and the respective standard deviation
#' @examples 
#' 
#' #Load data
#' data(simu.neutral.global)
#' data(simu.neutral.local)
#' data(simu.envcom.local)
#' 
#'  --------------------------------------------------------
#'   
#' #Plot SAC curve for neutral model and global dispersion
#' simu <- simu.neutral.global
#' sac(simu = simu)
#' 
#'  -------------------------------------------------------- 
#'  
#' #Plot SAC curve for neutral model and local dispersion
#' simu <- simu.neutral.local
#' sac(simu = simu)
#' 
#'  --------------------------------------------------------
#'  
#' #Plot SAC curve for environment and competition model and local dispersion
#' simu <- simu.envcom.local
#' sac(simu = simu)
#' 
#'  --------------------------------------------------------
#'  
#' #Plot SAC curve with random plotsize 
#' simu <- simu.envcom.local
#' sac(simu=simu, area = sort(sample(c(10:1000), size = 10)))
#' 
#'  -------------------------------------------------------- 
#'  
#' #Plot SAC curve with different repititions
#' simu <- simu.envcom.local
#' par(mfrow=c(3,1))
#' sac(simu=simu, rep = 3)
#' sac(simu=simu, rep = 30)
#' sac(simu=simu, rep = 30)
#' 
#' @export
#' 
sac <- function(simu,which.simulation=NULL, area = NULL, rep=50, plot=T){
  
  
  if (is.null(which.simulation)) which.simulation = length(simu$Output) 
  simu <- simu$Output[[which.simulation]]
  matrix <- simu$specMat
  
  landscapeDim = dim(matrix)
  landscapeArea = landscapeDim[1] * landscapeDim[2]
  if (is.null(area)){
    area = exp(seq(1, log(landscapeArea),len = 10))
  }
  
  area = round(sqrt(area))^2
  
  meanSpeciesRichness <- numeric()
  meanSpeciesRichnessUpperCI <- numeric()
  meanSpeciesRichnessLowerCI <- numeric()
  #Loop over all plot sizes
  for(i in 1:length(area))
  { 
    # Repetitions for each plot size
    subPlots <- localPlots(size=area[i], n = rep, simu=simu)$subPlots
    speciesRichness <- sapply(subPlots, SR)
    meanSpeciesRichness[i] <- mean(speciesRichness)
    meanSpeciesRichnessUpperCI[i] <- quantile(speciesRichness, probs = 0.95, na.rm = T)
    meanSpeciesRichnessLowerCI[i] <- quantile(speciesRichness, probs = 0.05, na.rm = T)
    
  }
  
  if(plot == T)
  {
    plot(area,meanSpeciesRichness,type="b", log="xy", xlab="Area (n cells)", ylab="Number of Species", main="SAC", lwd=2, pch=4)
    polygon(x=c(area,rev(area)), y=c(meanSpeciesRichnessUpperCI,rev(meanSpeciesRichnessLowerCI)), col="#00000030", border=NA)
    lines(area, (meanSpeciesRichnessUpperCI), col="red", lty=2, lwd=2)
    lines(area, (meanSpeciesRichnessLowerCI), col="red", lty=2, lwd=2)
  }
  
  return(data.frame(area = area, sr.Mean = meanSpeciesRichness,sr.UpperCI = meanSpeciesRichnessUpperCI, sr.LowerCI = meanSpeciesRichnessLowerCI))
}

## Internal function called by SAC
SR <- function(matrix){  
  sr <- length(unique(c(matrix)))
  return(speciesRichness = sr)
}

