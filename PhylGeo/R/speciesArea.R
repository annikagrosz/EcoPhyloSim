#' @title Species-area relation
#' @description Calculates the species-area pattern for a community matrix. 
#' @param area A single value or a vector determining the size(s) of the generated subplots
#' @param matrix The species community
#' @param rep The number of repetitions per size to calculate the mean 
#' @param plot Logical determining whether to plot the SAC or not
#' @return A list containing the mean species richness for each size and the respective standard deviation
sac <- function(area, matrix, rep=5, plot=T)
{
  meanSpeciesRichness <- numeric()
  meanSpeciesRichnessSD <- numeric()
  #Loop over all plot sizes
  for(i in 1:length(area))
  { 
    # Repetitions for each plot size
    subPlots <- localPlots(size=area[i], n = rep, matrix=matrix)$subPlots
    speciesRichness <- sapply(subPlots, specRich)
    meanSpeciesRichness[i] <- mean(speciesRichness)
    meanSpeciesRichnessSD[i] <- sd(speciesRichness)
  }
  
  if(plot == T)
  {
    plot(area,meanSpeciesRichness,type="b", log="xy", xlab="Area (n cells)", ylab="Number of Species", main="SAC", lwd=2, pch=4)
    polygon(x=c(area,rev(area)), y=c(meanSpeciesRichness + 1.96*meanSpeciesRichnessSD,rev(meanSpeciesRichness - 1.96*meanSpeciesRichnessSD)), col="#00000030", border=NA)
    lines(area, (meanSpeciesRichness + 1.96*meanSpeciesRichnessSD), col="red", lty=2, lwd=2)
    lines(area, (meanSpeciesRichness - 1.96*meanSpeciesRichnessSD), col="red", lty=2, lwd=2)
  }
  
  return(data.frame(area = area, sr.Mean = meanSpeciesRichness,sr.SD = meanSpeciesRichnessSD))
}