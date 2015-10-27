#' @title Rank Abundance Curve
#' @description Plots the Rank Abundance Curve for a given community. 
#' @param simu Simulation output of the class "Phylosim", usually consisting out of several lists. Needs at least the spatial distribution of the species stored in a matrix ($)
#' @param plot determining whether to plot the RAC as "line"(default) or "bar".
#' @details The highest abundance is assigned rank "1" while the lowest is assigned the rank corresponding with species richness.
#' @return A dataframe containing the ranked abundances, sorted by ascending rank.
#' @export

rac <- function(simu,which.simulation=NULL, plot="line"){
  
  if (is.null(which.simulation)) which.simulation = length(simu) - 1
  simu <- simu[[which.simulation]]
  matrix <- simu$specMat
  
  Abundances <- as.data.frame(table(matrix))
  sel <- order(Abundances$Freq, decreasing=T)
  RAC <- data.frame(Rank = seq(1,length(Abundances$Freq),1), Abundance = Abundances$Freq[sel], Species = Abundances$matrix[sel])
  if(plot=="bar"){
    barplot(RAC$Abundance, log="y",ylab="Log Abundance", xlab="Rank", main="RAC", names.arg = RAC$Rank)
  }
  if(plot=="line"){
    plot(RAC$Rank, RAC$Abundance, type="l",log="y",ylab="Log Abundance", xlab="Rank", main="RAC", lwd=2)
  }
  
  return(RAC) 
}



