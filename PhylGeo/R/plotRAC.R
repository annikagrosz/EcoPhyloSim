#' @title Rank Abundance Curve
#' @description Plots the Rank Abundance Curve for a given community. 
#' @param simu Simulation output of the class "Phylosim", usually consisting out of several lists. Needs at least the spatial distribution of the species stored in a matrix ($specMat)
#' @param which.simulation Integer, determines which result should be used. This argument is only usefull if interim steps are saved in the Phylosim object. By default (NULL), the end result is used.
#' @param plot determining whether to plot the RAC as "line"(default) or "bar".
#' @details Each species is given a rank according to their abundance (highest = rank 1). Then the the species' abundance is plotted in dependency of their rank. It can be used as an indicator for the ammount of equally abundant species a community can support.
#' @return A dataframe containing the ranked abundances, sorted by ascending rank.
#' @examples 
#' 
#' #Load data
#' data(simu.neutral.global)
#' data(simu.neutral.local)
#' data(simu.envcom.local)
#' 
#'  
#' 
#' #Plot RAC curve for neutral model and global dispersion
#' 
#' rac(simu = simu.neutral.global)
#' rac(simu = simu.neutral.global, plot ="bar")
#' 
#'  
#' 
#' #Plot RAC curve for neutral model and local dispersion
#' 
#' rac(simu = simu.neutral.local)
#' rac(simu = simu.neutral.local, plot ="bar")
#' 
#'  
#' 
#' #Plot RAC curve for environment and competition model and local dispersion
#' 
#' rac(simu = simu.envcom.local)
#' rac(simu = simu.envcom.local, plot ="bar")
#' 
#'  
#' 
#' @export

rac <- function(simu,which.simulation=NULL, plot="line"){
  
  if (is.null(which.simulation)) which.simulation = length(simu$Output) 
  simu <- simu$Output[[which.simulation]]
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



