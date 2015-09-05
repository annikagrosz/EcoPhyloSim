#' @title  Species community Simulation
#' @description A model of species community assembly under different assembly mechanism
#' @examples 
#' # Run the model
#' myModel <- fullMod(x = 50, y = 50, dispersal = 1, runs = 100, specRate = 1.0, density = FALSE, environment = FALSE, neutral = TRUE, seed = 1500)
#' 
#'  # Look at the phylogeny (requires package 'ape')
#'  require(ape)
#'  
#'  # Get the phylogeny
#'  phylogeny <- myModel$phylogeny
#'  
#'  # Only extant taxa
#'  extantPhylogeny <- drop.fossil(phylogeny)
#'  
#'  plot(extantPhylogeny)
#'  
#'  #Look at the species area relation
#'  
#'  species <- myModel$specMat
#'  sac(area = c(1,10,100,1000), matrix = species, rep = 100, plot= T)
#' @export
runSimulation <- function(par)
{
  
  # GENERAL SETTINGS
  
  if (par$type == "base"){
    
    if (par$dispersal == "global" | par$dispersal == 0){
      dispersal = 1
      dispersalCut = 1
    }else if (is.numeric(par$dispersal)){
      if (par$dispersal < 0.5) stop("dispersal parameter too small") 
      dispersalCut = 2*par$dispersal
      dispersal = 3
    }else stop("wrong dispersal parameter") 
    
    if(par$fitnessActsOn == "mortality"){
      reproductiveFitness = F
      mortalityFitness = T    
    }else if(par$fitnessActsOn == "reproduction"){
      reproductiveFitness = T
      mortalityFitness = F    
    }else if (par$fitnessActsOn == "both"){
      reproductiveFitness = T
      mortalityFitness = T    
    }else stop("wrong fitness parameters")
    
    if(par$density == FALSE & par$environment == FALSE)
    {
      neutral = TRUE
    }else{
      neutral = F
    }    
    
    ptm <- proc.time()
    
    out <- callModel( par$x,  par$y,  dispersal,  par$runs,  par$specRate, par$density, par$environment, neutral, mortalityFitness, par$fitnessBaseMortalityRatio, reproductiveFitness, dispersalCut, par$densityCut, par$seed)  
    
    print (paste("Core simulation finished after",floor(((proc.time() - ptm)[3])/60), "minute(s) and", ((proc.time() - ptm)[3])%%60, "second(s). Converting data"))
    
    for (i in 1:length(par$runs)){

      out[[i]] = list(
        specMat = matrix(out[[i]][[1]],ncol=par$x, nrow=par$y), 
        traitMat= matrix(out[[i]][[2]],ncol=par$x, nrow=par$y), 
        envMat = matrix(out[[i]][[3]],ncol=par$x, nrow=par$y), 
        compMat = matrix(out[[i]][[4]],ncol=par$x, nrow=par$y), 
        neutMat = matrix(out[[i]][[5]],ncol=par$x, nrow=par$y), 
        phylogeny = ape::read.tree(text = out[[i]][[6]]), 
        phyloTXT = out[[i]][[6]])
    }
    cat("done! \n")
    
    
    if (length(par$runs) == 1) out = out[[i]]
    out$par = par
    class(out) <- append(class(out),"Phylosim")
    return(out)
  
  ##################################################################
  }else if(par$type == "Leipzig"){
    
    # TODO ... NOT FUNCTIONAL YET !!!
    out <- .C(callLeipzig, as.integer(x),as.integer(y), as.integer(dispersal), as.integer(nSpec),as.integer(specRate), as.integer(runs), as.logical(density), as.numeric(densityStrength), specOut = as.integer(outVec), densOut = as.numeric(outVec))[9:10]
    
    specMat = matrix(out[[1]],ncol=x, nrow=y)
    densMat = matrix(out[[2]],ncol=x, nrow=y)
    print (paste("Finished after",floor(((proc.time() - ptm)[3])/60), "minute(s) and", ((proc.time() - ptm)[3])%%60, "second(s)."))
    return(list(specMat = specMat, densMat=densMat))
  
  ###################################################################
  }else if (par$type == "Rneutral"){
    result = NeutralMod(dim = par$x, specRate = par$specRate,  seed = par$seed, runs = par$runs )
    out = list(specMat = result, par = par)
    class(out) <- append(class(out),"Phylosim")
    return(out)
  }
}



# neutral = T
# x = 50
# y = 50
# dispersal = 0.5
# runs = c(100,200)
# specRate = 1.0
# density = F
# environment = F
# seed = 1
# fitnessBaseMortalityRatio = 5
# fitnessActsOn = "mortality" 
# densityCut = 1
# saveTimes = "last"
# 
# 
# if (dispersal == "global" | dispersal == 0){
#   dispersal = 1
#   dispersalCut = 1
# }else if (is.numeric(dispersal)){
#   if (dispersal < 0.5) stop("dispersal parameter too small") 
#   dispersalCut = 2*dispersal
#   dispersal = 3
# }else stop("wrong dispersal parameter") 
# 
# if(fitnessActsOn == "mortality"){
#   reproductiveFitness = F
#   mortalityFitness = T    
# }else if(fitnessActsOn == "reproduction"){
#   reproductiveFitness = T
#   mortalityFitness = F    
# }else if (fitnessActsOn == "both"){
#   reproductiveFitness = T
#   mortalityFitness = T    
# }else stop("wrong fitness parameters")
# 
# if(density == FALSE & environment == FALSE)
# {
#   neutral = TRUE
# }else{
#   neutral = F
# }
# if (length(runs)>1){
#   if (any(runs[-length(runs)] > runs[-1])) stop( "wrong argument to runs")
# }
# 
# 
# out <- callModel( x,  y,  dispersal,  runs,  specRate, density, environment, neutral, mortalityFitness, fitnessBaseMortalityRatio, reproductiveFitness, dispersalCut, densityCut, seed)  


