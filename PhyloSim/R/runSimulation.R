#' @title  Species Community Simulation
#' @description A model of species community assembly under different assembly mechanisms.  
#' @param par, a list of parameters created with \link{createCompletePar}
#' @return An object of class "Phylosim". This objet contains the species matrix, the trait matrix, the environmental matrix, the competition matrix and the neutral matrix, as well as the phlogeny and the parameter settings of the simulation. 
#' @details If your parameterset contains more than one runs argument, each interim step is saved in the Phylosim object. \cr\cr For larger simularions consider \link{runSimulationBatch} to make use of parallel computing. \cr\cr If you are using type="Rneutral" only one runs argument can be processed.
#' @importFrom adephylo distTips
#' @examples 
#'  
#' # Define a parameter set
#' par <- createCompletePar(x = 50, y = 50, dispersal = "global" , runs = c(500,1000),
#'         density = 1, environment = 0.5, specRate = 1)
#'
#' # Run the model
#' simu <- runSimulation(par)
#' 
#' # Look at the phylogeny (requires package 'ape')
#' require(ape)
#'  
#' # Get the phylogeny of the last run. In this example this is after 1000 runs.
#' phylogeny <- simu$Output[[2]]$phylogeny
#'  
#' # Only extant taxa
#' extantPhylogeny <- drop.fossil(phylogeny)
#'  
#' # Display the results  
#' plot(extantPhylogeny)
#'  
#' #Look at the species area relation
#' sac(simu, area = c(1,10,100,1000), rep = 100, plot= TRUE)
#' @useDynLib PhyloSim
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
    
    
    if (is.numeric(par$density)) if(par$density == 0){
      par$density = F
      par$compStrength = 1
    }else{
      par$compStrength = par$density
      par$density = T
    }
    
    if (is.numeric(par$environment)) if(par$environment == 0){
      par$environment = F
      par$envStrength = 1
    }else{
      par$envStrength = par$environment
      par$environment = T
    }
    
    
    if(par$density == 0 & par$environment == 0)
    {
      neutral = TRUE
    }else{
      neutral = F
    }    
    
    ptm <- proc.time()
    
    out <- callModel( par$x,  par$y,  dispersal,  round(par$runs),  par$specRate, par$density, par$environment, neutral, mortalityFitness, par$fitnessBaseMortalityRatio, reproductiveFitness, dispersalCut, par$densityCut, par$seed, par$envStrength, par$compStrength)  
    
    runtime <- as.numeric((proc.time() - ptm)[3])
    
    print (paste("Core simulation finished after",floor(((proc.time() - ptm)[3])/60), "minute(s) and", ((proc.time() - ptm)[3])%%60, "second(s). Converting data"))
    
    #     Rcpp::List listResults = Rcpp::List::create(Rcpp::Named("Species") = specOut,
    #                                                 Rcpp::Named("EnvTrait") = traitOut,
    #                                                 Rcpp::Named("NeutralTrait") = neutralOut,
    #                                                 Rcpp::Named("CompetitionTrait") = compOut,
    #                                                 Rcpp::Named("Environment") = envOut,
    #                                                 Rcpp::Named("Phylogeny") = phyloOut);
    
    output<-list()
    for (i in 1:length(par$runs)){
      
      output$Output[[i]] = list(
        specMat = matrix(out[[i]]$Species,ncol=par$x, nrow=par$y), 
        traitMat= matrix(out[[i]]$EnvTrait,ncol=par$x, nrow=par$y), 
        envMat = matrix(out[[i]]$Environment,ncol=par$x, nrow=par$y), 
        compMat = matrix(out[[i]]$CompetitionTrait,ncol=par$x, nrow=par$y), 
        neutMat = matrix(out[[i]]$NeutralTrait,ncol=par$x, nrow=par$y), 
        phylogeny = ape::read.tree(text = out[[i]]$Phylogeny), 
        phyloTXT = out[[i]]$Phylogeny)
    }
    cat("done! \n")
    
    par$runtime <- runtime
    
    output$Model <- par
    class(output) <- "PhyloSim"
    return(output)
    
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
    ptm <- proc.time()
    result = NeutralMod(xdim = par$x, ydim=par$y, specRate = par$specRate,  seed = par$seed, runs = par$runs)
    runtime <- as.numeric((proc.time() - ptm)[3])
    par$runtime <- runtime
    out = list(Output = list(list("specMat"=result)), Model = par)
    class(out) <- "PhyloSim"
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


