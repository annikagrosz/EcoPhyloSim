# This file provides a wrapper function (in R) for the PhylSimInterface-function callModel (in C++).

callPhylSimModelDefault<-function(runs = 5,    
                           x = 10,
                           y = 10,
                           dispersal = 1,
                           specRate = 0.7,
                           dens = 0,
                           env = 0,
                           mort = 0,
                           repro = 1,
                           neutral = 1,
                           dispersalCutoff = 1,
                           densityCutoff = 2,
                           seed = 10 ) {
  specOut = integer(x * y)
  traitOut = numeric(x * y)
  neutralOut = numeric(x * y)
  compOut = numeric(x  * y)
  envOut  = numeric(x * y)
  phyloOut = character(x * y)
  
  callModel(as.integer(x), as.integer(y), as.integer(dispersal), as.integer(runs),
            as.double(specRate), as.logical(dens), as.logical(env), as.logical(mort),
            as.logical(repro), as.logical(neutral), as.integer(dispersalCutoff), 
            as.integer(densityCutoff), as.integer(seed),
            as.vector(specOut, mode="integer"), as.vector(traitOut, mode="numeric"), 
            as.vector(neutralOut, mode="numeric"), as.vector(compOut, mode="numeric"), 
            as.vector(envOut, mode="numeric"), as.vector(phyloOut, mode="character"))
  
  return (as.list(specOut, traitOut, neutralOut, compOut, envOut, phyloOut))
}
