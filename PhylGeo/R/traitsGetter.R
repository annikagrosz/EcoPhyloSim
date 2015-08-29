getAverageTraits <- function(simu, orderPhylo = T){
  
  if(orderPhylo == T){
    extantPhylogeny <- drop.fossil(simu$phylogeny)
    species =  as.numeric(sapply(list(extantPhylogeny$tip.label), substring, 2))    
  }else{
    species = unique(simu$specMat)
  }
  
  traits <- matrix(nrow = length(species), ncol = 4)
  
  rownames(traits) = species
  colnames(traits) = c("environment", "env trait", "comp trait", "neutral trait")
  
  for (i in 1:length(species)){
    sel = simu$specMat == species[i]
    traits[i,1] = mean(simu$envMat[sel])
    traits[i,2] = mean(simu$traitMat[sel])
    traits[i,3] = mean(simu$compMat[sel])
    traits[i,4] = mean(simu$neutMat[sel])

  }
  return(traits)
}


