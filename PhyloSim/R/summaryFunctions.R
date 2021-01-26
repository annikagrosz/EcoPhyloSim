#' Calculate summary statistics from a phylogenetic tree
#' 
#' @param object an object of class phylo 
#' 
#' @author Florian Hartig, modified from a function by Allen Hulbert
#' 
#' 
#' @details 
#' 
#' See also 
#' 
#' https://www.lirmm.fr/MEP05/talk/14_Mooers.pdf
#' 
#' https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5790134/
#' 
#' 
treeMetrics = function(phy) {
  
  if(class(phy) != "phylo") {
    stop("The object passed to treeMetrics() is not of class 'phylo'")
  }
  
  require(ape)
  require(caper)
  require(geiger)
  require(picante)
  require(apTreeshape)
  # require(dispRity)
  require(RPANDA)
  require(nLTT)
  require(stringr)
  require(Rfast)
  require(castor)
  
  
  ##### Tree preparation #####
  
  # Drop root edge
  phy$root.edge = 0
  
  # Prune out extinct species
  treeExtant = ape::drop.fossil(phy)
  
  # Randomly resolve any existing polytomies
  tree = ape::multi2di(treeExtant)
  
  ##### Calculation of Statistics #####
  
  # Richness
  S = length(tree$tip.label)
  log10S = log10(S)
  
  # TODO - unclear to me why this works!
  
  # Absolute tree length, which is the diagonal of the vcv matrix
  v.matrix = ape::vcv(tree, corr=F)
  tree.length = diag(v.matrix)[1]
  
  # Tree with branch lengths scaled by total tree length
  tree.scaled = tree
  tree.scaled$edge.length = tree$edge.length/tree.length
  
  # TODO: why not use pd(samp, tree, include.root=TRUE)
  
  # PD (phylogenetic diversity, summed branch lengths) on scaled tree
  PD = sum(tree.scaled$edge.length)
  
  # Create treeshape objects for certain calculations
  tree.shape = as.treeshape(tree)
  tree.shape.scaled = as.treeshape(tree.scaled)
  
  #Pybus & Harvey (2000)'s gamma statistic
  gamma.stat = gammaStat(tree.scaled)
  
  # beta
  # Calculate Blum & Francois (2006)'s Beta metric of tree imbalance using apTreeshape package
  
  # tryCatch({
  #   beta.out = maxlik.betasplit.AH(tree.scaled)
  #   beta.stat = beta.out$max_lik
  #   
  # }, error = function(e) {
  #   beta.stat = NA
  # })
  
  # Mean Pairwise Distance (in scaled tree)
  tryCatch({
    pairwise.dists = dist.nodes(tree.scaled)
    sum.pairwise.dists = upper_tri(pairwise.dists, diag = FALSE, suma = TRUE)
    MPD = sum.pairwise.dists/(ncol(pairwise.dists)*(ncol(pairwise.dists)-1)/2)
  }, error = function(e) {
    MPD = NA
  })
  
  # Variance Pairwise Distance (in scaled tree)
  tryCatch({
    tri.pairwise.dists = upper_tri(pairwise.dists, diag = FALSE, suma = FALSE)
    VPD = var(as.vector(tri.pairwise.dists))
  }, error = function(e) {
    VPD = NA
  })
  
  # Some metrics are difficult to calculate on very large trees.
  # Also calculate RPANDA spectral density metrics (Lewitus & Morlon 2016)
  
  # RPANDA analyses seem to bonk on very large phylogenies, so only try calculating for fewer than 6000 species
  if(S < 6000) {
    
    # RPANDA
    tryCatch({
      MGL = spectR(tree)
      MGL_principal_eigenvalue = MGL$principal_eigenvalue 
      MGL_asymmetry = MGL$asymmetry  
      MGL_peakedness = MGL$peakedness
      MGL_eigengap = MGL$eigengap
    }, error = function(e) {
      MGL_principal_eigenvalue = NA
      MGL_asymmetry = NA
      MGL_peakedness = NA
      MGL_eigengap = NA
    })
    
  } else {
    MGL_principal_eigenvalue = NA
    MGL_asymmetry = NA  
    MGL_peakedness = NA
    MGL_eigengap = NA
  }
  
  # Colless index
  Colless = colless(tree.shape)
  
  # Sackin index
  Sackin = sackin(tree.shape)
  
  # logarithm of the ratio of the likelihoods under the Yule model and the PDA model
  Yule.PDA.ratio = shape.statistic(tree.shape)
  
  # Mean Root Distance (MRD, also abbreviated N-bar, Shao & Sokal 1990) (Agapow & Purvis 2002)
  # using castor package
  # --get_all_distances_to_root returns tip and node distances; only want tip distances which are returned first
  
  root.dist <- get_all_distances_to_root(tree, as_edge_count = TRUE)[1:(tree$Nnode+1)]
  MRD = mean(root.dist)
  
  # Variance in root distances across the tips, Shao & Sokal 1990 (Agapow & Purvis 2002)
  VRD = var(root.dist)
  
  # Calculate Helmus et al. 2007's Phylogenetic Species Variability (PSV) metric
  # based on the phylogenetic variance/covariance matrix
  # While Helmus et al.'s equation is as follows
  # PSV = (n*trace(C) - sum(C))/(n*(n-1)) 
  # this only holds for a tree with contemporaneous tips where C is the correlation matrix.
  # When the tree is not ultra-metric, Algar et al. 2009 (Ecol Lett) show the calculation
  # using the variance-covariance matrix, V:
  # PSV = (n*trace(V) - sum(V))/(trace(V)*(n-1)) 
  
  n = nrow(v.matrix)
  PSV = (n*sum(diag(v.matrix)) - sum(v.matrix))/(sum(diag(v.matrix))*(n-1))
  
  # mean I' from Purvis et al. 2002. Evaluating phylogenetic tree shape: two modifications to Fusco & Cronk's method
  if(tree$Nnode >= 3) {
    tree$node.label = NULL
    fusco = caper::fusco.test(tree)
    mean.Iprime = fusco$mean.Iprime
  } else {
    mean.Iprime = NA
  }
  
  # nLTT statistic on first example tree
  utils::data(exampleTrees, package = "nLTT") # Set of birth-death trees
  nLTT_stat <- nLTT::nLTTstat(
    tree1 = exampleTrees[[1]], # Can be changed to generated Yule tree or any other tree
    tree2 = tree
  )
  
  return(list(
              S = S, 
              log10S = log10S, 
              tree.length = tree.length, 
              PD = PD, 
              Gamma = gamma.stat, 
              # Beta = beta.stat, 
              Colless = Colless, 
              Sackin = Sackin, 
              Yule.PDA.ratio = Yule.PDA.ratio, 
              MRD = MRD, 
              VRD = VRD, 
              PSV = PSV, 
              mean.Iprime = mean.Iprime,
              MPD = MPD, VPD = VPD, 
              MGL_principal_eigenvalue = MGL_principal_eigenvalue, 
              MGL_asymmetry = MGL_asymmetry, 
              MGL_peakedness = MGL_peakedness, 
              MGL_eigengap = MGL_eigengap, 
              nLTT_stat = nLTT_stat
              ))
}




