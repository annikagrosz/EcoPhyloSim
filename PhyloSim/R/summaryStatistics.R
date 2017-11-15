
#' @title Summary Statistics
#' @description Calculates the summary satistics for a simulation of class PhyloSim.
#' @param simulation Simulation output of the class "Phylosim"
#' @param strict Logical. If true the output of the function will be NA if at least one summary statistic could not be calculated.
#' @details TODO: implement
#' @return A list containing the summary statistics.
#' @examples 
#' 
#' simu <- runSimulation(createCompletePar(x=50, y=50, runs=1000, specRate=2))
#' summStat <- calculateSummaryStatistics(simu)
#' 
#' 
#' @export
#' 

calculateSummaryStatistics <- function(simulation, strict=TRUE) {
  summaryStatistics <- list(racAuc=NA,
                            sacAuc=NA,
                            alphaDiversity = NA,
                            betaDiversity = NA,
                            gammaDiversity = NA,
                            imbalance = NA,
                            dispersion = NA,
                            gammaStatistics = NA,
                            meanNodeAge = NA,
                            varPart_1 = NA,
                            varPart_2 = NA,
                            ce = NA,
                            nb8 = NA)
  
  # TODO: implement special case: only 1 species
  if (is.double(simulation$Output[[1]]$phylogeny)) {
    return(summaryStatistics)
  }
  
  # RAC
  rac <- rac(simulation, plot = F)
  summaryStatistics$racAuc <- MESS::auc(x = rac$Rank,
                                        y = rac$Abundance)
  
  # SAC
  sac <- sac(simulation, size = seq(1,15), plot = F)
  summaryStatistics$sacAuc <- MESS::auc(x = sac$size,
                                        y = sac$sr.Mean)
  
  
  # alpha and beta diversity
  
  # Draw random plots
  # TODO: let the user chose the plot size and number of plots
  nSubPlots <- 20
  sizeSubPlots <- 10
  
  plots <- localPlots(simulation,
                      n = nSubPlots,
                      size = sizeSubPlots,
                      plot = F,
                      community = T)
  
  # alpha diversity
  # in this case alpha diversity = mean number of species per subplot
  summaryStatistics$alphaDiversity <- mean(rowSums(plots$communityTable>0))
  
  # beta diversity
  # uses vegan's beta diversity function
  # see betadiver(help=TRUE)
  # TODO: let the user chose the method
  method = 1
  summaryStatistics$betaDiversity <- mean(vegan::betadiver(plots$communityTable,
                                                           method=method))
  
  # gamma diversity
  summaryStatistics$gammaDiversity <- gammaDiversity(simulation)
  
  # phylogenetic imbalance
  # uses apTreeshape's colless function
  
  # phylo object must be converted to treeshape object
  tsPhylogeny <- apTreeshape::as.treeshape.phylo(ape::drop.fossil(simulation$Output[[1]]$phylogeny))
  
  # apTreeshape::colless only works on trees with more than 2 tips
  if(nrow(tsPhylogeny$merge) != 1) {
    # more than 2 tips
    # normalization of the colless index
    # TODO: let the user chose the normalization
    normalization = "yule"
    #normalization = "pda"
    #normalization = NULL
    
    summaryStatistics$imbalance <- apTreeshape::colless(tsPhylogeny,
                                                        norm = normalization)
  } else {
    # less than 2 tips
    # WARNING
    # This does not seem to make sense
    
    # copied from apTreeshape::colless
    # necessary, because apTreeshape does not implement colless for trees
    # with only two tips
    norm.yule <- function(ICN, tree) {
      leaf_nb <- nrow(tree$merge) + 1
      EICN <- leaf_nb * log(leaf_nb) + (0.57721566 - 1 - log(2)) * 
        leaf_nb
      IC <- (ICN - EICN)/(leaf_nb)
      IC
    }
    
    # TODO: implement pda and NULL normalization
    summaryStatistics$imbalance <- norm.yule(0, tsPhylogeny)
  }
  
  # phylogenetic dispersion
  # uses PhyloMeasures' implementation of the NRI
  
  # Commented code below causes error. Drop fossil seems to drop clades that are no real fossils?!
  #summaryStatistics$dispersion <- mean(PhyloMeasures::mpd.query(ape::drop.fossil(simulation$Output[[1]]$phylogeny),
  #                                                              plots$communityTable,
  #                                                              standardize = TRUE))
  
  summaryStatistics$dispersion <- mean(PhyloMeasures::mpd.query(simulation$Output[[1]]$phylogeny,
                                                                plots$communityTable,
                                                                standardize = TRUE))
  
  # gamma statistics
  # uses ape's implementation of the gammaStatistics
  # attention: only works on ultrametric trees
  summaryStatistics$gammaStatistics <- ape::gammaStat(ape::drop.fossil(simulation$Output[[1]]$phylogeny))
  
  # mean node age
  #summaryStatistics$meanNodeAge <- ape::chronoMPL(simulation$Output[[1]]$phylogeny)
  summaryStatistics$meanNodeAge <- mean(ape::drop.fossil(simulation$Output[[1]]$phylogeny)$edge.length)
  
  
  # variation partitioning
  #distance_matrix <- distances(plots$positions, limits=c(nrow(simulation$Output[[1]]$specMat), ncol(simulation$Output[[1]]$specMat)))
  env <- data.frame(unlist(lapply(plots$envPlots, mean)))
  comp <- data.frame(unlist(lapply(plots$compPlots, mean)))
  
  #mod <- vegan::varpart(plots$communityTable, as.dist(distance_matrix), env, comp)
  mod <- vegan::varpart(plots$communityTable, env, comp)
  
  summaryStatistics$varPart_1 <- mod$part$fract$Adj.R.squared[1]
  summaryStatistics$varPart_2 <- mod$part$fract$Adj.R.squared[1]
  
  
  # Clark Evans
  summaryStatistics$ce <- clarkEvans(simulation$Output[[1]]$specMat, correction="Donnelly")
  
  # nb8
  summaryStatistics$nb8 <- nb8Idx(simulation$Output[[1]]$specMat)
  
  
  
  if (strict == TRUE) {
    if (sum(is.na(summaryStatistics)) > 0) {
      summaryStatistics <- NA
    }
  }
  
  return(summaryStatistics)
}

gammaDiversity <- function(simulation, q=0){
  spec_vector <- c(simulation$Output[[1]]$specMat)
  specs <- unique(spec_vector)
  n_specs <- length(specs)
  
  if (q == 0) return(n_specs)
  
  n_individuals <- length(spec_vector)
  
  prop_abundances <- rep(NA, length(specs))
  
  for(i in 1:length(specs)){
    prop_abundances[i] <- sum(spec_vector == specs[i]) / n_individuals
  }
  
  gamma <- prop_abundances * prop_abundances^(q-1)
  gamma <- sum(gamma)^(1/(q-1))
  gamma <- 1/gamma
  return(gamma)
}

distances <- function(positions, limits){
  distance <- function(a, b, limits){
    # sqrt(min(|x1 - x2|, w - |x1 - x2|)^2 + min(|y1 - y2|, h - |y1-y2|)^2)
    return(sqrt(min(abs(a[1]-b[1]), limits[1] - abs(a[1]-b[1]))^2 + min(abs(a[2]-b[2]), limits[2] - abs(a[2]-b[2]))^2))
  }
  dist_mat <- matrix(0, ncol=length(positions), nrow=length(positions))
  # TODO: maybe there is a better way to implement this instead of a for loop
  for(i in 1:nrow(dist_mat)){
    for(j in 1:ncol(dist_mat)){
      dist_mat[i,j] <- distance(positions[[i]], positions[[j]], limits=limits)
    }
  }
  return(dist_mat)
}

clarkEvans <- function (mat, ...) {
  nCol <- ncol(mat)
  nRow <- nrow(mat)
  
  ids <- unique(as.vector(mat))
  ceSum <- 0
  n <- 0
  
  for (i in 1:length(ids)) {
    pos <- which(mat == ids[i])
    col <- ceiling(pos / nRow)
    row <- pos %% nCol
    pp <- spatstat::ppp(x=col, y=row, window=spatstat::owin(xrange = c(0, nCol), yrange = c(0, nRow)))
    ce <- spatstat::clarkevans(pp, ...)
    if (!is.infinite(ce) && !is.na(ce) && !is.nan(ce)) {
      ceSum <- ceSum + ce
      n <- n + 1
    }
  }
  return(ceSum/n)
}

getNeighbours8 <- function(x, y, mat, nCol=NULL, nRow=NULL, isTorus=TRUE) {
  if (is.null(nCol)) nCol <- ncol(mat)
  if (is.null(nRow)) nRow <- nrow(mat)
  
  neighbours <- vector("numeric")
  
  for (yOffset in c(-1, 0, 1)) {
    for (xOffset in c(-1, 0, 1)) {
      if (!(yOffset == 0 && xOffset == 0)) {
        xPos <- x + xOffset
        yPos <- y + yOffset
        
        if (xPos > nCol || xPos < 1 || yPos > nRow || yPos < 1) {
          if (isTorus != TRUE) next
        }
        
        if (xPos > nCol) {
          xPos <- xPos - nCol
        } else if (xPos < 1) {
          xPos <- nCol - xPos
        }
        
        if (yPos > nRow) {
          yPos <- yPos - nRow
        } else if (yPos < 1) {
          yPos <- nRow - yPos
        }
        
        neighbours <- append(neighbours, mat[yPos, xPos])
      }
    }
  }
  return(neighbours)
}

nb8Idx <- function (mat) {
  nCol <- ncol(mat)
  nRow <- nrow(mat)
  n <- nRow * nCol
  
  ids <- unique(as.vector(mat))
  
  id_freqs <- vector(mode = "numeric", length = length(ids))
  for (i in 1:length(ids)) {
    id_freqs[i] <- sum(mat == ids[i])
  }
  
  # print(nRow)
  # print(nCol)
  
  nbRates <- vector("numeric", length(ids))
  names(nbRates) <- ids
  
  for (row in 1:nRow) {
    for (col in 1:nCol) {
      id <- mat[row, col]
      nbrs <- getNeighbours8(col, row, mat, nRow = nRow, nCol = nCol)
      
      r <- sum(id == nbrs) / length(nbrs)
      # r <- r * (id_freqs[which(ids == id)] / n)
      # r <- r * (n / id_freqs[which(ids == id)])
      r <- r / id_freqs[which(ids == id)]
      nbRates[as.character(id)] <- nbRates[as.character(id)] + r
    }
  }
  
  # return(sum(nbRates) / length(nbRates))
  return(sum(nbRates))
}
