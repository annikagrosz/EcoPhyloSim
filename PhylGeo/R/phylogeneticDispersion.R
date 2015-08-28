
.calculatePhylogeneticDispersion <- function(simu, plotlength = 5, plots = 250, replicates = 500 , type = "PhylMeta", fun = mpd){
  plotsize = plotlength^2
  pvalues = list()
  for(i in 1:length(simu)){
    
    s <- simu[[i]]$specMat
    phyl <- simu[[i]]$phylogeny
    extantPhylo <- drop.fossil(phy = phyl)
    extantPhyloDis <- cophenetic(extantPhylo)
    
    if (type == "PhylMeta"){
      pvalues[[i]] <- nullModel(speciesMatrix = s, localPlotSize = plotsize, phylogeny = extantPhylo, numberOfPlots = plots, repetitions = replicates, fun = mpd)
    }else if (type == "PhylSample"){
      pvalues[[i]] <- nullModelSample(speciesMatrix = s, localPlotSize = plotsize, phylogeny = extantPhylo, numberOfPlots = plots,repetitions = replicates)    
    }else if (type == "PhylPool"){
      comMat <- localPlots(size = plotsize, n = plots, matrix = s, community = T)$communityTable
      pvalues[[i]] <- ses.mpd(samp = comMat, dis = extantPhyloDis, null.model = "phylogeny.pool", abundance.weighted = TRUE, runs = replicates)$mpd.obs.p      
    }else if (type == "SamplePool"){
      comMat <- localPlots(size = plotsize, n = plots, matrix = s, community = T)$communityTable
      pvalues[[i]] <- ses.mpd(samp = comMat, dis = extantPhyloDis, null.model = "sample.pool", abundance.weighted = TRUE, runs = replicates)$mpd.obs.p
      
    }
  }
  names(pvalues) <- names(simu)
  return(pvalues)
}

# Version that accepts multiple types
calculatePhylogeneticDispersion <- function(simu, plotlengths = 10,  plots = 250, replicates = 500, types = "PhylMeta", fun = mpd, reduce = T){
  
  pValues = list(list())
  
  for (i in 1:length(types)){
    for (j in 1:length(plotlengths)){
      pValues[[i]][[j]] = .calculatePhylogeneticDispersion(simu, plotlength = plotlengths[j], plots = plots, replicates = replicates, type = types[i], fun = fun)
    }
    names(pValues[[i]]) = plotlengths
  }
  names(pValues) = types
  if (reduce == T){
    if (length(plotlengths) == 1){
      for (i in 1:length(types)){
        pValues[[i]]= pValues[[i]][[1]]
      }
    }
    if (length(types) == 1) pValues = pValues[[1]]       
  }
  return(pValues)
}





plotPhylogeneticDispersion <- function(pvalues, pars, title = "P-values", multiple = T){
  
  if(multiple == T){
    lengths = as.numeric(names(pvalues))
    nlengths = length(lengths)
    lRange <- max(lengths) - min(lengths)
    
    z <- matrix(nrow = nlengths, ncol = length(pvalues[[1]]), dimnames = list(names(pvalues), names(pvalues[[1]])))
    zCILOW <- z
    zCIUP <- z
    for (i in 1:nlengths){
      z[i,] <- sapply(pvalues[[i]], median, na.rm = T)
      zCILOW[i,] <- sapply(pvalues[[i]], function(x)quantile(x,0.25, na.rm = T))   
      zCIUP[i,] <- sapply(pvalues[[i]], function(x)quantile(x,0.75, na.rm = T))
    }
  }else{
    z <- sapply(pvalues, median, na.rm = T)
    zSD <- sapply(pvalues, sd, na.rm = T) 
  }
  
  lev = c(0, rev(unique(pars$dispersal)[order(unique(pars$dispersal))]))
  if (lev[length(lev)] == 0) lev = lev[1:(length(lev)-1)]
  
  dispersalValues <- factor(pars$dispersal, levels = lev)
  fitnessValues <- factor( pars$density - pars$environment + 2* pars$density * pars$environment, labels = c("Env", "Neutral", "Dens", "Both"))
  
  ColPalet <- colorRampPalette(c("turquoise4", "white", "palevioletred"))
  Cols <- ColPalet(100)
  index <- seq(0,1,0.01)
  par(mar=c(0, 2, 0, 0), xpd=TRUE)
  emptyplot(xlim=c(0, 3.5), ylim=c(0,3.5), frame.plot = FALSE)
  title(title, line=-2)
  
  
  
  for(i in 1:length(unique(dispersalValues))){
    for(j in 1:length(unique(fitnessValues))){
      disp = unique(dispersalValues)[i]
      fitn = unique(fitnessValues)[j]
      k <- which(dispersalValues == disp & fitnessValues == fitn)
      x <- as.numeric(fitn) / 2
      y <- (length(unique(dispersalValues)) - as.numeric(disp) + 1) / 2
      
      if (multiple == T){
        
        mpv <- mean(z[,k])
        filledrectangle(wx = 0.3, wy = 0.3, col = Cols[which.max(index[index <= mpv])], mid = c(x, y), angle = 0)
        
        xval <- (lengths - min(lengths)) / lRange *0.3 -0.15
        yval <- z[,k] * 0.3 - 0.15
        yUP <- zCIUP[,k] * 0.3 - 0.15 
        yLOW <- zCILOW[,k] * 0.3 - 0.15
        
        polygon(c(x+xval, rev(x+xval)), c(y+yUP, rev(y+yLOW)), col = "gray", border = NA)
        lines(x+xval, y+yval)
        
      }else{    
        filledrectangle(wx = 0.3, wy = 0.3, col = Cols[which.max(index[index <= z[k]])], mid = c(x, y), angle = 0)
        text(x = x, y = y+0.05, labels = round(z[k],digits = 3))
        text(x = x, y = y-0.05, labels = round(zSD[k],digits = 3))        
      }
    }
  }
  
  text(x = 0, y = seq(0.5,2.5,0.5), labels = rev(levels(dispersalValues)) , pos = 4) 
  text(x = seq(0.5,2,0.5), y = 2.8, labels = levels(fitnessValues) ) 
  
  barx <- 2.5
  bary <- 1.5
  barl <- 0.45
  colorbar.plot(x = 2.5, y = bary, strip=seq(0,1,0.001), col=Cols, strip.length = barl, horizontal = F, strip.width = 0.05)
  text(x = barx + 0.2, y = bary + 1.8* barl, labels = "overdispersed" , pos = 4) 
  text(x = barx + 0.2, y = bary, labels = "neutral" , pos = 4) 
  text(x = barx + 0.2, y = bary - 1.8* barl, labels = "underdispersed \n(clustered)", pos = 4) 
}

