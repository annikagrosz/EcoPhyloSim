# indexAnalysis


```r
require(PhylGeo)
```

```
## Loading required package: PhylGeo
## Loading required package: foreach
```

```r
require(picante)
```

```
## Loading required package: picante
## Loading required package: ape
## Loading required package: vegan
## Loading required package: permute
## Loading required package: lattice
## This is vegan 2.3-0
## Loading required package: nlme
```

```r
  fileName <- "globalNeutral"
  file <- paste("C:/Users/Paul/Desktop/Data/",fileName, ".rdata", collapse="", sep="")
  load(file)
```


```r
  plotSize <- (c(2,3,4,5,6,7,8,9,10,20,30,40))^2
  numberOfPlots <- floor(256^2/plotSize)
  
   mat <- matrix(OUT$specOut, ncol=256, nrow=256)
   phylo <- read.tree(text = OUT$phyloOut)
   extantPhylo <- drop.fossil(phylo)
   extantPhyloCophen <- cophenetic(extantPhylo)
  
  sesMNTD <- list()
  sesMPD <- list()
  for(i in 1:length(numberOfPlots))
  {
    comMat <- localPlots(size = plotSize[i], n = numberOfPlots[i], matrix = mat, community = T)$communityTable
    sesMNTD[[i]] <- ses.mntd(comMat, extantPhyloCophen, "phylogeny.pool",runs=999, iterations=1000)  
    sesMPD[[i]] <- ses.mpd(comMat, extantPhyloCophen, "phylogeny.pool",runs=999, iterations=1000)  
  }
  
  testNames <- paste("test_", plotSize, "_", numberOfPlots, sep="")
```


```r
  meanPvaluesMNTD <- numeric()
  meanPvaluesMPD <- numeric()
  pValueListMNTD <- list()
  pValueListMPD <- list()
  
 for(i in 1:length(sesMNTD))
 { 
   meanPvaluesMNTD[i] <-mean(sesMNTD[[i]]$mntd.obs.p)
   pValueListMNTD[[i]] <- sesMNTD[[i]]$mntd.obs.p
 }
  
 for(i in 1:length(sesMPD))
 { 
   meanPvaluesMPD[i] <-mean(sesMPD[[i]]$mpd.obs.p)
   pValueListMPD[[i]] <- sesMPD[[i]]$mpd.obs.p
 }

  names(pValueListMPD) <- names(pValueListMNTD) <- testNames
```


```r
  Cols <- rainbow(length(numberOfPlots))
  hist(meanPvaluesMNTD, main = "MNTD", col = Cols)
```

![](indexAnalysis_files/figure-html/unnamed-chunk-4-1.png) 

```r
  hist(meanPvaluesMPD, main = "MPD", col = Cols)
```

![](indexAnalysis_files/figure-html/unnamed-chunk-4-2.png) 

```r
  boxplot(pValueListMNTD, ylim = c(-0.1, 1.1), col = Cols, border = "grey30", main = "MNTD", las=2)
```

![](indexAnalysis_files/figure-html/unnamed-chunk-4-3.png) 

```r
  boxplot(pValueListMPD, ylim = c(-0.1, 1.1), col = Cols, border = "grey30", main = "MPD", las=2)
```

![](indexAnalysis_files/figure-html/unnamed-chunk-4-4.png) 

