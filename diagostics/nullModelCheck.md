# nullModelCheck


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
require(PhylGeo)
```

```
## Loading required package: PhylGeo
## Loading required package: foreach
```

```r
require(shape)
```

```
## Loading required package: shape
```

```r
require(fields)
```

```
## Loading required package: fields
## Loading required package: spam
## Loading required package: grid
## Spam version 1.0-1 (2014-09-09) is loaded.
## Type 'help( Spam)' or 'demo( spam)' for a short introduction 
## and overview of this package.
## Help for individual functions is also obtained by adding the
## suffix '.spam' to the function name, e.g. 'help( chol.spam)'.
## 
## Attaching package: 'spam'
## 
## The following objects are masked from 'package:base':
## 
##     backsolve, forwardsolve
## 
## Loading required package: maps
```

```r
files = c("globalEnv", "globalNeutral", "globalDensNN", "globalBothNN","local8Env", "local8Neutral", "local8DensNN", "local8BothNN", "local4Env", "local4Neutral", "local4DensNN", "local4BothNN","local8BothWide","local8DensWide", "local4BothWide","local4DensWide")

outList <- list()
for(i in 1:length(files)){
  file <- paste("C:/Users/Paul/Desktop/Data/",files[i], ".rdata", collapse="", sep="")
  load(file)
  outList[[i]] <- OUT
   
}
names(outList) <- files
rm(file, files, i, OUT)
```


```r
# Phylogeny Pool Picante 
pvalsPhyloPicante <- list()
for(i in 1:length(outList)){
  s <- matrix(outList[[i]]$specOut, nrow = 256, ncol = 256)
  comMat <- localPlots(size = 100, n = 100, matrix = s, community = T)$communityTable
  phyl <- read.tree(text = outList[[i]]$phyloOut)
  extantPhylo <- drop.fossil(phy = phyl)
  extantPhyloDis <- cophenetic(extantPhylo)
  pvalsPhyloPicante[[i]] <- ses.mpd(samp = comMat, dis = extantPhyloDis, null.model = "phylogeny.pool", abundance.weighted = TRUE, runs = 1000)$mpd.obs.p
}

names(pvalsPhyloPicante) <- names(outList)

#Phylogeny Pool PhylGeo
pvalsPhyloGeo <- list()
for(i in 1:length(outList)){
  s <- matrix(outList[[i]]$specOut, nrow = 256, ncol = 256)
  phyl <- read.tree(text = outList[[i]]$phyloOut)
  extantPhylo <- drop.fossil(phy = phyl)
  pvalsPhyloGeo[[i]] <- nullModel(speciesMatrix = s, localPlotSize = 100, phylogeny = extantPhylo, numberOfPlots = 100,repetitions = 1000)
}

names(pvalsPhyloGeo) <- names(outList)

# Sample Pool Picante
pvalsSamplePicante <- list()
for(i in 1:length(outList)){
  s <- matrix(outList[[i]]$specOut, nrow = 256, ncol = 256)
  comMat <- localPlots(size = 100, n = 100, matrix = s, community = T)$communityTable
  phyl <- read.tree(text = outList[[i]]$phyloOut)
  extantPhylo <- drop.fossil(phy = phyl)
  extantPhyloDis <- cophenetic(extantPhylo)
  pvalsSamplePicante[[i]] <- ses.mpd(samp = comMat, dis = extantPhyloDis, null.model = "sample.pool", abundance.weighted = TRUE, runs = 1000)$mpd.obs.p
}

names(pvalsSamplePicante) <- names(outList)

# Sample Pool PhylGeo
pvalsSampleGeo <- list()
for(i in 1:length(outList)){
  s <- matrix(outList[[i]]$specOut, nrow = 256, ncol = 256)
  phyl <- read.tree(text = outList[[i]]$phyloOut)
  extantPhylo <- drop.fossil(phy = phyl)
  pvalsSampleGeo[[i]] <- nullModelSample(speciesMatrix = s, localPlotSize = 100, phylogeny = extantPhylo, numberOfPlots = 100,repetitions = 1000)
}

names(pvalsSampleGeo) <- names(outList)
# 
# pvalsRecon <- list()
# for(i in 1:length(outList)){
#   s <- matrix(outList[[i]]$specOut, nrow = 256, ncol = 256)
#   t <- matrix(outList[[i]]$neutralOut, nrow = 256, ncol = 256)
#   extantPhylo <- phyloReconstruct(speciesMatrix = s, traitMatrix = t, "ward.D2")
#   pvalsRecon[[i]] <- nullModel(speciesMatrix = s, localPlotSize = 100, phylogeny = extantPhylo, numberOfPlots = 650,repetitions = 1000)
# }
# names(pvalsRecon) <- names(outList)
```

### Plots Recorded Phylogeny
![](nullModelCheck_files/figure-html/plotting_recorded_phylogeny_and_landscape-1.png) ![](nullModelCheck_files/figure-html/plotting_recorded_phylogeny_and_landscape-2.png) ![](nullModelCheck_files/figure-html/plotting_recorded_phylogeny_and_landscape-3.png) ![](nullModelCheck_files/figure-html/plotting_recorded_phylogeny_and_landscape-4.png) ![](nullModelCheck_files/figure-html/plotting_recorded_phylogeny_and_landscape-5.png) ![](nullModelCheck_files/figure-html/plotting_recorded_phylogeny_and_landscape-6.png) ![](nullModelCheck_files/figure-html/plotting_recorded_phylogeny_and_landscape-7.png) ![](nullModelCheck_files/figure-html/plotting_recorded_phylogeny_and_landscape-8.png) ![](nullModelCheck_files/figure-html/plotting_recorded_phylogeny_and_landscape-9.png) ![](nullModelCheck_files/figure-html/plotting_recorded_phylogeny_and_landscape-10.png) ![](nullModelCheck_files/figure-html/plotting_recorded_phylogeny_and_landscape-11.png) ![](nullModelCheck_files/figure-html/plotting_recorded_phylogeny_and_landscape-12.png) ![](nullModelCheck_files/figure-html/plotting_recorded_phylogeny_and_landscape-13.png) ![](nullModelCheck_files/figure-html/plotting_recorded_phylogeny_and_landscape-14.png) ![](nullModelCheck_files/figure-html/plotting_recorded_phylogeny_and_landscape-15.png) ![](nullModelCheck_files/figure-html/plotting_recorded_phylogeny_and_landscape-16.png) 

### Plots Reconstructed Phylogeny

```r
# par(oma = c(0, 0, 2, 0))
# for(i in 1:length(outList)){
#   s <- matrix(outList[[i]]$specOut, nrow = 256, ncol = 256)
#   t <- matrix(outList[[i]]$neutralOut, nrow = 256, ncol = 256)
#   extantPhylo <- phyloReconstruct(speciesMatrix = s, traitMatrix = t, "ward.D")
#   plotSpatialPhylo(landscape = s, phylogeny = extantPhylo, plot = "both")
#   mtext(names(outList)[i], outer = TRUE, cex = 1.5)
#   
# }
```

### P-Value Boxplots


### Spatial Patterns


### Phylogeny.Pool Picante

![](nullModelCheck_files/figure-html/pValue_matrix-1.png) 
### Phylogeny.Pool PhylGeo
![](nullModelCheck_files/figure-html/unnamed-chunk-1-1.png) 
### Sample.Pool Picante
![](nullModelCheck_files/figure-html/unnamed-chunk-2-1.png) 
### Sample.Pool Phylgeo
![](nullModelCheck_files/figure-html/unnamed-chunk-3-1.png) 

