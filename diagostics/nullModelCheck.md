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
pvals <- list()
for(i in 1:length(outList)){
  s <- matrix(outList[[i]]$specOut, nrow = 256, ncol = 256)
  phyl <- read.tree(text = outList[[i]]$phyloOut)
  extantPhylo <- drop.fossil(phy = phyl)
  pvals[[i]] <- nullModel(speciesMatrix = s, localPlotSize = 100, phylogeny = extantPhylo, numberOfPlots = 650,repetitions = 1000)
}

names(pvals) <- names(outList)

pvalsRecon <- list()
for(i in 1:length(outList)){
  s <- matrix(outList[[i]]$specOut, nrow = 256, ncol = 256)
  t <- matrix(outList[[i]]$neutralOut, nrow = 256, ncol = 256)
  extantPhylo <- phyloReconstruct(speciesMatrix = s, traitMatrix = t, "ward.D")
  pvalsRecon[[i]] <- nullModel(speciesMatrix = s, localPlotSize = 100, phylogeny = extantPhylo, numberOfPlots = 650,repetitions = 1000)
}
names(pvalsRecon) <- names(outList)
```

### Plots Recorded Phylogeny

```r
par(oma = c(0, 0, 2, 0))
for(i in 1:length(outList)){
  s <- matrix(outList[[i]]$specOut, nrow = 256, ncol = 256)
  phyl <- read.tree(text = outList[[i]]$phyloOut)
  extantPhylo <- drop.fossil(phy = phyl)
  plotSpatialPhylo(landscape = s, phylogeny = extantPhylo, plot = "both")
  mtext(names(outList)[i], outer = TRUE, cex = 1.5)
  
}
```

![](nullModelCheck_files/figure-html/unnamed-chunk-3-1.png) ![](nullModelCheck_files/figure-html/unnamed-chunk-3-2.png) ![](nullModelCheck_files/figure-html/unnamed-chunk-3-3.png) ![](nullModelCheck_files/figure-html/unnamed-chunk-3-4.png) ![](nullModelCheck_files/figure-html/unnamed-chunk-3-5.png) ![](nullModelCheck_files/figure-html/unnamed-chunk-3-6.png) ![](nullModelCheck_files/figure-html/unnamed-chunk-3-7.png) ![](nullModelCheck_files/figure-html/unnamed-chunk-3-8.png) ![](nullModelCheck_files/figure-html/unnamed-chunk-3-9.png) ![](nullModelCheck_files/figure-html/unnamed-chunk-3-10.png) ![](nullModelCheck_files/figure-html/unnamed-chunk-3-11.png) ![](nullModelCheck_files/figure-html/unnamed-chunk-3-12.png) ![](nullModelCheck_files/figure-html/unnamed-chunk-3-13.png) ![](nullModelCheck_files/figure-html/unnamed-chunk-3-14.png) ![](nullModelCheck_files/figure-html/unnamed-chunk-3-15.png) ![](nullModelCheck_files/figure-html/unnamed-chunk-3-16.png) 

### Plots Reconstructed Phylogeny

```r
par(oma = c(0, 0, 2, 0))
for(i in 1:length(outList)){
  s <- matrix(outList[[i]]$specOut, nrow = 256, ncol = 256)
  t <- matrix(outList[[i]]$neutralOut, nrow = 256, ncol = 256)
  extantPhylo <- phyloReconstruct(speciesMatrix = s, traitMatrix = t, "ward.D")
  plotSpatialPhylo(landscape = s, phylogeny = extantPhylo, plot = "both")
  mtext(names(outList)[i], outer = TRUE, cex = 1.5)
  
}
```

![](nullModelCheck_files/figure-html/unnamed-chunk-4-1.png) ![](nullModelCheck_files/figure-html/unnamed-chunk-4-2.png) ![](nullModelCheck_files/figure-html/unnamed-chunk-4-3.png) ![](nullModelCheck_files/figure-html/unnamed-chunk-4-4.png) ![](nullModelCheck_files/figure-html/unnamed-chunk-4-5.png) ![](nullModelCheck_files/figure-html/unnamed-chunk-4-6.png) ![](nullModelCheck_files/figure-html/unnamed-chunk-4-7.png) ![](nullModelCheck_files/figure-html/unnamed-chunk-4-8.png) ![](nullModelCheck_files/figure-html/unnamed-chunk-4-9.png) ![](nullModelCheck_files/figure-html/unnamed-chunk-4-10.png) ![](nullModelCheck_files/figure-html/unnamed-chunk-4-11.png) ![](nullModelCheck_files/figure-html/unnamed-chunk-4-12.png) ![](nullModelCheck_files/figure-html/unnamed-chunk-4-13.png) ![](nullModelCheck_files/figure-html/unnamed-chunk-4-14.png) ![](nullModelCheck_files/figure-html/unnamed-chunk-4-15.png) ![](nullModelCheck_files/figure-html/unnamed-chunk-4-16.png) 

### P-Value Boxplots

```r
par(oma = c(4.5, 0, 0, 0))
a <- boxplot(pvals, las=2, ylim=c(0,1.1), main="p-Values MPD, recorded")
text(x = seq(1,16,1), labels = round(sapply(pvals, mean),2), y = 1.1)
```

![](nullModelCheck_files/figure-html/unnamed-chunk-5-1.png) 

```r
a <- boxplot(pvalsRecon, las=2, ylim=c(0,1.1), main="p-Values MPD, reconstructed")
text(x = seq(1,16,1), labels = round(sapply(pvalsRecon, mean),2), y = 1.1)
```

![](nullModelCheck_files/figure-html/unnamed-chunk-5-2.png) 


