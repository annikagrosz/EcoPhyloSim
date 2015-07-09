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
  extantPhylo <- phyloReconstruct(speciesMatrix = s, traitMatrix = t, "ward.D2")
  pvalsRecon[[i]] <- nullModel(speciesMatrix = s, localPlotSize = 100, phylogeny = extantPhylo, numberOfPlots = 650,repetitions = 1000)
}
names(pvalsRecon) <- names(outList)
```

### Plots Recorded Phylogeny
![](nullModelCheck_files/figure-html/plotting_recorded_phylogeny_and_landscape-1.png) ![](nullModelCheck_files/figure-html/plotting_recorded_phylogeny_and_landscape-2.png) ![](nullModelCheck_files/figure-html/plotting_recorded_phylogeny_and_landscape-3.png) ![](nullModelCheck_files/figure-html/plotting_recorded_phylogeny_and_landscape-4.png) ![](nullModelCheck_files/figure-html/plotting_recorded_phylogeny_and_landscape-5.png) ![](nullModelCheck_files/figure-html/plotting_recorded_phylogeny_and_landscape-6.png) ![](nullModelCheck_files/figure-html/plotting_recorded_phylogeny_and_landscape-7.png) ![](nullModelCheck_files/figure-html/plotting_recorded_phylogeny_and_landscape-8.png) ![](nullModelCheck_files/figure-html/plotting_recorded_phylogeny_and_landscape-9.png) ![](nullModelCheck_files/figure-html/plotting_recorded_phylogeny_and_landscape-10.png) ![](nullModelCheck_files/figure-html/plotting_recorded_phylogeny_and_landscape-11.png) ![](nullModelCheck_files/figure-html/plotting_recorded_phylogeny_and_landscape-12.png) ![](nullModelCheck_files/figure-html/plotting_recorded_phylogeny_and_landscape-13.png) ![](nullModelCheck_files/figure-html/plotting_recorded_phylogeny_and_landscape-14.png) ![](nullModelCheck_files/figure-html/plotting_recorded_phylogeny_and_landscape-15.png) ![](nullModelCheck_files/figure-html/plotting_recorded_phylogeny_and_landscape-16.png) 

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

![](nullModelCheck_files/figure-html/plotting_reconstructed_phylogeny_and_landscape-1.png) ![](nullModelCheck_files/figure-html/plotting_reconstructed_phylogeny_and_landscape-2.png) ![](nullModelCheck_files/figure-html/plotting_reconstructed_phylogeny_and_landscape-3.png) ![](nullModelCheck_files/figure-html/plotting_reconstructed_phylogeny_and_landscape-4.png) ![](nullModelCheck_files/figure-html/plotting_reconstructed_phylogeny_and_landscape-5.png) ![](nullModelCheck_files/figure-html/plotting_reconstructed_phylogeny_and_landscape-6.png) ![](nullModelCheck_files/figure-html/plotting_reconstructed_phylogeny_and_landscape-7.png) ![](nullModelCheck_files/figure-html/plotting_reconstructed_phylogeny_and_landscape-8.png) ![](nullModelCheck_files/figure-html/plotting_reconstructed_phylogeny_and_landscape-9.png) ![](nullModelCheck_files/figure-html/plotting_reconstructed_phylogeny_and_landscape-10.png) ![](nullModelCheck_files/figure-html/plotting_reconstructed_phylogeny_and_landscape-11.png) ![](nullModelCheck_files/figure-html/plotting_reconstructed_phylogeny_and_landscape-12.png) ![](nullModelCheck_files/figure-html/plotting_reconstructed_phylogeny_and_landscape-13.png) ![](nullModelCheck_files/figure-html/plotting_reconstructed_phylogeny_and_landscape-14.png) ![](nullModelCheck_files/figure-html/plotting_reconstructed_phylogeny_and_landscape-15.png) ![](nullModelCheck_files/figure-html/plotting_reconstructed_phylogeny_and_landscape-16.png) 

### P-Value Boxplots
![](nullModelCheck_files/figure-html/plotting_pValues_in_boxplots-1.png) ![](nullModelCheck_files/figure-html/plotting_pValues_in_boxplots-2.png) 

### Spatial Patterns
![](nullModelCheck_files/figure-html/plotting_spatial_patterns-1.png) ![](nullModelCheck_files/figure-html/plotting_spatial_patterns-2.png) ![](nullModelCheck_files/figure-html/plotting_spatial_patterns-3.png) 

