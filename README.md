# PhyloSim
R package for simulation of biogeographical and phylogenetic data 

### Installation 


#### For users

You can install directly from gh, using the 'devtools' package:

```{r}
install.packages(c("devtools","Rcpp"))
library(devtools)
library(Rcpp)

install_github("biometry/phylosim", subdir = "PhyloSim", auth_token ="fb17360869e0bb3518677936611776687b48e17a", dependencies = TRUE,
build_vignettes = TRUE)
```

For the moment this uses a private key that grants access to the repo, don't forward or copy this code elsewhere!

#### For developers

Clone or fork the repo, and the follow the hints here http://biometry.github.io/APES/R/R70-PackageDevelopment.html if you don't know how to compile a package

