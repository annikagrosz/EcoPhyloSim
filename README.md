# PhyloSim
R package for simulation of biogeographical and phylogenetic data 

### Installation 


#### For users

You can install directly from gh, using the 'devtools' package:

```{r}
# install.packages(c("devtools","Rcpp")) # I case you don't have them installed
library(devtools)
library(Rcpp)

install_url("https://dl.dropboxusercontent.com/s/zkdof0b5b523qxt/PhyloSim_0.3.tar.gz")

?PhyloSim
browseVignettes("PhyloSim")
```

#### For developers

Clone or fork the repo, and the follow the hints here http://biometry.github.io/APES/R/R70-PackageDevelopment.html if you don't know how to compile a package

See also https://github.com/biometry/phylosim/wiki


