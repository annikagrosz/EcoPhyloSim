# phylosim
R package for simulation of biogeographical and phylogenetic data 

Installation 

1. Either locally (if you want to make changes to the package, this is what you want to do), by cloning or forking the repo, and the following hints here http://biometry.github.io/APES/R/R70-PackageDevelopment.html
2. Directly from gh, using the 'devtools' package:

```{r}
install.packages("devtools")
library(devtools)

install_github("biometry/phylosim", subdir = "PhylGeo", auth_token ="fb17360869e0bb3518677936611776687b48e17a")
```

For the moment this uses a private key that grants access to the repo, don't forward or copy this code elsewhere!
