par <- createCompletePar(runs = seq(1000, 1020, by=1), dispersal = 2)
sim <- runSimulation(par)

plotAllSpecMats <- function (sim) {
  op <- par(mfrow=c(2,2))
  for (i in 1:length(sim$Output)) {
    image(sim$Output[[i]]$specMat, main=i)
  }
  par(op)
}

plotAllSpecMats(sim)
