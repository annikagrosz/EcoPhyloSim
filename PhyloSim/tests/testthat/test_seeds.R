context("seeds")

testthat::test_that("simulations with the same seed return same summary statistics", {
  seeds <- ceiling(runif(10, -10000, 10000))
  for (seed in seeds) {
    set.seed(seed)
    simu1 <- runSimulation(createCompletePar(specRate = 2, calculateSummaries = T))
    s1 <- simu1$Output[[1]]$summaries
    set.seed(seed)
    simu2 <- runSimulation(createCompletePar(specRate = 2, calculateSummaries = T))
    s2 <- simu1$Output[[1]]$summaries
    for (i in 1:length(s1)) {
      testthat::expect_equal(s1[[i]] == s2[[i]], TRUE)
    }
  }
})
