context("summaryStatistics")

test_that("calculateSummaryStatistics returns for random simulations", {
  for(i in 1:100) {
    summStat <- calculateSummaryStatistics(runSimulation(createCompletePar(specRate = runif(1,1,5),
                                                         density = runif(1,0,5))))
    expect_output(str(summStat), "List of 6")
  }
})
