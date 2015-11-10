context("run Simulation")

test_that("Simulation is running correctly",{
  par<-createCompletePar(x=50, y=50, dispersal="global",density=1)
  parRneutral<-createCompletePar(x=50, y=50, dispersal="global",density=1, type="Rneutral")
  simu<-runSimulation(par)
  simuRneutral<-runSimulation(parRneutral)
  
  expect_output(str(par), "List of 13")
  expect_is(simu, "Phylosim")
  expect_is(simuRneutral, "Phylosim")
  
  })

test_that("SimulationBatch is running correctly",{
  par1<-createCompletePar(x=50, y=50, dispersal="global",density=1)
  par2<-createCompletePar(x=50, y=50, dispersal="global",density=2)
  parlist<-list(par1,par2)
  simuBatch<-runSimulationBatch(parlist)

  expect_is(simuBatch, "PhylosimBatch")
  expect_is(simuBatch[[1]], "Phylosim")
  expect_is(simuBatch[[2]], "Phylosim")
  
})