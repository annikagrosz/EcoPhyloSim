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

  expect_is(simuBatch, "PhylosimList")
  expect_is(simuBatch[[1]], "Phylosim")
  expect_is(simuBatch[[2]], "Phylosim")
  
})





test_that("Neutral Model is working as expected",{
 # testthat::skip("Not working")
  
  sacVal<-matrix(0,10,100)
  #racVal<-matrix(NA,20,100)
  richness<-numeric()

  for(i in 1:100){
  par<-createCompletePar(x=50,y=50,dispersal = "global",
                           density=0, environment=0, runs=5e+05, seed = i)
  simu<-runSimulation(par)
  sacVal[,i]<- sac(simu,area=c(4,8,16,32,64,128,256,512,1024,2048), plot=FALSE)$sr.Mean
 # racVal[,i]<-rac(simu, plot=FALSE)$Abundance
  richness[i]<-specRich(simu)
  }
  
  parRNeutral<-createCompletePar(x=50,y=50,dispersal = "global",
                                 density=0, environment=0, type="Rneutral",runs=1000)
  
 #sacR<-matrix(0,10,10)
 # richness<-numeric()
#  for(i in 1:10){
  simuRNeutral<-runSimulation(parRNeutral)
  sacR<-sac(simuRNeutral,area=c(4,8,16,32,64,128,256,512,1024,2048), plot=FALSE)$sr.Mean
 # racR<-rac(simuRNeutral, plot=FALSE)$Abundance
 richnessR<-specRich(simuRNeutral)
 # }
 expect_true(richnessR<=quantile(richness, probs=0.975) & richnessR>=quantile(richness,probs=0.025))
  for(i in 1:10){
    expect_true(sacR[i]<=quantile(sacVal[i,],probs=0.975)& sacR[i]>=quantile(sacVal[i,],probs=0.025))
   # expect_true(racR[i]<=quantile(racVal[i,],probs=0.975)& racR[i]>=quantile(racVal[i,],probs=0.025))
    
  }
  })

