context("run Simulation")
testthat("Model outputs are  as expected",{
  # Here are the model runs that created the .RData files
  ## TODO When done comment this part of the code!
  sacVal1<-sacVal2<-sacVal3<-sacVal4<-sacVal5<-sacVal6<-matrix(0,10,100)
  
  for(i in 1:100){
    #Neutral Models
  par1<-createCompletePar(x=100,y=100, dispersal="global", runs=1000, seed=i)
  par2<-createCompletePar(x=100,y=100, dispersal=1, runs=1000, seed=i)
  
    #Other Model runs
  par3<-createCompletePar(x=100,y=100, dispersal="global", runs=1000,
                         density=0.5, environment=0.5, seed=i)
  par4<-createCompletePar(x=100,y=100, dispersal=1, runs=1000,
                         density=0.5, environment=0.5, seed=i) 
  
  par5<-createCompletePar(x=100,y=100, dispersal="global", runs=1000,
                         density=1, environment=1, seed=i)
  par6<-createCompletePar(x=100,y=100, dispersal=1, runs=1000,
                         density=1, environment=1, seed=i) 
  
  parList<-list(par1,par2,par3,par4,par5,par6)
  
  simu<-runSimulationBatch(parList, parallel = 15)
  
  sacVal1[,i]<- sac(simu[[1]],area=c(4,8,16,32,64,128,256,512,1024,2048), plot=FALSE)$sr.Mean
  sacVal2[,i]<- sac(simu[[2]],area=c(4,8,16,32,64,128,256,512,1024,2048), plot=FALSE)$sr.Mean
  sacVal3[,i]<- sac(simu[[3]],area=c(4,8,16,32,64,128,256,512,1024,2048), plot=FALSE)$sr.Mean
  sacVal4[,i]<- sac(simu[[4]],area=c(4,8,16,32,64,128,256,512,1024,2048), plot=FALSE)$sr.Mean
  sacVal5[,i]<- sac(simu[[5]],area=c(4,8,16,32,64,128,256,512,1024,2048), plot=FALSE)$sr.Mean
  sacVal6[,i]<- sac(simu[[6]],area=c(4,8,16,32,64,128,256,512,1024,2048), plot=FALSE)$sr.Mean
  }
  
  
  ## Save the data as a list in inst/extdata
  simulations<-list(sacVal1,sacVal2,sacVal3,sacVal4,sacVal5,sacVal6)
  save(simulations, file="TestData.RData")

  ## Now the model is tested
  
  #Neutral Models
  par1<-createCompletePar(x=100,y=100, dispersal="global", runs=1000)
  par2<-createCompletePar(x=100,y=100, dispersal=1, runs=1000)
  
  #Other Model runs
  par3<-createCompletePar(x=100,y=100, dispersal="global", runs=1000,
                          density=0.5, environment=0.5)
  par4<-createCompletePar(x=100,y=100, dispersal=1, runs=1000,
                          density=0.5, environment=0.5) 
  
  par5<-createCompletePar(x=100,y=100, dispersal="global", runs=1000,
                          density=1, environment=1)
  par6<-createCompletePar(x=100,y=100, dispersal=1, runs=1000,
                          density=1, environment=1) 
  
  
  
  parList<-list(par1,par2,par3,par4,par5,par6)
  
  simu<-runSimulationBatch(parList, parallel = 15)
  
  sac1<-sac(simu[[1]],area=c(4,8,16,32,64,128,256,512,1024,2048), plot=FALSE)$sr.Mean
  sac2<-sac(simu[[2]],area=c(4,8,16,32,64,128,256,512,1024,2048), plot=FALSE)$sr.Mean
  sac3<-sac(simu[[3]],area=c(4,8,16,32,64,128,256,512,1024,2048), plot=FALSE)$sr.Mean
  sac4<-sac(simu[[4]],area=c(4,8,16,32,64,128,256,512,1024,2048), plot=FALSE)$sr.Mean
  sac5<-sac(simu[[5]],area=c(4,8,16,32,64,128,256,512,1024,2048), plot=FALSE)$sr.Mean
  sac6<-sac(simu[[6]],area=c(4,8,16,32,64,128,256,512,1024,2048), plot=FALSE)$sr.Mean
  
  
  ##Get the data
  load(load(system.file("extdata", "TestData.RData", package="PhyloSim")))
  sacVal1<-simulations[[1]]
  sacVal2<-simulations[[2]]
  sacVal3<-simulations[[3]]
  sacVal4<-simulations[[4]]
  sacVal5<-simulations[[5]]
  sacVal6<-simulations[[6]]
  
  for(i in 1:10){
    expect_true(sac1[i]<=quantile(sacVal1[i,],probs=0.975)& sac1[i]>=quantile(sacVal1[i,],probs=0.025))
    expect_true(sac2[i]<=quantile(sacVal2[i,],probs=0.975)& sac2[i]>=quantile(sacVal2[i,],probs=0.025))
    expect_true(sac3[i]<=quantile(sacVal3[i,],probs=0.975)& sac3[i]>=quantile(sacVal3[i,],probs=0.025))
    expect_true(sac4[i]<=quantile(sacVal4[i,],probs=0.975)& sac4[i]>=quantile(sacVal4[i,],probs=0.025))
    expect_true(sac5[i]<=quantile(sacVal5[i,],probs=0.975)& sac5[i]>=quantile(sacVal5[i,],probs=0.025))
    expect_true(sac6[i]<=quantile(sacVal6[i,],probs=0.975)& sac6[i]>=quantile(sacVal6[i,],probs=0.025))
  }
  
  
})