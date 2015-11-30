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
  
  
  simu1<-runSimulation(par1)
#   simu2<-runSimulation(par2)
#   simu3<-runSimulation(par3)
#   simu4<-runSimulation(par4)
#   simu5<-runSimulation(par5)
#   simu6<-runSimulation(par6)
  
  sacVal1[,i]<- sac(simu1,area=c(4,8,16,32,64,128,256,512,1024,2048), plot=FALSE)$sr.Mean
#   sacVal2[,i]<- sac(simu2,area=c(4,8,16,32,64,128,256,512,1024,2048), plot=FALSE)$sr.Mean
#   sacVal3[,i]<- sac(simu3,area=c(4,8,16,32,64,128,256,512,1024,2048), plot=FALSE)$sr.Mean
#   sacVal4[,i]<- sac(simu4,area=c(4,8,16,32,64,128,256,512,1024,2048), plot=FALSE)$sr.Mean
#   sacVal5[,i]<- sac(simu5,area=c(4,8,16,32,64,128,256,512,1024,2048), plot=FALSE)$sr.Mean
#   sacVal6[,i]<- sac(simu6,area=c(4,8,16,32,64,128,256,512,1024,2048), plot=FALSE)$sr.Mean
  }
  
  
  ## Also here check if relative paths can be used. If not, use data....
  save(sacVal1, file="Simu1.RData")
  save(sacVal2, file="Simu2.RData")
  save(sacVal3, file="Simu3.RData")
  save(sacVal4, file="Simu4.RData")
  save(sacVal5, file="Simu5.RData")
  save(sacVal6, file="Simu6.RData")
  
  
  ## Now the model is tested
  #Neutral Models
  par1<-createCompletePar(x=100,y=100, dispersal="global", runs=1000)
  par2<-createCompletePar(x=100,y=100, dispersal=1, runs=1000)
  
  #Other Model runs
  par3<-createCompletePar(x=100,y=100, dispersal="global", runs=1000,
                         density=2, environment=2)
  par4<-createCompletePar(x=100,y=100, dispersal=1, runs=1000,
                         density=2, environment=2) 
  
  par5<-createCompletePar(x=100,y=100, dispersal="global", runs=1000,
                         density=5, environment=5)
  par6<-createCompletePar(x=100,y=100, dispersal=1, runs=1000,
                         density=5, environment=5) 
  
  simu1<-runSimulation(par1)
  simu2<-runSimulation(par2)
  simu3<-runSimulation(par3)
  simu4<-runSimulation(par4)
  simu5<-runSimulation(par5)
  simu6<-runSimulation(par6)
  
  sac1<-sac(simu1,area=c(4,8,16,32,64,128,256,512,1024,2048), plot=FALSE)$sr.Mean
  sac2<-sac(simu2,area=c(4,8,16,32,64,128,256,512,1024,2048), plot=FALSE)$sr.Mean
  sac3<-sac(simu3,area=c(4,8,16,32,64,128,256,512,1024,2048), plot=FALSE)$sr.Mean
  sac4<-sac(simu4,area=c(4,8,16,32,64,128,256,512,1024,2048), plot=FALSE)$sr.Mean
  sac5<-sac(simu5,area=c(4,8,16,32,64,128,256,512,1024,2048), plot=FALSE)$sr.Mean
  sac6<-sac(simu6,area=c(4,8,16,32,64,128,256,512,1024,2048), plot=FALSE)$sr.Mean
  
  
  ##TODO Check realtive file paths. If not use data....
  load("Simu1.RData")
  load("Simu2.RData")
  load("Simu3.RData")
  load("Simu4.RData")
  load("Simu5.RData")
  load("Simu6.RData")
  
  for(i in 1:10){
    expect_true(sac1[i]<=quantile(sacVal1[i,],probs=0.975)& sac1[i]>=quantile(sacVal1[i,],probs=0.025))
    expect_true(sac2[i]<=quantile(sacVal2[i,],probs=0.975)& sac2[i]>=quantile(sacVal2[i,],probs=0.025))
    expect_true(sac3[i]<=quantile(sacVal3[i,],probs=0.975)& sac3[i]>=quantile(sacVal3[i,],probs=0.025))
    expect_true(sac4[i]<=quantile(sacVal4[i,],probs=0.975)& sac4[i]>=quantile(sacVal4[i,],probs=0.025))
    expect_true(sac5[i]<=quantile(sacVal5[i,],probs=0.975)& sac5[i]>=quantile(sacVal5[i,],probs=0.025))
    expect_true(sac6[i]<=quantile(sacVal6[i,],probs=0.975)& sac6[i]>=quantile(sacVal6[i,],probs=0.025))
  }
  
  
})