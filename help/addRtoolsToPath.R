# check if RTools really not there
Sys.getenv()['PATH']

# Add Rtools to path

Sys.setenv('PATH' = paste( "c:\\\\Rtools\\\\bin;c:\\\\Rtools\\\\gcc-4.6.3\\\\bin;",  Sys.getenv('PATH'), sep =""))


#simulation----------------


par <- createCompletePar(x = 50, y = 50, dispersal = 1, runs = c(500,1000), density = 1, environment = 0.5, specRate = 1)

system.time(simu <- runSimulation(par))