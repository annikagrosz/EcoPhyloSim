# check if RTools really not there
Sys.getenv()['PATH']
Sys.setenv('PATH' = paste( "c:\\\\Rtools\\\\bin;c:\\\\Rtools\\\\gcc-4.6.3\\\\bin;",  Sys.getenv('PATH'), sep =""))


# Path for pdflatex
Sys.getenv()['PATH']
Sys.setenv(PATH = paste(Sys.getenv("PATH"), "C:\\Users\\de.student\\Desktop\\NEW\\miktex\\bin\\" , sep=.Platform$path.sep))

Sys.setenv(PATH = paste(Sys.getenv("PATH"), "D:\\Uni\\Hiwi\\TEX\\miktex\\bin\\" , sep=.Platform$path.sep))
#simulation----------------

#C:\Users\de.student\Desktop\NEW\miktex\bin

par <- createCompletePar(x = 50, y = 50, dispersal = 1, runs = c(500,1000), density = 1, environment = 0.5, specRate = 1)

system.time(simu <- runSimulation(par))

simu <- runSimulationBatch(par)

