# check if RTools really not there
Sys.getenv()['PATH']

# Add Rtools to path

Sys.setenv('PATH' = paste( "c:\\\\Rtools\\\\bin;c:\\\\Rtools\\\\gcc-4.6.3\\\\bin;",  Sys.getenv('PATH'), sep =""))
