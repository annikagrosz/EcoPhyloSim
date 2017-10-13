#' @title Run PhyloSim Analysis
#' @description This function is used to perform many (>1000) PhyloSim simulations in parallel with only one function call. The user can specify parameters that are fixed across all simulations and parameters that are variable. The results are saved to a specified folder. 
#' @param variableParams A named list, where the names are parameter names and the values are priors (functions). A call to the function must return a single value. See details.
#' @param fixedParams A named list, where the names are parameter names and the values are parameter values for the PhyloSim model.
#' @param nRuns Integer, specifying the number of simulations to run
#' @param outDir String, specifying the directory to which the output will be written
#' @param summariesOnly Boolean, specifying whether only the summary statistics should be saved.
#' @param nCores Integer or "auto", number of parallel cores to use for simulations
#' @param chunkSize Integer, specifying the number of simulations per save file. Choosing a very large chunkSize (>30,000) might deplete the RAM, leading to bad performance.
#' @details variableParams must be list of functions. The idea behind this is to draw from a prior, so the functions should return a single value drawn from the corresponding parameter prior. However, the implementation allows the user to specify stateful function, instead of priors to perform analyses with variable Parameters that are not drawn from a prior, but follow a sequence e.g. run simulations with varying specRate.
#' @examples 
#' variableParams <- list(environment=function()return(runif(1)),
#'                        specRate=function()return(runif(1, 1, 10)))
#' fixedParams <- list(x=30, y=30,
#'                     runs=1000,
#'                     densityCut=1,
#'                     calculateSummaries=TRUE
#' 
#' runPhylosimAnalysis(variableParams,
#'                     fixedParams,
#'                     1000,
#'                     "outDirName")
#' 
#' @author Tankred Ott
#' @export
runPhylosimAnalysis <- function(variableParams, fixedParams, nRuns, outDir, summariesOnly=FALSE, nCores="auto", chunkSize="auto") {
  # Get available parameters
  arguments <- formals(PhyloSim::createCompletePar)
  
  # Validate input
  validateParams(names(variableParams), names(arguments))
  validateParams(names(fixedParams), names(arguments))
  validatePriors(variableParams)
  cat("ATTENTION: The values of the fixed parameters and the return values of the priors are not validated automatically. Please make sure to read the help of PhyloSim::createCompletePar to choose valid parameters.\n")
  
  # Find parameters that stay at default values
  defaultParams <- getDefaultParams(arguments, c(fixedParams, variableParams))
  
  # if output directory doesn't exist create it
  if(!dir.exists(outDir)){
    dir.create(outDir)
  }
  
  # write an first overview over the parameters to file
  writeParamOverview(defaultParams, fixedParams, variableParams, joinPath(c(outDir, "paramOverview.txt")))
  # # read it later in like this:
  # inp <- readLines("argsOut.txt")
  # strsplit(inp, split = " ")
  
  # get the number of parallel cores
  nCores <- getValidatedNCores(nCores)
  
  # determine chunk size
  chunkSize <- getValidatedChunkSize(chunkSize, nRuns)
  
  # Create a list of parameter that are fixed over all simulations
  fixedParamsList <- c(defaultParams, fixedParams)
  
  # Runs the simulation in chunks and saves the results to outDir
  runSimulationChunkwise(fixedParamsList, variableParams, nRuns, chunkSize, outDir, summariesOnly, nCores)  
}

#' @author Tankred Ott
# Checks if the input parameters are valid parameters for a function
# with the parameters arguments
validateParams <- function(input, arguments) {
  isE <- is.element(input, arguments)
  if (sum(isE) != length(input)) {
    errInputs <- input[!isE]
    multiple <- length(errInputs) > 1
    errMsg <- paste(c(paste(errInputs, collapse = ", ")),
                    if (multiple) "are" else "is","not", if (!multiple) "a",
                    "valid", if (multiple) "parameters" else "parameter")
    stop(errMsg)
  }
}

#' @author Tankred Ott
# Check if priors are behaving as expected
validatePriors <- function(variableParams) {
  for (i in 1:length(variableParams)) {
    if(!is.function(variableParams[[i]])) stop(paste(c("The prior for parameter",
                                                       names(variableParams)[i],
                                                       "must be a function"),
                                                     collapse = " "))
    # Call to prior should perform a single draw/return a single number
    # This is a little bite ineffecient, as we are only drawing 1 value at a time.
    # However, this should make no difference as the bottleneck is the simulation,
    # and the code becomes more readable and extensible this way
    x <- variableParams[[i]]()
    if(!(is.vector(x) && length(x) == 1)) stop(paste(c("Expected prior",
                                                       names(variableParams)[i], 
                                                       "to return a single value"),
                                                     collapse = " "))
  }
}

#' @author Tankred Ott
# expects a lists of parameter names and the corresponding values
# return a list of parameters and corresponding values that stay at the default value
getDefaultParams <- function (allParams, nonDefaultParams) {
  replacedIndices <- is.element(names(allParams), names(nonDefaultParams))
  return(allParams[!replacedIndices])
}

#' @author Tankred Ott
# Writes the parameters and corresponding values (for fixed and default, but not
# for variable as those are priors) to a file
writeParamOverview <- function(defaultParams, fixedParams, variableParams, f="param_overview.txt") {
  argsOut <- list(defaultParams=names(defaultParams),
                  defaultArgsParams=unlist(defaultParams, use.names = FALSE),
                  fixedParams=names(fixedParams),
                  fixedParamsVals=unlist(fixedParams, use.names = FALSE),
                  variableParams=names(variableParams))
  
  for(i in 1:length(argsOut)) {
    append <- if (i == 1) FALSE else TRUE
    out <- paste(c(names(argsOut)[i], argsOut[[i]]))
    write(file = f, x = out, ncolumns = length(out), append = append)
  }  
}

#' @author Tankred Ott
# checks the chunkSize parameter corresponding to nRuns
# Implements the automatic chunkSize adaption when chunkSize="auto"
getValidatedChunkSize<- function (chunkSize, nRuns, maxChunkSize = 10000) {
  if (is.character(chunkSize)) {
    if (chunkSize == "auto") {
      if (nRuns <= 100) chunkSize <- nRuns
      else if (nRuns <= 10000) chunkSize <- 100
      else chunkSize <- min(floor(nRuns / 100), maxChunkSize)
      cat(paste(c("chunkSize automatically set to", chunkSize, "\n"), collapse = " "))
    } else {
      stop(paste(c(chunkSize, "is not a valid input for chunkSize."), collapse = " "))
    }
  } else if (chunkSize > maxChunkSize) {
    warning(paste(c("chunkSize is greater than",
                    maxChunkSize,
                    ". There might not be enough RAM available."),
                  collapse = " "))
  }
  return(chunkSize)
}

#' @author Tankred Ott
# joins directory paths (windows and Linux)
joinPath <- function(x, sysName = "auto") {
  if (sysName == "auto") sysName <- Sys.info()[["sysname"]]
  if(sysName == "Windows") return(paste(x, collapse = "\\"))
  else if(sysName == "Linux") return(paste(x, collapse = "/"))
  else stop(paste(c("System", sysName, "is not supported."), collapse = " "))
}

# returns the number of parallel cores
getValidatedNCores <- function (nCores) {
  nMaxCores <- parallel::detectCores()
  if (is.character(nCores)) {
    if (nCores == "auto") {
      if (is.na(nMaxCores)) stop("Could not detect the number of available cores. Please enter the number of parallel cores to use manually.")
      else {
        nCores <- nMaxCores - 1
        cat(paste(c("nCores set to", nCores, "\n"), collapse = " "))
      }
    }
    # nCores is not a string
  } else {
    if (!is.na(nMaxCores)) {
      if (nCores > nMaxCores) stop("nCores may not be larger than the number of cores available")
      else if (nCores == nMaxCores) warning("All available cores are used for simulation. This will affect the performance of your computer.")
    }
  }
  return(nCores)
}

#' @author Tankred Ott
# This function devides the runs in chunks of chunkSize, generates a phylosim parameter object for each run by
# combining the fixed parameters and draws from the priors of the variable parameter, runs the phylosim model in
# parallel mode and saves the results and the parameters in outDir
runSimulationChunkwise <- function (fixedParamsList, variableParams, nRuns, chunkSize, outDir, summariesOnly, nCores) {
  nChunks <- ceiling(nRuns / chunkSize)
  remainder <- nRuns %% chunkSize
  for (i in 1:nChunks) {
    cat(paste(c("Running chunk",i ,"of", nChunks, "\n"), collapse = " "))
    t0 <- proc.time()
    
    n <- chunkSize # number of simulations per chunk
    # if nRuns not divisible by chunkSize, we need to add an additional
    # smaller chunk at the end
    if (i == nChunks && (remainder != 0)) {
      n <- remainder
    }
    
    # create phyloSim parameter object
    params <- rep(list(NA), n)
    for (j in 1:n) {
      # create list of variable parameters drawn from prior
      varParamsList <- lapply(variableParams, FUN = do.call, list())
      params[[j]] <- do.call(PhyloSim::createCompletePar, args = c(fixedParamsList, varParamsList))
    }
    
    if (summariesOnly && (params[[i]]$calculateSummaries == FALSE)) stop("summariesOnly is set to TRUE, but calculateSummaries set to FALSE")
    
    # generate "unique" suffix for data files
    from <- as.integer((i-1) * chunkSize + 1)
    to <- as.integer(from + n - 1)
    suffix <- paste(c("_", from,"-", to), collapse = "")
    paramFileName <- paste(c("param", suffix), collapse = "")
    outFileName <- paste(c("out", suffix), collapse = "")
    
    save(params, file = joinPath(c(outDir, paramFileName)))
    
    # run simulations in parallel
    out <- runSimulationBatch(params, nCores)
    
    save(out, file = joinPath(c(outDir, outFileName)))
    cat(paste(c("Finished chunk", i ,"of", paste0(nChunks, "."), "time elapsed:", proc.time()[3]-t0[3], "\n"), collapse = " "))
  }
  cat(paste(c("Output and parameters are saved to directory", c("'", outDir, "'")), collapse = " "))
}

#' @title Cunkwise summary statistics calculation
#' @description Calculates summary statistics for all "out_" files created with runPhylosimAnalysis and saves the results to "summary_" files in the same folder.
#' @param dir Directory from which the "out_" files should be read and to which the "summary_" files will be saved.
#' @param nCores "auto" or number of parallel cores.
#' @author Tankred Ott
#' @export
chunkwiseCalculateSummaries <- function (dir=".", nCores="auto") {
  simFiles <- getFileNames(path=dir)
  if (length(simFiles) == 0) stop(paste("No PhyloSim output files found in", if (dir == ".") getwd() else dir))
  
  # this is inefficient for large files, as the whole file is loaded into memory just to get the chunkSize
  # better might be to run the first summary calculation without parallelization and start the parallel runs with the 
  # second file
  chunkSize <- length(loadLocal(joinPath(c(dir, simFiles[1]))))
  
  nCores <- getValidatedNCores(nCores)
  cl <- parallel::makeCluster(nCores)
  doParallel::registerDoParallel(cl)
  
  library(foreach)
  foreach(i=1:length(simFiles), .packages = c("PhyloSim")) %dopar% {
    loadLocal <- function(path) return(local(get(load(path))))
    joinPath <- function(x, sysName = "auto") {
      if (sysName == "auto") sysName <- Sys.info()[["sysname"]]
      if(sysName == "Windows") return(paste(x, collapse = "\\"))
      else if(sysName == "Linux") return(paste(x, collapse = "/"))
      else stop(paste(c("System", sysName, "is not supported."), collapse = " "))
    }
    
    # Load simulation chunk
    simChunk <- loadLocal(joinPath(c(dir, simFiles[i])))
    
    # Calculate summaries
    summaryChunk <- lapply(simChunk, PhyloSim::calculateSummaryStatistics, strict=TRUE)
    
    # Determine file name
    n <- length(simChunk)
    from <- as.integer((i-1) * chunkSize + 1)
    to <- as.integer(from + n - 1)
    suffix <- paste(c("_", from,"-", to), collapse = "")
    summaryFileName <-  paste(c("summary", suffix), collapse = "")
    # save file
    save(summaryChunk, file = joinPath(c(dir, summaryFileName)))
  }
  parallel::stopCluster(cl)
}

#' @title Clean up parameter and summary chunks
#' @description Reads in summary files created with chunkwiseCalculateSummaries, and parameter files created with runPhylosimAnalysis and removes crashed simulations (those where summaries == NA). The resulting cleaned-up chunks are saved to the subfolder "cleaned_up".
#' @author Tankred Ott
#' @param dir Directory to which the results of chunkwiseCalculateSumaries and runPhylosimAnalysis were saved.
#' @export
cleanUpChunks <- function (dir=".") {
  
}


#' @author Tankred Ott
loadLocal <- function(path) return(local(get(load(path))))

#' @author Tankred Ott
getFileNames <- function(path=".", pattern="^out_[0-9]+-[0-9]+$") {
  files <- list.files(path = path, pattern = pattern)
  files <- gtools::mixedsort(files)
  return(files)
}

