#' Bootstrap the baseline model
#' 
#' Run a subset of the baseline model a number of times after resampling e.g. Hauls in each Stratum, EDUSs in each Stratum.
#' 
#' @param LengthDistributionData The \code{\link[RstoxBase]{LengthDistributionData}} to resample Hauls in.
#' 
#' @export
#' 
Bootstrap <- function(
    projectPath, 
    # Table with ProcessName, ResamplingFunction, ResampleWithin, Seed:
    BootstrapMethodTable = data.table::data.table(), 
    NumberOfBootstraps = 1L, 
    NumberOfCores = integer()
) {
    
    # Identify the first process set for resampling by the BootstrapMethodTable:
    processIndexTable <- readProcessIndexTable(projectPath, modelName = "baseline")
    startProcess <- min(match(BootstrapMethodTable$ProcessName, processIndexTable$processName))
    lastProcessID <- utils::tail(processIndexTable$processID, 1)
    
    # Get the functions to use for resampling:
    BootstrapMethodList <- split(BootstrapMethodTable, by = "ProcessName")
    # Remove the ProcessName from the lists: 
    BootstrapMethodList <- lapply(BootstrapMethodList, function(x) x[, ProcessName := NULL])
    # Get the replaceData list, which is one element per bootstrap, where each element is a list named by the procecsses to resample the output from:
    replaceData <- lapply(BootstrapMethodList, getReplaceData, size = NumberOfBootstraps)
    
    # Define a function to run processes and save the output of the last process to the output folder:
    runOneBootstrapSaveLastOutput <- function(ind, replaceData, projectPath, startProcess, lastProcessID, NumberOfBootstraps) {
        # Get the resample function for this bootstrap run:
        thisReplaceData <- lapply(replaceData, "[[", ind)
        # Re-run the baseline:
        runProcesses(projectPath, modelName = "baseline", startProcess = startProcess, endProcess = Inf, save = FALSE, fileOutput = FALSE, setUseProcessDataToTRUE = FALSE, replaceData = thisReplaceData, msg = FALSE)
        # Get the last process output:
        lastProcessOutput <- getProcessOutput(projectPath, modelName = "baseline", processID = lastProcessID, drop.datatype = FALSE)
        print(lastProcessOutput)
        # Save the last process output:
        ###writeProcessOutputMemoryFiles(
        ###    processOutput = lastProcessOutput, 
        ###    projectPath = projectPath, 
        ###    modelName = "analysis", 
        ###    processID = lastProcessID, 
        ###    type = "output", 
        ###    subfolder = getBootstrapRunName(ind, NumberOfBootstraps, prefix = "Run")
        ###)
        return(lastProcessOutput)
    }
    
    # Run the subset of the baseline model:
    bootstrapIndex <- seq_len(NumberOfBootstraps)
    BootstrapData <- RstoxData::runOnCores(
        bootstrapIndex, 
        FUN = runOneBootstrapSaveLastOutput, 
        NumberOfCores = NumberOfCores, 
        replaceData = replaceData, 
        projectPath = projectPath, 
        startProcess = startProcess, 
        lastProcessID = lastProcessID, 
        NumberOfBootstraps = NumberOfBootstraps
    )
    
    if(is.list(BootstrapData[[1]]) && !data.table::is.data.table(BootstrapData[[1]])){
        tableNames <- names(BootstrapData[[1]])
        BootstrapData <- lapply(tableNames, rbindlistByName, x = BootstrapData)
        names(BootstrapData) <- tableNames
    }
    else {
        BootstrapData <- structure(list(data.table::rbindlist(BootstrapData)), names = names(BootstrapData[[1]]))
    }
    
    return(BootstrapData)
}

rbindlistByName <- function(name, x) {
    data.table::rbindlist(lapply(x, "[[", name))
}

readBootstrap <- function(projectPath, processName) {
    # Bootstrap lives in the model "analysis" only:
    modelName = "analysis"
    # Get the folder holding the output form the bootstrap:
    processID <- getProcessIDFromProcessName(projectPath = projectPath, modelName = modelName, processName = processName)
    folderPath <- getProcessOutputFolder(projectPath = projectPath, modelName = modelName, processID = processID, type = "output")
    
    # Return NULL if the folder does not exist:
    if(!file.exists(folderPath)) {
        warning("The process ", folderPath, " does not have an output folder. Has the proccess been run?")
        return(NULL)
    }
    
    # Get a nested list of all output files from the bootstrap:
    bootstrapOutputFiles <- getFilesRecursiveWithOrder(folderPath)
    
    # Read all files:
    processOutput <- rapply(
        bootstrapOutputFiles, 
        readProcessOutputFile, 
        how = "replace"
    )
    
}


# Function to get the name of the bootsrap run, which will be used as the subfolder name hoding the output from the bootstrap:
getBootstrapRunName <- function(ind, NumberOfBootstraps, prefix = "Run") {
    paste0(
        prefix, 
        formatC(ind, width = NumberOfBootstraps, format = "d", flag = "0")
    )
}



BootstrapMethodTable <- data.table::data.table(
    ProcessName = c(
        "LengthDistribution", 
        "NASC"
    ), 
    ResampleFunction = c(
        "ResampleHauls", 
        "ResampleEDSUs"
    ), 
    ResampleBy = c(
        "Stratum", 
        "Stratum"
    ), 
    Seed = c(
        1, 
        2
    )
)



getReplaceData <- function(x, size) {
    # Get seeds:
    seeds <- getSeedVector(x$Seed, size)
    x_expanded <- data.table::data.table(
        x[, !"Seed"], 
        Seed = seeds
    )
    
    # Split rows into a list:
    x_split <- split(x_expanded, by = "Seed")
    
    x_split <- lapply(x_split, function(x) list(x$ResampleFunction, as.list(x[, !c("ResampleFunction")])))
    
    return(x_split)
}






#' Resamples Hauls.
#' 
#' This function resamples Hauls with replacement by altering the LengthDistributionWeight.
#' 
#' @param LengthDistributionData The \code{\link[RstoxBase]{LengthDistributionData}} to resample Hauls in.
#' 
#' @export
#' 
ResampleHauls <- function(LengthDistributionData, Seed, ResampleBy = "Stratum") {
    
    # Get the unique strata:
    uniqueStrata <- unique(LengthDistributionData$Stratum)
    
    # Build a table of Stratum and Seed and merge with the LengthDistributionData:
    SeedTable <- data.table::data.table(
        Stratum = uniqueStrata, 
        Seed = getSeedVector(Seed, size = length(uniqueStrata))
    )
    LengthDistributionData <- merge(LengthDistributionData, SeedTable, by = ResampleBy)
    
    # Resample the Hauls:
    LengthDistributionData[, LengthDistributionWeight := resampleOne(.SD, seed = Seed[1], var = "Haul"), by = ResampleBy]
    return(LengthDistributionData)
}

#' Resamples EDSUs
#' 
#' This function resamples Hauls with replacement by altering the LengthDistributionWeight.
#' 
#' @param NASCData The \code{\link[RstoxBase]{NASCData}} to resample EDSUs in.
#' 
#' @export
#' 
ResampleEDSUs <- function(NASCData, Seed, ResampleBy = "Stratum") {
    
    # Get the unique strata:
    uniqueStrata <- unique(NASCData$Stratum)
    
    # Build a table of Stratum and Seed and merge with the NASCData:
    SeedTable <- data.table::data.table(
        Stratum = uniqueStrata, 
        Seed = getSeedVector(Seed, size = length(uniqueStrata))
    )
    NASCData <- merge(NASCData, SeedTable, by = ResampleBy)
    
    # Resample the Hauls:
    NASCData[, NASCData := resampleOne(.SD, seed = Seed[1], var = "EDSU"), by = ResampleBy]
    return(NASCData)
}






# Function to resample Hauls of one subset of the data:
resampleOne <- function(subData, seed, var = c("Haul", "EDSU")) {
    
    # Get the vavriable to resample:
    var <- match.arg(var)
    
    #count <- data.table::data.table(
    #    unique(subData[[var]])
    #)
    #setnames(count, var)
    
    # Get unique values:
    resampled <- sampleSorted(unique(subData[[var]]), seed = seed, replace = TRUE)
    # Tabulate the sampled Hauls:
    resampleTable <- data.table::as.data.table(table(resampled))
    setnames(resampleTable, c("resampled","N"), c(var, "resampledCountWithUniqueName"))

    # Merge the resampled counts into the data:
    count <- merge(subData, resampleTable, by = var, all = TRUE)
    
    # Insert the new count into WeightedCount (with NAs replaced by 0):
    count <- subData[, ifelse(
        is.na(count$resampledCountWithUniqueName), 
        0, 
        count$resampledCountWithUniqueName
    )]
    
    return(count)
}





#### Tools to perform resampling: ####

# Function to sample after sorting:
sampleSorted <- function(x, size, seed, replace = TRUE, sorted = TRUE){
    # If not given, get the size of the sample as the length of the vector:
    lx <- length(x)
    if(missing(size)){
        size <- lx
    }
    if(sorted){
        x <- sort(x)
    }
    # Sample:
    set.seed(seed)
    sampled <- x[sample.int(lx, size = size, replace = replace)]
    return(sampled)
}

getSeedVector <- function(seed, size = 1) {
    set.seed(seed)
    sample(getSequenceToSampleFrom(), size, replace = FALSE)
}

getSequenceToSampleFrom <- function(){
    seedSequenceLength <- getRstoxFrameworkDefinitions("seedSequenceLength")
    seq_len(seedSequenceLength)
}

