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
    
    # Run the baseline until the startProcess:
    temp <- runProcesses(projectPath, modelName = "baseline", endProcess = startProcess - 1)
    
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
        # Save the last process output:
        ###writeProcessOutputMemoryFiles(
        ###    processOutput = lastProcessOutput, 
        ###    projectPath = projectPath, 
        ###    modelName = "analysis", 
        ###    processID = lastProcessID, 
        ###    type = "output", 
        ###    subfolder = getBootstrapRunName(ind, NumberOfBootstraps, prefix = "Run")
        ###)
        
        # Add Bootstrap run ID:
        #addBootstrapID(
        #    lastProcessOutput, 
        #    ind = ind
        #)
        for(dataType in names(lastProcessOutput)) {
            lastProcessOutput[[dataType]][, BootstrapID := ..ind]
        }
        
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

addBootstrapID <- function(x, ind) {
    if(is.list(x) && !data.table::is.data.table(x)){
        lapply(x, addBootstrapIDOne, ind = ind)
    }
    else {
        addBootstrapIDOne(x, ind = ind)
    }
}

addBootstrapIDOne <- function(x, ind) {
    x[, BootstrapID := ..ind]
}


rbindlistByName <- function(name, x) {
    data.table::rbindlist(lapply(x, "[[", name))
}

#readBootstrap <- function(projectPath, processName) {
#    # Bootstrap lives in the model "analysis" only:
#    modelName = "analysis"
#    # Get the folder holding the output form the bootstrap:
#    processID <- getProcessIDFromProcessName(projectPath = projectPath, modelName = modelName, processName = processName)
#    folderPath <- getProcessOutputFolder(projectPath = projectPath, modelName = modelName, processID = processID, type = "output")
#    
#    # Return NULL if the folder does not exist:
#    if(!file.exists(folderPath)) {
#        warning("StoX: The process ", folderPath, " does not have an output folder. Has the proccess been run?")
#        return(NULL)
#    }
#    
#    # Get a nested list of all output files from the bootstrap:
#    bootstrapOutputFiles <- getFilesRecursiveWithOrder(folderPath)
#    
#    # Read all files:
#    processOutput <- rapply(
#        bootstrapOutputFiles, 
#        readProcessOutputFile, 
#        how = "replace"
#    )
#    
#}


# # Function to get the name of the bootsrap run, which will be used as the subfolder name hoding the output from the bootstrap:
# getBootstrapRunName <- function(ind, NumberOfBootstraps, prefix = "Run") {
#     paste0(
#         prefix, 
#         formatC(ind, width = NumberOfBootstraps, format = "d", flag = "0")
#     )
# }


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




#' Resamples biotic PSUs
#' 
#' This function resamples PSUs with replacement by altering the LengthDistributionWeight.
#' 
#' @param LengthDistributionData The \code{\link[RstoxBase]{LengthDistributionData}} to resample Hauls in.
#' 
#' @export
#' 
resampleDataBy <- function(data, seed, varToScale, varToResample, resampleBy) {
    
    # Get the unique resampleBy:
    uniqueResampleBy <- unique(data[[resampleBy]])
    
    # Build a table of Stratum and Seed and merge with the MeanLengthDistributionData:
    seedTable <- data.table::data.table(
        resampleBy = uniqueResampleBy, 
        seed = getSeedVector(seed, size = length(uniqueResampleBy))
    )
    data.table::setnames(seedTable, c(resampleBy, "seed"))
    data <- merge(data, seedTable, by = resampleBy)
    
    # Resample the Hauls:
    data[, eval(varToScale) := resampleOne(.SD, seed = seed[1], varToScale = varToScale, varToResample = varToResample), by = resampleBy]
    
    data[, seed := NULL]
    #return(MeanLengthDistributionData)
}

# Function to resample Hauls of one subset of the data:
resampleOne <- function(subData, seed, varToResample, varToScale) {
    
    # Resample the unique:
    if(length(varToResample) != 1) {
        stop("varToResample must be a single string naming the variable to resample")
    }
    resampled <- sampleSorted(unique(subData[[varToResample]]), seed = seed, replace = TRUE)
    # Tabulate the resamples:
    resampleTable <- data.table::as.data.table(table(resampled))
    # Set an unmistakable name to the counts:
    data.table::setnames(resampleTable, c("resampled","N"), c(varToResample, "resampledCountWithUniqueName"))
    
    # Merge the resampled counts into the data:
    count <- merge(subData, resampleTable, by = varToResample, all = TRUE)
    
    # Insert the new count into WeightedCount (with NAs replaced by 0):
    count[, resampledCountWithUniqueName := ifelse(
        is.na(count$resampledCountWithUniqueName), 
        0, 
        count$resampledCountWithUniqueName
    )]
    
    #count[, eval(varToScale) := lapply(get(varToScale), "*", resampledCountWithUniqueName)]
    
    for(var in varToScale) {
        count[, eval(var) := resampledCountWithUniqueName * get(var)]
    }
    
    return(count[[varToScale]])
}


#' Resamples biotic PSUs
#' 
#' This function resamples biotic PSUs with replacement within each Stratum, changing the MeanLengthDistributionWeight.
#' 
#' @param MeanLengthDistributionData The \code{\link[RstoxBase]{MeanLengthDistributionData}} data.
#' @param Seed The seed, given as a sinigle initeger.
#' 
#' @export
#' 
ResampleBioticPSUs <- function(MeanLengthDistributionData, Seed) {
    # Resample PSUs within Strata, modifying the weighting avriable of MeanLengthDistributionData:
    MeanLengthDistributionData$Data <- resampleDataBy(
        data = MeanLengthDistributionData$Data, 
        seed = Seed, 
        varToScale = RstoxBase::getRstoxBaseDefinitions("dataTypeDefinition")[["MeanLengthDistributionData"]]$weighting, 
        varToResample = "PSU", 
        resampleBy = "Stratum"
    )
    
    return(MeanLengthDistributionData)
}

#' Resamples acoustic PSUs
#' 
#' This function resamples acoustic PSUs with replacement within each Stratum, changing the MeanNASC
#' 
#' @param MeanNASC The \code{\link[RstoxBase]{MeanNASC}} data.
#' @param Seed The seed, given as a sinigle initeger.
#' 
#' @export
#' 
ResampleAcousticPSUs <- function(MeanNASC, Seed) {
    # Resample PSUs within Strata, modifying the weighting avriable of MeanLengthDistributionData:
    MeanNASC$Data <- resampleDataBy(
        data = MeanNASC$Data, 
        seed = Seed, 
        varToScale = RstoxBase::getRstoxBaseDefinitions("dataTypeDefinition")[["MeanNASC"]]$weighting, 
        varToResample = "PSU", 
        resampleBy = "Stratum"
    )
    
    return(MeanNASC)
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

