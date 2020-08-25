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
    NumberOfBootstraps = 1L#, 
    #NumberOfCores = integer()
) {
    
    # Identify the first process set for resampling by the BootstrapMethodTable:
    processIndexTable <- readProcessIndexTable(projectPath, modelName = "baseline")
    startProcess <- min(match(BootstrapMethodTable$ProcessName, processIndexTable$processName))
    lastProcessID <- utils::tail(processIndexTable$processID, 1)
    
    # Run the baseline until the startProcess:
    activeProcessID <- getProcessIndexFromProcessID(
        projectPath = projectPath, 
        modelName = "baseline", 
        processID = getActiveProcess(
            projectPath = projectPath, 
            modelName = "baseline"
        )$processID
    )
    preRunTo <- startProcess - 1
    if(activeProcessID < preRunTo) {
        temp <- runProcesses(projectPath, modelName = "baseline", endProcess = preRunTo)
    }
    
    # Draw the seeds in a table, and then split into a list for each bootstrap run:
    SeedTable <- data.table::as.data.table(lapply(BootstrapMethodTable$Seed, RstoxBase::getSeedVector, size = NumberOfBootstraps))
    names(SeedTable) <- BootstrapMethodTable$ProcessName
    SeedList <- split(SeedTable, seq_len(nrow(SeedTable)))
    
    
    
    
    
    createReplaceData <- function(Seed, BootstrapMethodTable) {
        lapply(Seed, function(x) list(MoreArgs = list(Seed = x)))
    }
    addFunctionNameToReplaceData <- function(replaceData, BootstrapMethodTable) {
        #browser()
        out <- lapply(names(replaceData), function(name) 
            c(
                list(FunctionName = BootstrapMethodTable[ProcessName == name, ResampleFunction]), 
                replaceData[[name]]
            ))
        names(out) <- names(replaceData)
        return(out)
    }
    replaceData <- lapply(SeedList, createReplaceData, BootstrapMethodTable = BootstrapMethodTable)
    replaceData <- lapply(replaceData, addFunctionNameToReplaceData, BootstrapMethodTable = BootstrapMethodTable)
    
    # Run the subset of the baseline model:
    bootstrapIndex <- seq_len(NumberOfBootstraps)
    BootstrapData <- RstoxData::mapplyOnCores(
        FUN = runOneBootstrapSaveLastOutput, 
        #NumberOfCores = NumberOfCores, 
        NumberOfCores = 1, 
        # Vector inputs:
        ind = bootstrapIndex, 
        Seed = SeedList, 
        replaceData = replaceData, 
        # Other inputs:
        MoreArgs = list(
            projectPath = projectPath, 
            startProcess = startProcess, 
            lastProcessID = lastProcessID
        )
        #replaceData = replaceData, 
        #projectPath = projectPath, 
        #startProcess = startProcess, 
        #lastProcessID = lastProcessID, 
        #NumberOfBootstraps = NumberOfBootstraps
    )
    
    if(is.list(BootstrapData[[1]]) && !data.table::is.data.table(BootstrapData[[1]])){
        tableNames <- names(BootstrapData[[1]])
        BootstrapData <- lapply(tableNames, rbindlistByName, x = BootstrapData)
        names(BootstrapData) <- tableNames
    }
    else {
        BootstrapData <- structure(list(data.table::rbindlist(BootstrapData)), names = names(BootstrapData[[1]]))
    }
    #stop(1231)
    
    return(BootstrapData)
}


# Define a function to run processes and save the output of the last process to the output folder:
runOneBootstrapSaveLastOutput <- function(ind, Seed, replaceData, projectPath, startProcess, lastProcessID) {
    
    # Re-run the baseline:
    runProcesses(projectPath, modelName = "baseline", startProcess = startProcess, endProcess = Inf, save = FALSE, fileOutput = FALSE, setUseProcessDataToTRUE = FALSE, replaceData = replaceData, msg = FALSE)
    # ... and get the last process output:
    lastProcessOutput <- getProcessOutput(projectPath, modelName = "baseline", processID = lastProcessID, drop.datatype = FALSE)
    
    # Add Bootstrap run ID:
    for(dataType in names(lastProcessOutput)) {
        lastProcessOutput[[dataType]][, BootstrapID := ..ind]
    }
    
    return(lastProcessOutput)
    
    # Get the resample function for this bootstrap run:
    ###  thisReplaceData <- lapply(replaceData, "[[", ind)
    # Re-run the baseline:
    ### runProcesses(projectPath, modelName = "baseline", startProcess = startProcess, endProcess = Inf, save = FALSE, fileOutput = FALSE, setUseProcessDataToTRUE = FALSE, replaceData = thisReplaceData, msg = FALSE)
    # Get the last process output:
    ###  lastProcessOutput <- getProcessOutput(projectPath, modelName = "baseline", processID = lastProcessID, drop.datatype = FALSE)
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
    ### for(dataType in names(lastProcessOutput)) {
    ###     lastProcessOutput[[dataType]][, BootstrapID := ..ind]
    ### }
    ### 
    ### return(lastProcessOutput)
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
    seeds <- RstoxBase::getSeedVector(x$Seed, size)
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
        seed = RstoxBase::getSeedVector(seed, size = length(uniqueResampleBy))
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
    resampled <- RstoxBase::sampleSorted(unique(subData[[varToResample]]), seed = seed, replace = TRUE)
    # Tabulate the resamples:
    #resampleTable <- table(resampled)
    #if(!length(resampleTable)) {
    #    
    #}
    resampleTable <- data.table::as.data.table(table(resampled, useNA = "ifany"))
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
ResampleMeanLengthDistributionData <- function(MeanLengthDistributionData, Seed) {
    # Resample PSUs within Strata, modifying the weighting variable of MeanLengthDistributionData:
    MeanLengthDistributionData$Data <- resampleDataBy(
        data = MeanLengthDistributionData$Data, 
        seed = Seed, 
        #varToScale = RstoxBase::getRstoxBaseDefinitions("dataTypeDefinition")[["MeanLengthDistributionData"]]$weighting, 
        varToScale = "WeightedCount", 
        varToResample = "PSU", 
        resampleBy = "Stratum"
    )
    
    return(MeanLengthDistributionData)
}

### #' Resamples biotic PSUs
### #' 
### #' This function resamples biotic PSUs with replacement within each Stratum, changing the MeanLengthDistributionWeight.
### #' 
### #' @param MeanLengthDistributionData The \code{\link[RstoxBase]{MeanLengthDistributionData}} data.
### #' @param Seed The seed, given as a sinigle initeger.
### #' 
### #' @export
### #' 
### ResampleAssignmentLengthDistributionData <- function(AssignmentLengthDistributionData, Seed) {
###     # Resample PSUs within Strata, modifying the weighting variable of AssignmentLengthDistributionData:
###     AssignmentLengthDistributionData <- resampleDataBy(
###         data = AssignmentLengthDistributionData, 
###         seed = Seed, 
###         #varToScale = RstoxBase::getRstoxBaseDefinitions("dataTypeDefinition")[["AssignmentLengthDistributionData"]]$weighting, 
###         varToScale = "WeightedCount", 
###         varToResample = "PSU", 
###         resampleBy = "Stratum"
###     )
###     
###     return(AssignmentLengthDistributionData)
### }


#' Resamples biotic PSUs
#' 
#' This function resamples biotic PSUs with replacement within each Stratum, changing the MeanLengthDistributionWeight.
#' 
#' @param MeanLengthDistributionData The \code{\link[RstoxBase]{MeanLengthDistributionData}} data.
#' @param Seed The seed, given as a sinigle initeger.
#' 
#' @export
#' 
ResampleBioticAssignment <- function(BioticAssignment, Seed) {
    # Resample PSUs within Strata, modifying the weighting variable of BioticAssignment:
    BioticAssignment <- resampleDataBy(
        data = BioticAssignment, 
        seed = Seed, 
        varToScale = "WeightingFactor", 
        varToResample = "Haul", 
        resampleBy = "Stratum"
    )
    
    return(BioticAssignment)
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
ResampleMeanNASCData <- function(MeanNASC, Seed) {
    # Resample PSUs within Strata, modifying the weighting variable of MeanLengthDistributionData:
    MeanNASC$Data <- resampleDataBy(
        data = MeanNASC$Data, 
        seed = Seed, 
        #varToScale = RstoxBase::getRstoxBaseDefinitions("dataTypeDefinition")[["MeanNASC"]]$weighting, 
        varToScale = "NASC", 
        varToResample = "PSU", 
        resampleBy = "Stratum"
    )
    
    return(MeanNASC)
}
