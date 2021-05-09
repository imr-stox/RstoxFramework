#' Bootstrap the baseline model
#' 
#' Run a subset of the baseline model a number of times after resampling e.g. Hauls in each Stratum, EDUSs in each Stratum.
#' 
#' @param outputData The output of the function from an earlier run.
#' @param projectPath The path to the project to containing the baseline to bootstrap.
#' @param BootstrapMethodTable A table of the columns ProcessName, ResampleFunction and Seed, where each row defines the resample function to apply to the output of the given process (and the seed to use in the resampling). Run RstoxFramework::getResampleFunctions() to get a list of the implemented resample functions. Note that if a process is selected inn \code{BootstrapMethodTable} that is not used in the model up to the \code{OutputProcesses}, the bootstrapping of that process will not be effective on the end result (e.g. select the correct process that returns BioticAssignment data type).
#' @param NumberOfBootstraps Integer: The number of bootstrap replicates.
#' @param OutputProcesses A vector of the processes to save from each bootstrap replicate.
#' @param UseOutputData Logical: If TRUE use the existing output file from the function.
#' @param NumberOfCores The number of cores to use for parallel processing. A copy of fthe project is created in tempdir() for each core.
#' @param BaselineSeed The seed to use for any processes requiring a \code{Seed} parameter, currenly only the \code{\link[RstoxBase]{ImputeSuperIndividuals}}.
#' 
#' @return
#' A list of the RstoxData \code{\link[RstoxData]{DataTypes}} and RstoxBase \code{\link[RstoxBase]{DataTypes}}.
#' 
#' @export
#' 
Bootstrap <- function(
    outputData, 
    projectPath, 
    # Table with ProcessName, ResamplingFunction, ResampleWithin, Seed:
    BootstrapMethodTable = data.table::data.table(), 
    NumberOfBootstraps = 1L, 
    OutputProcesses = character(), 
    UseOutputData = FALSE, 
    NumberOfCores = 1L, 
    BaselineSeed = 1L
) {
    
    # Use preivously generated output data if specified:
    if(UseOutputData) {
        # This was moved to getFunctionArguments() on 2020-10-22:
        #outputData <- get(load(outputDataPath))
        return(outputData)
    }
    
    # Identify the first process set for resampling by the BootstrapMethodTable (here BootstrapMethodTable$ProcessName can be a vector, in which case the table is returned from the first process of these and onwards):
    processIndexTable <- readProcessIndexTable(projectPath, modelName = "baseline", startProcess = BootstrapMethodTable$ProcessName, endProcess = OutputProcesses, return.processIndex = TRUE)
    
    # Check the output processes:
    if(length(OutputProcesses) && !all(OutputProcesses %in% processIndexTable$processName)) {
        warning("StoX: The following processes specified in OutputProcesses were not recognized: ", paste(setdiff(OutputProcesses, processIndexTable$processName), collapse = ", "))
        OutputProcesses <- intersect(OutputProcesses, processIndexTable$processName)
    }
    if(!length(OutputProcesses)) {
        stop("StoX: 
             At least one valid process name must be given in OutputProcesses")
    }
    
    # Run the baseline until the startProcess:
    activeProcessID <- getProcessIndexFromProcessID(
        projectPath = projectPath, 
        modelName = "baseline", 
        processID = getActiveProcess(
            projectPath = projectPath, 
            modelName = "baseline"
        )$processID
    )
    #preRunTo <- min(processIndexTable$processIndex) - 1
    preRunTo <- min(processIndexTable$processIndex)
    if(activeProcessID < preRunTo) {
        message("StoX: Running baseline until the first process to bootstrap...")
        temp <- runProcesses(
            projectPath, 
            modelName = "baseline", 
            endProcess = preRunTo, 
            save = FALSE, 
            saveProcessData = FALSE
        )
    }
    
    # Draw the seeds in a table, and then split into a list for each bootstrap run:
    SeedTable <- data.table::as.data.table(lapply(BootstrapMethodTable$Seed, RstoxBase::getSeedVector, size = NumberOfBootstraps))
    names(SeedTable) <- BootstrapMethodTable$ProcessName
    SeedList <- split(SeedTable, seq_len(nrow(SeedTable)))
    
    # Create the replaceDataList input to runProcesses, which defines the seed for each bootstrap run:
    replaceDataList <- createReplaceData(Seed = SeedList, BootstrapMethodTable = BootstrapMethodTable)
    
    # This should be exposed as a parameter.
    if(!length(BaselineSeed)) {
        stop("The BaselineSeed (used to set the seeds of any seed used in the Baseline model, currently only applied in the ImputeSuperIndividuals function) must be set.")
    }
    BaselineSeedVector <- RstoxBase::getSeedVector(BaselineSeed, size = NumberOfBootstraps)
    replaceArgs <- lapply(BaselineSeedVector, function(x) list(Seed = x))
    
    # Get the number of cores to open:
    NumberOfCores <- RstoxData::getNumberOfCores(NumberOfCores, n  = NumberOfBootstraps)
    
    # Copy the projec to the tempdir for each core:
    projecName <- basename(projectPath)
    if(NumberOfCores > 1)  {
        projectPath_copies <- file.path(tempdir(), paste0(projecName, seq_len(NumberOfCores)))
        #mapply(copyProject, projectPath, projectPath_copies, ow = TRUE)
        RstoxData::mapplyOnCores(copyProject, MoreArgs = list(ow = TRUE, projectPath = projectPath), projectPath_copies, NumberOfCores = NumberOfCores)
    }
    else {
        projectPath_copies <- projectPath
    }
    
    
    # Run the subset of the baseline model:
    bootstrapProgressFile <- getProjectPaths(projectPath, "bootstrapProgressFile")
    NumberOfBootstrapsFile <- getProjectPaths(projectPath, "NumberOfBootstrapsFile")
    stopBootstrapFile <- getProjectPaths(projectPath, "stopBootstrapFile")
    if(file.exists(stopBootstrapFile)) {
        unlink(stopBootstrapFile, force = TRUE, recursive = TRUE)
    }
    
    writeLines(as.character(NumberOfBootstraps), NumberOfBootstrapsFile)
    
    
    bootstrapIndex <- seq_len(NumberOfBootstraps)
    BootstrapData <- RstoxData::mapplyOnCores(
        FUN = runOneBootstrapSaveOutput, 
        NumberOfCores = NumberOfCores, 
        # Vector inputs:
        ind = bootstrapIndex, 
        Seed = SeedList, 
        replaceArgs = replaceArgs, 
        replaceDataList = replaceDataList, 
        projectPath = projectPath_copies, 
        
        # Other inputs:
        MoreArgs = list(
            projectPath_original = projectPath, 
            startProcess = min(processIndexTable$processIndex), 
            endProcess = max(processIndexTable$processIndex), 
            outputProcessesIDs = OutputProcesses, 
            bootstrapProgressFile = bootstrapProgressFile
        )
    )
    
    # Here we need to merge the NeCDF4 bootstrap files, when we get these files implemented. For now all bootstrap data are accumulated in memory and dumped to an RData file.
    
    # Changed on 2020-11-02 to run the baseline out after bootstrapping (with no modification, so a clean baseline run):
    ### # Reset the model to the last process before the bootstrapped processes:
    ### resetModel(
    ###     projectPath = projectPath, 
    ###     modelName = "baseline", 
    ###     processID = processIndexTable$processID[1], 
    ###     processDirty = FALSE, 
    ###     shift = -1
    ### )
    # Rerun the baseline to the end:
    temp <- runProcesses(
        projectPath, 
        modelName = "baseline", 
        startProcess = processIndexTable$processIndex[1], 
        save = FALSE, 
        saveProcessData = FALSE
    )
    
    # Get the data types to return:
    dataTypeNames <- names(BootstrapData[[1]])
    BootstrapData <- lapply(dataTypeNames, rbindlistByName, x = BootstrapData)
    names(BootstrapData) <- dataTypeNames
    
    # Drop the list for data types with only one table:
    for(dataType in names(BootstrapData)) {
        if(isProcessOutputDataType(BootstrapData[[dataType]])) {
            BootstrapData[[dataType]] <- BootstrapData[[dataType]][[1]]
        }
    }
    
    unlink(NumberOfBootstrapsFile, force = TRUE, recursive = TRUE)
    unlink(bootstrapProgressFile, force = TRUE, recursive = TRUE)
    
    return(BootstrapData)
}





rbindlistByName <- function(name, x) {
    subnames <- names(x[[1]][[name]])
    out <- lapply(subnames, rbindSublistByName, name, x)
    names(out) <- subnames
    return(out)
}
rbindSublistByName <- function(subname, name, x) {
    data.table::rbindlist(lapply(x, function(y) y[[name]][[subname]]))
}


# Function to create a list of the ReplaceData input to runProcesses(), 
createReplaceDataSansFunctionName <- function(Seed) {
    lapply(Seed, function(x) list(MoreArgs = list(Seed = x)))
}
addFunctionNameToReplaceData <- function(replaceData, BootstrapMethodTable) {
    out <- lapply(names(replaceData), function(name) 
        c(
            list(FunctionName = BootstrapMethodTable[ProcessName == name, ResampleFunction]), 
            replaceData[[name]]
        ))
    # Add the process names to the list to enable replaceDataList in runProcesses():
    names(out) <- names(replaceData)
    return(out)
}
createReplaceData <- function(Seed, BootstrapMethodTable) {
    replaceDataList <- lapply(Seed, createReplaceDataSansFunctionName)
    replaceDataList <- lapply(replaceDataList, addFunctionNameToReplaceData, BootstrapMethodTable = BootstrapMethodTable)
    return(replaceDataList)
}


# Define a function to run processes and save the output of the last process to the output folder:
runOneBootstrapSaveOutput <- function(ind, Seed, replaceArgs, replaceDataList, projectPath, projectPath_original, startProcess, endProcess, outputProcessesIDs, bootstrapProgressFile) {
    
    # Stop if the file stopBootstrap.txt exists:
    if(file.exists(getProjectPaths(projectPath_original, "stopBootstrapFile"))) {
        stop("Bootstrap aborted")
    }
    
    # Re-run the baseline:
    runProcesses(
        projectPath, 
        modelName = "baseline", 
        startProcess = startProcess, 
        endProcess = endProcess, 
        save = FALSE, 
        saveProcessData = FALSE, 
        fileOutput = FALSE, 
        setUseProcessDataToTRUE = FALSE, 
        replaceArgs = replaceArgs, 
        replaceDataList = replaceDataList, 
        msg = FALSE
    )
    
    # Get the requested outputs:
    processOutput <- getModelData(
        projectPath = projectPath, 
        modelName = "baseline", 
        processes = outputProcessesIDs, 
        drop.datatype = FALSE
    )
    
    # Add bootstrap IDs to the processOutput:
    for(dataType in names(processOutput)) {
        for(table in names(processOutput[[dataType]])) {
            # This did not work for some reason (https://stackoverflow.com/questions/51877642/adding-a-column-by-reference-to-every-data-table-in-a-list-does-not-stick):
            # processOutput[[dataType]][[table]][, BootstrapID := ..ind]
            # So we do it not by reference:
            processOutput[[dataType]][[table]]$BootstrapID <- ind
        }
    }
    
    # Add a dot to the progess file:
    cat(".", file = bootstrapProgressFile, append = TRUE)
    
    return(processOutput)
}


#' Stop a bootstrap run.
#' 
#' @inheritParams general_arguments
#' 
#' @export
#' 
stopBootstrap <- function(projectPath) {
    write("", file = getProjectPaths(projectPath, "stopBootstrapFile"))
}
#' Get bootstrap progress
#' 
#' @inheritParams general_arguments
#' @param percent Logical: If TRUE return the progress in percent, otherwise in [0,1].
#' 
#' @export
#' 
getBootstrapProgress <- function(projectPath, percent = FALSE) {
    bootstrapProgressFile <- getProjectPaths(projectPath, "bootstrapProgressFile")
    NumberOfBootstrapsFile <- getProjectPaths(projectPath, "NumberOfBootstrapsFile")
    NumberOfBootstraps <- as.numeric(readLines(NumberOfBootstrapsFile, warn = FALSE))
    bootstrapProgress <- readLines(bootstrapProgressFile, warn = FALSE)
    NumberOfSuccessfulBootstraps <- lengths(regmatches(bootstrapProgress, gregexpr(".", bootstrapProgress)))
    bootstrapProgress <- NumberOfSuccessfulBootstraps / NumberOfBootstraps
    if(percent) {
        bootstrapProgress <- bootstrapProgress * 100
    }
    return(bootstrapProgress)
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


#getReplaceData <- function(x, size) {
#    # Get seeds:
#    seeds <- RstoxBase::getSeedVector(x$Seed, size)
#    x_expanded <- data.table::data.table(
#        x[, !"Seed"], 
#        Seed = seeds
#    )
#    
#    # Split rows into a list:
#    x_split <- split(x_expanded, by = "Seed")
#    
#    x_split <- lapply(x_split, function(x) list(x$ResampleFunction, as.list(x[, !c("ResampleFunction")])))
#    
#    return(x_split)
#}




#' Resamples StoX data
#' 
#' This function resamples varToResample with replacement by altering the input data
#' 
#' @export
#' 
resampleDataBy <- function(data, seed, varToScale, varToResample, resampleBy) {
    
    # Get the unique resampleBy, and sort in C-locale for consistensy across platforms:
    #uniqueResampleBy <- unique(data[[resampleBy]])
    uniqueResampleBy <- stringi::stri_sort(unique(data[[resampleBy]]), locale = "C")
    
    
    # Build a table of Stratum and Seed and merge with the MeanLengthDistributionData:
    seedTable <- data.table::data.table(
        resampleBy = uniqueResampleBy, 
        seed = RstoxBase::getSeedVector(seed, size = length(uniqueResampleBy))
    )
    data.table::setnames(seedTable, c(resampleBy, "seed"))
    data <- merge(data, seedTable, by = resampleBy)
    
    # Resample the data:
    data[, eval(varToScale) := resampleOne(.SD, seed = seed[1], varToScale = varToScale, varToResample = varToResample), by = resampleBy]
    
    data[, seed := NULL]
    #return(MeanLengthDistributionData)
}

# Function to resample Hauls of one subset of the data:
resampleOne <- function(subData, seed, varToResample, varToScale) {
    
    # Resample the unique:
    if(length(varToResample) != 1) {
        stop("StoX: varToResample must be a single string naming the variable to resample")
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
    # The sort = FALSE is vvery important, as it retains the order of the data. This should probably be replaced by a more robust solution, e.g. by merging in resampleDataBy():
    count <- merge(subData, resampleTable, by = varToResample, all.x = TRUE, sort = FALSE)
    
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
    # Resample Hauls within Strata, modifying the weighting variable of BioticAssignment:
    BioticAssignment <- resampleDataBy(
        #data = BioticAssignment$BioticAssignment, 
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
ResampleMeanNASCData <- function(MeanNASCData, Seed) {
    # Resample PSUs within Strata, modifying the weighting variable of MeanLengthDistributionData:
    MeanNASCData$Data <- resampleDataBy(
        data = MeanNASCData$Data, 
        seed = Seed, 
        #varToScale = RstoxBase::getRstoxBaseDefinitions("dataTypeDefinition")[["MeanNASC"]]$weighting, 
        varToScale = "NASC", 
        varToResample = "PSU", 
        resampleBy = "Stratum"
    )
    
    return(MeanNASCData)
}



##################################################
##################################################
#' Report Bootstrap
#' 
#' Reports the sum, mean or other functions on a variable of the \code{\link{BootstrapData}}.
#' 
#' @inheritParams RstoxBase::ModelData
#' @inheritParams RstoxBase::ProcessData
#' @inheritParams RstoxBase::general_report_arguments
#' @param BaselineProcess A strings naming the baseline process to report from the boostrap output.
#' @param AggregationFunction The function to apply to each bootstrap run. This must be a function returning a single value.
#' @param BootstrapReportFunction The function to apply across bootstrap run, such as "cv" or "stoxSummary".
#' @param AggregationWeightingVariable The variable to weight by in the \code{AggregationFunction}.
#' @param BootstrapReportWeightingVariable The variable to weight by in the \code{BootstrapReportFunction}.
#'
#' @details This function is useful to, e.g, sum Biomass for each SpeciesCategory and IndividualTotalLenght, or average IndividualTotalLenght for each IndiivdualAge and Stratum.
#' 
#' @return
#' A \code{\link{ReportBootstrapData}} object.
#' 
#' @examples
#' 
#' @seealso 
#' 
#' @export
#' 
ReportBootstrap <- function(
    BootstrapData, 
    BaselineProcess = character(), 
    TargetVariable = character(), 
    AggregationFunction = RstoxBase::getReportFunctions(getMultiple = FALSE), 
    BootstrapReportFunction = RstoxBase::getReportFunctions(getMultiple = TRUE), 
    GroupingVariables = character(), 
    RemoveMissingValues = FALSE, 
    AggregationWeightingVariable = character(), 
    BootstrapReportWeightingVariable = character()
) 
{
    # Run the initial aggregation (only applicable for single output functions):
    AggregationFunction <- match.arg(AggregationFunction)
    out <- RstoxBase::aggregateBaselineDataOneTable(
        stoxData = BootstrapData[[BaselineProcess]], 
        TargetVariable = TargetVariable, 
        aggregationFunction = AggregationFunction, 
        GroupingVariables = c(GroupingVariables, "BootstrapID"), 
        na.rm = RemoveMissingValues, 
        WeightingVariable = AggregationWeightingVariable
    )
    
    
    # Get the name of the new TargetVariable:
    TargetVariableAfterInitialAggregation <- RstoxBase::getReportFunctionVariableName(
        functionName = AggregationFunction, 
        TargetVariable = TargetVariable
    )
    
    # Run the report function of the bootstraps:
    BootstrapReportFunction <- match.arg(BootstrapReportFunction)
    out <- RstoxBase::aggregateBaselineDataOneTable(
        stoxData = out, 
        TargetVariable = TargetVariableAfterInitialAggregation, 
        aggregationFunction = BootstrapReportFunction, 
        GroupingVariables = GroupingVariables, 
        na.rm = RemoveMissingValues, 
        padWithZerosOn = "BootstrapID", 
        WeightingVariable = BootstrapReportWeightingVariable
    )
    
    return(out)
}

