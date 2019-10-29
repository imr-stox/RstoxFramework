##### Models: #####

#' 
#' @export
#' 
getModelNames <- function() {
    getRstoxFrameworkDefinitions("stoxModelTypes")
}
#' 
#' @export
#' 
getModelInfo <- function() {
    getRstoxFrameworkDefinitions("stoxModelInfo")
}

##########


##### Templates: #####

#' 
#' @export
#' 
getAvailableTemplatesDescriptions <- function() {
    # Get the evailable templates:
    availableTemplates <- getAvaiableTemplates(TRUE)
    # Return the tempates as a data frame of name and description:
    data.table::data.table(
        name = names(availableTemplates), 
        description = sapply(availableTemplates, attr, "description")
    )
} 
##########


##### Processes: #####

# Function to get whether the process has input data error:
getFunctionInputErrors <- function(ind, processTable, functionInputs) {
    
    #### Check wheter the processes from which process output is requested as funciton input exist prior to the current process: ####
    # Get names of processes prior to the current process:
    priorProcesses <- processTable$processName[seq_len(ind - 1)]
    # Get the names of the processes from which funciton intpu is requested:
    requestedProcessNames <- unlist(functionInputs[[ind]])
    requestedFunctionInputDataTypes <- names(requestedProcessNames)
    
    # Are all of the function inputs present in the prior processes?:
    requestedProcessesExist <- all(requestedProcessNames %in% priorProcesses)
    correctDataTypeRequested <- FALSE
    
    if(!requestedProcessesExist) {
        warning(
            "The following requested processes do not exist prior to the process", 
            processTable$processName[ind], 
            ": ", 
            paste(setdiff(requestedProcessNames, priorProcesses), collapse = ", ")
        )
    }
    #### Check also that the processes given by the function inputs acutally return the desired data type: ####
    else {
        # Get the indices of these processes in the processTable:
        indexOfRelevantPriorProcesses <- match(requestedProcessNames, priorProcesses)
        # Match the output data types of the relevant prior processes:
        correctDataTypeRequested <- processTable$dataType[indexOfRelevantPriorProcesses] == requestedFunctionInputDataTypes
        
        
        if(!all(correctDataTypeRequested)) {
            warning(
                "The following processes do not have the requested data type output: ", 
                paste(processTable$dataType[indexOfRelevantPriorProcesses][requestedProcessNames], collapse = ", ")
            )
        }
    }
    
    # Return TRUE if error:
    !(requestedProcessesExist && all(correctDataTypeRequested))
}


getCurrentProcessID <- function(projectPath, modelName) {
    # Get the path to the currentProcessFile:
    currentProcessFile <- getProjectPaths(projectPath, "currentProcessFile")
    # Missing file implies not saved:
    if(!file.exists(currentProcessFile)) {
        FALSE
    }
    else {
        as.logical(readLines(currentProcessFile)[1])
    }
}


getCanShowInMap <- function(functionNames) {
    # Get the data types returned by the functions of the processes:
    dataTypes <- sapply(functionNames, getFunctionOutputDataType)
    # Are the datatypes of the dataTypesToShowInMap?:
    isTRUE(dataTypes %in% getRstoxFrameworkDefinitions("dataTypesToShowInMap"))
}

##########


##### Interactive: #####

# RunProcess In the GUI

# In a for loop:
# 1. runProcess()
# If success:
#   1.2. getMapMode()
#   1.3. getMapData()
#   1.4. getInteractiveMode()
#   1.5. getInteractiveData()
#   1.6. getLog
# 

# Function for getting the interactive mode of the process:
getInteractiveMode <- function(projectPath, modelName, processID) {
    
    # Get the data type of the process:
    dataType <- getDataType(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID
    )
    
    # Select the type if interactive mode depending on the output data type from the process:
    if(dataType %in% getRstoxFrameworkDefinitions("stratumDataTypes")) {
        "stratum"
    }
    else if(dataType %in% getRstoxFrameworkDefinitions("asouticPSUDataType")) {
        "asouticPSU"
    }
    else if(dataType %in% getRstoxFrameworkDefinitions("sweptAreaPSUDataType")) {
        "sweptAreaPSU"
    }
    else if(dataType %in% getRstoxFrameworkDefinitions("assignmentDataTypes")) {
        "assignment"
    }
    else {
        stop("Invalid dataType")
    }
}

# Function for getting the map mode of the process:
getMapMode <- function(projectPath, modelName, processID) {
    # Get the data type of the process:
    dataType <- getDataType(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID
    )
    
    # Select the type of interactive mode depending on the output data type from the process:
    if(dataType %in% getRstoxFrameworkDefinitions("strataDataTypes")) {
        "stratum"
    }
    else if(dataType %in% getRstoxFrameworkDefinitions("assignmentDataTypes")) {
        "station"
    }
    else if(dataType %in% getRstoxFrameworkDefinitions("EDSUDataType")) {
        "EDSU"
    }
    else {
        stop("Invalid dataType")
    }
}


# Functions for getting the appropriate process data from the process, called depending on the interactive mode:
getInteractiveData  <- function(projectPath, modelName, processID) {
    
    # Get the interactive mode:
    interactiveMode <- getInteractiveMode(projectPath, modelName, processID)
    
    # Call the appropriate function depending on the interactive mode:
    if(interactiveMode == "stratum") {
        getStratumData(
            projectPath = projectPath, 
            modelName = modelName, 
            processID = processID
        )
    }
    else if(interactiveMode == "assignment") {
        getAssignmentPSUData(
            projectPath = projectPath, 
            modelName = modelName, 
            processID = processID
        )
    }
    else if(interactiveMode == "asouticPSU") {
        getAcousticPSUData(
            projectPath = projectPath, 
            modelName = modelName, 
            processID = processID
        )
    }
    else if(interactiveMode == "sweptAreaPSU") {
        getSweptAreaPSUData(
            projectPath = projectPath, 
            modelName = modelName, 
            processID = processID
        )
    }
    else {
        stop("Invalid interactiveMode")
    }
}

# Functions for getting the appropriate map data from the process, called depending on the map mode:
getMapData  <- function(projectPath, modelName, processID) {
    
    # Get the interactive mode:
    mapMode <- getInteractiveMode(projectPath, modelName, processID)
    
    # Call the appropriate function depending on the interactive mode:
    if(mapMode == "stratum") {
        getStratumData(
            projectPath = projectPath, 
            modelName = modelName, 
            processID = processID
        )
    }
    else if(mapMode == "station") {
        getStationData(
            projectPath = projectPath, 
            modelName = modelName, 
            processID = processID
        )
    }
    else if(mapMode == "EDSU") {
        getSweptAreaPSUData(
            projectPath = projectPath, 
            modelName = modelName, 
            processID = processID
        )
    }
    else {
        stop("Invalid mapMode")
    }
}


# Individual get data functions:
getStratumData <- function(projectPath, modelName, processID) {
    
    # Get the process data:
    processData <- getProcessData(projectPath, modelName, processID)
    # Issue an error of the process data are not of StratumPolygon type:
    if(names(processData) != "StratumPolygon"){
        processName <- getProcessName(projectPath, modelName, processID)
        warning("The process ", processName, " does not return process data of type StratumPolygon")
        return(NULL)
    }
    
    # Create the objects EDSU_PSU, PSU_Stratum and Stratum
    stratumPolygon <- geojsonio::geojson_json(processData)
    #stratum <- data.table::data.table(
    #    stratum = names(processData), 
    #    includeInTotal = 
    #)
    
    stratumPolygon
}

getAcousticPSUData <- function(projectPath, modelName, processID) {
    
    # Get the process data:
    processData <- getProcessData(projectPath, modelName, processID)
    # Issue an error of the process data are not of AcousticPSU type:
    if(names(processData) != "AcousticPSU"){
        processName <- getProcessName(projectPath, modelName, processID)
        warning("The process ", processName, " does not return process data of type AcousticPSU")
        return(NULL)
    }
    
    # Create the objects EDSU_PSU, PSU_Stratum and Stratum
    EDSU_PSU <- processData$AcousticPSU[, c("EDSU", "PSU")]
    PSU_Stratum <- unique(processData$AcousticPSU[, c("PSU", "Stratum")])
    Stratum = unique(processData$AcousticPSU$Stratum)
    
    # Return the list of data.tables:
    list(
        EDSU_PSU = EDSU_PSU, 
        PSU_Stratum = PSU_Stratum, 
        Stratum = Stratum
    )
}

getSweptAreaPSUData <- function(projectPath, modelName, processID) {
    
    # Get the process data:
    processData <- getProcessData(projectPath, modelName, processID)
    # Issue an error of the process data are not of SweptAreaPSU type:
    if(names(processData) != "SweptAreaPSU"){
        processName <- getProcessName(projectPath, modelName, processID)
        warning("The process ", processName, " does not return process data of type SweptAreaPSU")
        return(NULL)
    }
    
    # Create the objects EDSU_PSU, PSU_Stratum and Stratum
    Station_PSU <- processData$SweptAreaPSU[, c("Station", "PSU")]
    PSU_Stratum <- unique(processData$SweptAreaPSU[, c("PSU", "Stratum")])
    Stratum = unique(processData$SweptAreaPSU$Stratum)
    
    # Return the list of data.tables:
    list(
        Station_PSU = Station_PSU, 
        PSU_Stratum = PSU_Stratum, 
        Stratum = Stratum
    )
}

getAssignmentData <- function(projectPath, modelName, processID) {
    
    # Get the process data:
    processData <- getProcessData(projectPath, modelName, processID)
    # Issue an error of the process data are not of Assignment type:
    if(names(processData) != "Assignment"){
        processName <- getProcessName(projectPath, modelName, processID)
        warning("The process ", processName, " does not return process data of type Assignment")
        return(NULL)
    }
    
    # Create the objects EDSU_PSU, PSU_Stratum and Stratum
    PSU_Layer_AssignmentID <- unique(processData$Assignment[, c("PSU", "Layer", "AssignmentID")])
    AssignmentID_Station_StationWeight <- unique(processData$Assignment[, c("AssignmentID", "Station", "StationWeight")])
    
    # Return the list of data.tables:
    list(
        PSU_Layer_AssignmentID = PSU_Layer_AssignmentID, 
        AssignmentID_Station_StationWeight = AssignmentID_Station_StationWeight
    )
}

getStationData <- function(projectPath, modelName, processID) {
    # Get the station data:
    getProcessOutput(projectPath, modelName, processID, tableName = "Station")$Station
}

getEDSUData <- function(projectPath, modelName, processID) {
    # Get the EDSU data:
    Log <- getProcessOutput(projectPath, modelName, processID, tableName = "Log")$Log
    extrapolateEDSU(Log)
}

# Add stop position of the EDSUs for plotting in the map:
extrapolateLongitudeLatitude <- function(StartLongitude, StartLatitude) {
    
    # Add stop longitude and latitude:
    stopLongitude <- StartLongitude[-1]
    stopLatitude <- StartLatitude[-1]
    # Get the vector of the last segment:
    lastLongitude <- diff(utils::tail(StartLongitude, 2))
    lastLatitude <- diff(utils::tail(StartLatitude, 2))
    # Add this vector to the last stop point to get the extrapolated final stop point:
    stopLongitudeFinal <- utils::tail(stopLongitude, 1) + lastLongitude
    stopLatitudeFinal <- utils::tail(stopLatitude, 1) + lastLatitude
    
    StopLongitude <- c(stopLongitude, lastLongitude)
    StopLatitude <- c(stopLatitude, lastLatitude)
    
    # Add mid longitude and latitude:
    MidLongitude <- (StartLongitude + StopLongitude) / 2
    MidLatitude <- (StartLatitude + StopLatitude) / 2
    
    StoxAcousticData <- data.table::data.table(
        StopLongitude = StopLongitude,
        StopLatitude = StopLatitude,
        MidLongitude = MidLongitude, 
        MidLatitude = MidLatitude
    )
    
    # Add a tag for extrapolated mid and stop positions:
    StoxAcousticData$Extrapolated <- c(
        logical(length(StartLongitude) - 1), 
        TRUE
    )
    
    # Return the StoxAcousticData:
    StoxAcousticData
}

extrapolateEDSU <- function(Log) {
    # Run the extrapolation function on each Paltform, effectively ordering the data by platform:
    Log[, extrapolateLongitudeLatitude(StartLongitude, StartLatitude), by = Platform]
}

##########



##### Process properties: #####

getProcessPropertyNames <- function() {
    
    
    
    #ellipsisMode: 
    #    filter, 
    #    fileSelector, 
    #    directorySelector, 
    #    length2tsAcousticCategory, 
    #    length2tsLayerPSU, 
    #    functionSelector (select dataType or package and then select funciton from the list),  
}

getProcessProperties <- function(process) {
    
    # Get the process properties depending on the processPropertyName:
    processParameters <- getRstoxFrameworkDefinitions("processParameters")
    processParametersDisplayNames <- getRstoxFrameworkDefinitions("processParametersDisplayNames")
    processParametersDescriptions <- getRstoxFrameworkDefinitions("processParametersDescriptions")
    
    ##### Define the process name, the function name and the process parameters as the process property "process": #####
    processArguments <- list(
        name = c("processName", "functionName", names(processParameters)), 
        displayName = c("Process name", "Function", unname(unlist(processParametersDisplayNames))), 
        defaultValue = c("", "", unname(unlist(processParameters))), 
        type = c("character", "character", sapply(processParameters, class)), 
        possibleValues = list(
            NULL, 
            NULL, 
            c(FALSE, TRUE), 
            c(FALSE, TRUE), 
            c(FALSE, TRUE)
        ), 
        description = c(
            "The name of the process, which must be unique within each model", 
            "The name of the function called by the process", 
            processParametersDescriptions
        )
    )
    # Add the values:
    processArguments$value <- process[processArguments$name]
    
    # Remove the showInMap argument if not relevant:
    if(!getCanShowInMap(process$functionName)) {
        keep <- process$name != "showInMap"
        process <- lapply(process, subset, keep)
    }
    
    
    ##### Define the funciton inputs: #####
    functionInputs <- list()
    if(length(process$functionName)) {
        functionInputNames <- names(process$functionInputs)
        name = functionInputNames
        displayName = functionInputNames
        possibleValues = getFunctionDefaults(process$functionName)[functionInputNames]
        defaultValue = sapply(possibleValues, "[[", 1)
        type = getParameterDataTypes(process$functionName)[functionInputNames]
        description = getFunctionArgumentDescriptions(process$functionName)
    }
    
    
    # functionParameters
        
        
}


# getFunctionOutputDataType
# getCanShowInMap
# getFunctionDefaults
# getParameterDataTypes
# getFunctionArgumentDescriptions





getFunctionArgumentDescriptions <- function(functionName, packageName) {
    
    # The package needs to be loaded to get the documentatino database:
    library(packageName, character.only = TRUE)
    # Get the Rd database:
    db <- tools::Rd_db(packageName)
    # Define a temporary file to write the Rd to
    tmp <- tempfile()
    # Paste the Rd to one string and write to the temporary file:
    thisRd <- as.character(db[[paste0(functionName, ".Rd")]])
    thisRd <- paste(thisRd, collapse = "")
    write(thisRd, tmp)
    # Read the file back in:
    p <- tools::parse_Rd(tmp)
    
    # Detect the arguments, and get the valid arguments as those which are not line breaks:
    atArguments <- sapply(p, attr, "Rd_tag") == "\\arguments"
    argumentNames <- unlist(lapply(p[atArguments][[1]], head, 1))
    validArgumentNames <- argumentNames != "\n"
    
    # Get and return the argument names and the descriptions:
    argumentNames <- subset(argumentNames, validArgumentNames)
    descriptions <- sapply(p[atArguments][[1]][validArgumentNames], function(x) paste0(unlist(x)[-1], collapse = " "))
    list(
        argumentNames = argumentNames, 
        descriptions = descriptions
    )
}


writeFunctionPDF <- function(functionName, packageName, pdfDir) {
    # We need to load the package to get the Rd database:
    library(packageName, character.only = TRUE)
    # Get the Rd database:
    db <- tools::Rd_db(packageName)
    # Define a temporary file to write the Rd to
    tmp <- tempfile()
    # Paste the Rd to one string and write to the temporary file:
    thisRd <- as.character(db[[paste0(functionName, ".Rd")]])
    thisRd <- paste(thisRd, collapse = "")
    write(thisRd, tmp)
    # Define the output PDF file_
    pdfFile <- path.expand(file.path(pdfDir , paste0(functionName, ".pdf")))
    # Build the PDF
    msg <- callr::rcmd(
        "Rd2pdf", 
        cmdargs = c(
            "--force", 
            "--no-index", 
            paste0("--title=", packageName, "::", functionName), 
            paste0("--output=", pdfFile), 
            tmp
        )
    )
    # Return the path to the PDF file:
    pdfFile
}

# system.time(writeFunctionPDF("BioStationWeighting.Rd", "RstoxTempdoc", "~/Code/Github/RstoxFramework"))


##########


