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


#processName
#processID
#modelName
#
#functionName
#functionOutputDataType
#activeFunctionInputs

#' 
#' @export
#' 
scanForModelError <- function(projectPath, modelName, processID = NULL) {
    
    # Get the table of process name and ID:
    processIndexTable <- readProcessIndexTable(projectPath, modelName)
    # Return an empty data.table if the processIndexTable is empty:
    if(nrow(processIndexTable) == 0) {
        return(data.table::data.table())
    }
    
    # Subset the table to the reuqested processID, if given:
    if(length(processID)) {
        atProcessID <- which(processID == processIndexTable$processID)
        if(length(atProcessID) == 0) {
            stop("The requested processID does not exist in the model ", modelName, " of projecct ", projectPath, ".")
        }
        processIndexTable <- processIndexTable[seq_len(atProcessID), ]
    }
    
    # Add the projectPath:
    processIndexTable[, projectPath := projectPath]
    
    # Add a column logging function input errors:
    processIndexTable[, functionInputError := FALSE]
    
    # Add function names:
    functionName <- mapply(
        getFunctionName, 
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processIndexTable$processID
    )
    processIndexTable[, functionName := ..functionName]
    
    # Add output data type:
    processIndexTable[, functionOutputDataType := lapply(functionName, getStoxFunctionMetaData, metaDataName = "functionOutputDataType")]
    
    # Get all active function inputs:
    functionInputs <- lapply(processIndexTable$processID, function(processID) getFunctionInputs(projectPath, modelName, processID, only.valid = TRUE))
    processIndexTable[, functionInputs := ..functionInputs]
    
    # Get all active function parameters (not needed in this function but included for consistency):
    functionParameters <- lapply(processIndexTable$processID, function(processID) getFunctionParameters(projectPath, modelName, processID, only.valid = TRUE))
    processIndexTable[, functionParameters := ..functionParameters]
    
    # Run through the processes and detect model errors:
    for(processIndex in seq_len(nrow(processIndexTable))) {
        if(length(processIndexTable$functionInputs[[processIndex]])) {
            functionInputError <- checkFunctionInputs(processIndexTable[seq_len(processIndex), ])
        }
        else {
            functionInputError <- FALSE
        }
        # Do any of the funciton inputs have error?
        processIndexTable$functionInputError[processIndex] <- any(functionInputError)
    }
        
    return(processIndexTable)
}


checkFunctionInput <- function(functionInput, functionInputDataType, processIndexTable) {
    # Expect an error, and return FALSE if all checks passes:
    functionInputError <- TRUE
    # (0) Chech that the function input is a string with positive number of characters:
    if(!is.character(functionInput)) {
        warning("Function input must be a character string (", functionInputDataType, ").")
    }
    # (1) Error if empty string:
    else if(nchar(functionInput) == 0) {
        warning("Function input must be a non-empty character string (", functionInputDataType, ").")
    }
    # (2) Error if not the name of a previous process:
    else if(! functionInput %in% processIndexTable$processName) {
        warning("Function input ", functionInput, " is not the name of a previous process (", functionInputDataType, ").")
    }
    else {
        atRequestedPriorProcess <- which(functionInput == processIndexTable$processName)
        outputDataTypeOfRequestedPriorProcess <- getStoxFunctionMetaData(processIndexTable$functionName[atRequestedPriorProcess], "functionOutputDataType")
        
        # (3) Error if the previous process returns the wrong data type:
        if(! functionInputDataType %in% outputDataTypeOfRequestedPriorProcess) {
            warning("Function input of process ", processIndexTable$processName[atRequestedPriorProcess], " does not return the correct data type (", functionInputDataType, ").")
        }
        else if(processIndexTable$functionInputError[atRequestedPriorProcess]) {
            warning("The process ", processIndexTable$processName[atRequestedPriorProcess], " has input error.")
        }
        else {
            functionInputError <- FALSE
        }
    }
    return(functionInputError)
}

checkFunctionInputs <- function(processIndexTable) {
    # Get the function input name and value paris:
    functionInput <- processIndexTable$functionInputs[[nrow(processIndexTable)]]
    functionInputDataType <- names(processIndexTable$functionInputs[[nrow(processIndexTable)]])
    functionInputError <- mapply(
        checkFunctionInput, 
        functionInput = functionInput, 
        functionInputDataType = functionInputDataType, 
        MoreArgs = list(processIndexTable = processIndexTable)
    )
    
    return(functionInputError)
}

##### Processes: #####
# Function to get whether the process has input data error:
getFunctionInputErrors <- function(ind, processTable, functionInputs) {

    #### Check wheter the processes from which process output is requested as funciton input exist prior to the current process: ####
    if(ind == 1) {
        return(FALSE)
    }
    
    # Get names of processes prior to the current process:
    prior <- seq_len(ind - 1)
    # Discard processes with enabled = FALSE:
    prior <- subset(prior, processTable$enabled[prior])
    
    # Select the prior processes:
    priorProcesses <- processTable$processName[prior]
    
    # Get the names of the processes from which funciton input is requested:
    requestedProcessNames <- unlist(functionInputs[[ind]])
    requestedFunctionInputDataTypes <- names(requestedProcessNames)
    
    # Are all of the function inputs present in the prior processes?:
    requestedProcessesExist <- all(requestedProcessNames %in% priorProcesses)
    correctDataTypeRequested <- FALSE
    
    if(!requestedProcessesExist) {
        warning(
            "The following requested processes do not exist, or are not enabled, prior to the process ", 
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


getCanShowInMap <- function(functionName, dataType = NULL) {
    # Get the data types returned by the functions of the processes:
    if(length(dataType) == 0) {
        dataType <- getStoxFunctionMetaData(functionName, metaDataName = "functionOutputDataType")
    }
    
    # Is the datatype of the dataTypesToShowInMap?:
    dataType %in% getRstoxFrameworkDefinitions("dataTypesToShowInMap")
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
#' 
#' @export
#' 
getInteractiveMode <- function(projectPath, modelName, processID) {
    
    # Get the data type of the process:
    dataType <- getDataType(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID
    )
    
    # Get also the process parameters to detect whether showInMap is FALSE, in which case interactiveMode should be "none":
    showInMap <- getProcessParameters(projectPath, modelName, processID)$showInMap
    
    # Select the type of interactive mode depending on the output data type from the process:
    if(dataType %in% getRstoxFrameworkDefinitions("stratumDataType") && showInMap) {
        "stratum"
    }
    else if(dataType %in% getRstoxFrameworkDefinitions("acousticPSUDataType")) {
        "acousticPSU"
    }
    else if(dataType %in% getRstoxFrameworkDefinitions("sweptAreaPSUDataType")) {
        "sweptAreaPSU"
    }
    else if(dataType %in% getRstoxFrameworkDefinitions("assignmentDataType")) {
        "assignment"
    }
    else if(dataType %in% getRstoxFrameworkDefinitions("stationDataType") && showInMap) {
        "station"
    }
    else if(dataType %in% getRstoxFrameworkDefinitions("EDSUDataType") && showInMap) {
        "EDSU"
    }
    else {
        "none"
    }
}

### # Function for getting the map mode of the process:
### getMapMode <- function(projectPath, modelName, processID) {
###     # Get the data type of the process:
###     dataType <- getDataType(
###         projectPath = projectPath, 
###         modelName = modelName, 
###         processID = processID
###     )
###     
###     # Select the type of interactive mode depending on the output data type from the process:
###     if(dataType %in% getRstoxFrameworkDefinitions("strataDataTypes")) {
###         "stratum"
###     }
###     else if(dataType %in% getRstoxFrameworkDefinitions("assignmentDataTypes")) {
###         "station"
###     }
###     else if(dataType %in% getRstoxFrameworkDefinitions("EDSUDataType")) {
###         "EDSU"
###     }
###     else {
###         stop("Invalid dataType")
###     }
### }


# Functions for getting the appropriate process data from the process, called depending on the interactive mode:
#' 
#' @export
#' 
getInteractiveData  <- function(projectPath, modelName, processID) {
    
    # Get the interactive mode:
    interactiveMode <- getInteractiveMode(projectPath, modelName, processID)
    
    # Call the appropriate function depending on the interactive mode:
    if(interactiveMode == "stratum") {
        getStratumList(
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
    else if(interactiveMode == "acousticPSU") {
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
#' 
#' @export
#' 
getMapData  <- function(projectPath, modelName, processID) {
    
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
    else if(interactiveMode == "station") {
        getStationData(
            projectPath = projectPath, 
            modelName = modelName, 
            processID = processID
        )
    }
    else if(interactiveMode == "EDSU") {
        getEDSUData(
            projectPath = projectPath, 
            modelName = modelName, 
            processID = processID
        )
    }
    else {
        warning("No map data available from the process ", processID, " of model ", modelName, " of project ", projectPath)
        geojsonio::geojson_json(getRstoxFrameworkDefinitions("emptyStratumPolygon"))
    }
}


# Individual get data functions:
#' 
getStratumData <- function(projectPath, modelName, processID) {
    
    # Get the process data:
    processData <- getProcessData(projectPath, modelName, processID)
    # Return an empty StratumPolygon if processData is empty:
    if(length(processData) == 0) {
        return(getRstoxFrameworkDefinitions("emptyStratumPolygon"))
    }
    
    # Issue an error of the process data are not of StratumPolygon type:
    if(names(processData) != "StratumPolygon"){
        processName <- getProcessName(projectPath, modelName, processID)
        warning("The process ", processName, " does not return process data of type StratumPolygon")
        return(NULL)
    }
    
    # Create the objects EDSU_PSU, PSU_Stratum and Stratum
    stratumPolygon <- geojsonio::geojson_json(processData$StratumPolygon)
    #stratum <- data.table::data.table(
    #    stratum = names(processData), 
    #    includeInTotal = 
    #)
    
    stratumPolygon
}

#' 
getAcousticPSUData <- function(projectPath, modelName, processID) {
    
    # Get the process data:
    processData <- getProcessData(projectPath, modelName, processID)
    # Issue an error of the process data are not of AcousticPSU type:
    #if(!all(names(processData) %in% c("Stratum_PSU", "EDSU_PSU"))){
    if(! "EDSU_PSU" %in% names(processData)){
        processName <- getProcessName(projectPath, modelName, processID)
        warning("The process ", processName, " does not return process data of type AcousticPSU")
        return(NULL)
    }
    
    ## Create the objects EDSU_PSU, PSU_Stratum and Stratum
    #EDSU_PSU <- processData$AcousticPSU[, c("EDSU", "PSU")]
    #PSU_Stratum <- unique(processData$AcousticPSU[, c("PSU", "Stratum")])
    #Stratum = unique(processData$AcousticPSU$Stratum)
    # Create the objects EDSU_PSU, PSU_Stratum and Stratum
    Stratum = data.table::data.table(
        Stratum = unique(processData$Stratum_PSU$Stratum)
    )
    
    # Return the list of data.tables:
    output <- c(
        processData, 
        list(Stratum = Stratum)
    )
    
    return(output)
}

#' 
getSweptAreaPSUData <- function(projectPath, modelName, processID) {
    
    # Get the process data:
    processData <- getProcessData(projectPath, modelName, processID)
    # Issue an error of the process data are not of SweptAreaPSU type:
    if(! "Station_PSU" %in% names(processData)){
        processName <- getProcessName(projectPath, modelName, processID)
        warning("The process ", processName, " does not return process data of type SweptAreaPSU")
        return(NULL)
    }
    
    ### # Create the objects EDSU_PSU, PSU_Stratum and Stratum
    ### Station_PSU <- processData$SweptAreaPSU[, c("Station", "PSU")]
    ### PSU_Stratum <- unique(processData$c[, c("PSU", "Stratum")])
    ### Stratum = unique(processData$SweptAreaPSU$Stratum)
    ### 
    ### # Return the list of data.tables:
    ### list(
    ###     Station_PSU = Station_PSU, 
    ###     PSU_Stratum = PSU_Stratum, 
    ###     Stratum = Stratum
    ### )
    return(processData)
}

#' 
getAssignmentData <- function(projectPath, modelName, processID) {
    
    # Get the process data:
    processData <- getProcessData(projectPath, modelName, processID)
    # Issue an error of the process data are not of Assignment type:
    if(names(processData) != "Assignment"){
        processName <- getProcessName(projectPath, modelName, processID)
        warning("The process ", processName, " does not return process data of type Assignment")
        return(NULL)
    }
    
    ### # Create the objects EDSU_PSU, PSU_Stratum and Stratum
    ### PSU_Layer_AssignmentID <- unique(processData$Assignment[, c("PSU", "Layer", "AssignmentID")])
    ### AssignmentID_Station_StationWeight <- unique(processData$Assignment[, c("AssignmentID", "Station", "StationWeight")])
    ### 
    ### # Return the list of data.tables:
    ### list(
    ###     PSU_Layer_AssignmentID = PSU_Layer_AssignmentID, 
    ###     AssignmentID_Station_StationWeight = AssignmentID_Station_StationWeight
    ### )
    processData$Assignment
}


#' 
getStratumList <- function(projectPath, modelName, processID) {
    
    # Get the process data:
    processData <- getProcessData(projectPath, modelName, processID)
    # Return an empty list if processData is empty:
    if(length(processData) == 0) {
        return(list())
    }
    
    # Issue an error of the process data are not of StratumPolygon type:
    if(names(processData) != "StratumPolygon"){
        processName <- getProcessName(projectPath, modelName, processID)
        warning("The process ", processName, " does not return process data of type StratumPolygon")
        return(list())
    }
    
    # Create the objects EDSU_PSU, PSU_Stratum and Stratum
    stratumList <- getStratumNames(processData$StratumPolygon)
    #stratum <- data.table::data.table(
    #    stratum = names(processData), 
    #    includeInTotal = 
    #)
    
    list(stratumList)
}

#' 
getStationData <- function(projectPath, modelName, processID) {
    # Get the station data:
    Cruise <- getProcessOutput(projectPath, modelName, processID, tableName = "Cruise")$Cruise
    Station <- getProcessOutput(projectPath, modelName, processID, tableName = "Station")$Station
    CruiseStation <- merge(Cruise, Station, by = intersect(names(Cruise), names(Station)))
    #Haul <- getProcessOutput(projectPath, modelName, processID, tableName = "Haul")$Haul
    #Station_Haul <- merge(Station, Haul, by = intersect(names(Station), names(Haul)))
    
    # Split the Station table into the coordinates and the properties:
    coordinateNames <- c("Longitude", "Latitude")
    coordinates <- CruiseStation[, ..coordinateNames]
    #rownames(coordinates) <- Station$Station
    #infoToKeep <- c("CruiseKey", "Platform", "StationKey", "Station", "CatchPlatform", "DateTime", "Longitude", "Latitude", "BottomDepth")
    infoToKeep <- c("Station", "Platform", "DateTime", "Longitude", "Latitude", "BottomDepth")
    properties <- CruiseStation[, ..infoToKeep]
    #properties <- Station[, !(colnames(Station) %in% c("Longitude", "Latitude")), with = FALSE]
    #rownames(properties) <- Station$Station
    
    # Add the haul info as a property, wrapped in a JSON string:
    #HaulInfo <- Station_Haul[, .(HaulInfo = jsonlite::toJSON(.SD)), .SDcols = names(Haul), by = Station]
    #properties$HaulInfo <- HaulInfo$HaulInfo
    
    # Create a spatial points data frame and convert to geojson:
    StationData <- sp::SpatialPointsDataFrame(coordinates, properties, match.ID = TRUE)
    StationData <- geojsonio::geojson_json(StationData)
    
    StationData
}

#' 
getEDSUData <- function(projectPath, modelName, processID) {
    
    # Get the Log data:
    Cruise <- getProcessOutput(projectPath, modelName, processID, tableName = "Cruise")$Cruise
    Log <- getProcessOutput(projectPath, modelName, processID, tableName = "Log")$Log
    CruiseLog <- merge(Cruise, Log, by = intersect(names(Cruise), names(Log)))
    
    # Extrapolate:  
    CruiseLog <- extrapolateEDSU(CruiseLog)
    
    # Define two feature collections, (1) one for the click points for the EDSUs with properties such as position, time, log etc., and (2) the line segments from start to stop point, with the property 'interpolated':
    
    # (1) Click points:
    # Extract the click points:
    coordinateNames <- c("Longitude", "Latitude")
    clickPointNames <- c("clickLongitude", "clickLatitude")
    clickPoints <- CruiseLog[, ..clickPointNames]
    data.table::setnames(clickPoints, old = clickPointNames, new = coordinateNames)
    
    # ...and define the properties:
    #infoToKeep <- c("CruiseKey", "Platform", "LogKey", "Log", "EDSU", "DateTime", "Longitude", "Latitude", "LogOrigin", "Longitude2", "Latitude2", "LogOrigin2", "LogDuration", "LogDistance", "EffectiveLogDistance", "BottomDepth")
    infoToKeep <- c("Platform", "EDSU", "Log", "DateTime", "Longitude", "Latitude", "EffectiveLogDistance", "BottomDepth")
    properties <- CruiseLog[, ..infoToKeep]
    
    # Create a spatial points data frame and convert to geojson:
    EDSUPoints <- sp::SpatialPointsDataFrame(clickPoints, properties, match.ID = FALSE)
    EDSUPoints <- geojsonio::geojson_json(EDSUPoints)
    
    # (2) Line segments:
    #lineStrings <- CruiseLog[, sp::Line(cbind(c(startLongitude, endLongitude), c(startLatitude, endLatitude))), by = EDSU]
    LineList <- apply(
        CruiseLog[, c("startLongitude", "endLongitude", "startLatitude", "endLatitude")], 
        1, 
        function(x) sp::Line(array(x, dim = c(2, 2)))
    )
    LinesList <- lapply(seq_along(LineList), function(ind) sp::Lines(LineList[[ind]], ID = CruiseLog$EDSU[ind]))
    EDSULines <- sp::SpatialLines(LinesList)
    EDSULines <- sp::SpatialLinesDataFrame(EDSULines, data = CruiseLog[, "interpolated"], match.ID = FALSE)
    EDSULines <- geojsonio::geojson_json(EDSULines)
    
    # List the points and lines and return:
    EDSUData <- list(
        EDSUPoints = EDSUPoints, 
        EDSULines = EDSULines
    )
    
    return(EDSUData)
}


#Log2SpatialLinesPolygon <- function(Log) {
#    
#    getLine <- function(Log) {
#        l <- cbind(
#            c(Log$startLongitude, Log$endLongitude), 
#            c(Log$startLatitude, Log$endLatitude)
#        )
#        L <- Line(l)
#        Lines(list(L), ID = Log$EDSU)
#        
#    }
#    segments <- Log[, getLine(.SD), .SDcols = names(Log)]
#    
#    
#        
#            
#    
#    ## from the sp vignette:
#    l1 <- cbind(c(1, 2, 3), c(3, 2, 2))
#    l2 <- cbind(c(1, 2, 3), c(1, 1.5, 1))
#    
#    Sl1 <- Line(l1)
#    Sl2 <- Line(l2)
#    
#    S1 <- Lines(list(Sl1), ID = "a")
#    S2 <- Lines(list(Sl2), ID = "b")
#    
#    Sl <- SpatialLines(list(S1, S2))
#    
#    
#}
 


extrapolateEDSU <- function(Log, pos = 0.5) {
    # Run the extrapolation function on each Paltform, effectively ordering the data by platform:
    Log <- Log[, extrapolateLongitudeLatitude(.SD), by = CruiseKey, .SDcols = names(Log)]
    
    # Get the click points of the EDSUs:
    Log <- getClickPoints(Log, pos = pos)
    
    Log
}


getClickPoints <- function(Log, pos = 0.5) {
    # Create the click points as a weighted average of the start and end points:
    Log$clickLongitude <- (Log$startLongitude * (1 - pos) + Log$endLongitude * pos)
    Log$clickLatitude <- (Log$startLatitude * (1 - pos) + Log$endLatitude * pos)
    Log
}

# Function to extract the start, middle and end positions from StoxBiotic:
getStartMiddleEndPosition <- function(Log, positionOrigins = c("start", "middle", "end"), coordinateNames = c("Longitude", "Latitude")) {
    
    # Get the number of positions of the Log:
    numPositions <- nrow(Log)
    
    # Define the position names:
    positionNames <- c(outer(positionOrigins, coordinateNames, paste0))
    
    # Create a table with missing positions:
    positionsNA <- data.table::as.data.table(
        array(dim = c(numPositions, length(positionNames)), dimnames = list(NULL, positionNames))
    )
    # Fill in the present data:
    if(!all(Log$LogOrigin[1] == Log$LogOrigin && Log$LogOrigin2[1] == Log$LogOrigin2)) {
        stop("LogOrigin or LogOrigin2 is not constant")
    }
    
    presentNames <- c(outer(Log[1, c(LogOrigin, LogOrigin2)], c("Longitude", "Latitude"), paste0))
    positionsNA[, presentNames] <- Log[, .(Longitude, Longitude2, Latitude, Latitude2)]
    
    # Add the missing positions to the Log:
    Log <- cbind(Log, positionsNA)
    
    Log
}

# Add stop position of the EDSUs for plotting in the map:
extrapolateLongitudeLatitude <- function(Log) {
    
    # Funciton to map values outside of a range to the maximum value:
    mapToRange <- function(x, length) {
        x[x > length] <- length
        x
    }
    
    # Get the number of positions of the Log:
    numPositions <- nrow(Log)
    
    # Extract the start, middle and end position:
    Log <- getStartMiddleEndPosition(Log)
    
    # Add the 'interpolated' tag:
    Log$interpolated <- FALSE
    
    # Detect missing values:
    naStart <- is.na(Log$startLongitude) | is.na(Log$startLatitude)
    naMiddle <- is.na(Log$middleLongitude) | is.na(Log$middleLatitude)
    naEnd <- is.na(Log$endLongitude) | is.na(Log$endLatitude)
    StartNotEnd <- which(!naStart & naEnd)
    onlyMiddle <- which(naStart & !naMiddle & naEnd)
    EndNotStart <- which(naStart & !naEnd)
    
    
    # Interpolate:
    if(length(StartNotEnd)) {
        Log[StartNotEnd, c("endLongitude", "endLatitude")] <- Log[mapToRange(StartNotEnd + 1, numPositions), c("startLongitude", "startLatitude")]
        Log$interpolated[StartNotEnd] <- TRUE
    }
    if(length(onlyMiddle)) {
        Log[onlyMiddle, c("startLongitude", "startLatitude")] <- (
            Log[mapToRange(onlyMiddle - 1, numPositions), c("middleLongitude", "middleLatitude")] +
            Log[onlyMiddle, c("middleLongitude", "middleLatitude")]
        ) / 2
        Log[onlyMiddle, c("endLongitude", "endLatitude")] <- (
            Log[onlyMiddle, c("middleLongitude", "middleLatitude")] + 
            Log[mapToRange(onlyMiddle + 1, numPositions), c("middleLongitude", "middleLatitude")]
        ) / 2
        Log$interpolated[v] <- TRUE
    }
    if(length(EndNotStart)) {
        Log[EndNotStart, c("startLongitude", "startLatitude")] <- Log[mapToRange(EndNotStart - 1, numPositions), c("endLongitude", "endLatitude")]
        Log$interpolated[EndNotStart] <- TRUE
    }

    Log
}





##########



##### Process properties: #####
#' 
#' @export
#' 
getProcessPropertyNames <- function() {
    
    # Parameter data types, default (null) value, test:
    # logical: file(x), inherits(x, "connection")
    # character: "", is.character()
    # integer
    # double
    # filePath
    # directoryPath
    # 
    
    # We separate the primitive data types of parameters and the parameterformat which is a GUI specific tag indicating which action the GUI should take to aid the user to create the parameter value, such as selecting a file or building a table.
    
    
    # parameterformat: 
    #    filter, 
    #    fileSelector, directorySelector
    #    splitNASC, length2tsAcousticCategory, 
    #    catchability
    #    length2tsLayerPSU, 
    

    
}



parameter2JSONString <- function(parameter) {
    # If already a character, return immediately:
    if(is.character(parameter)) {
        return(parameter)
    }
    else {
        return(as.character(jsonlite::toJSON(parameter, auto_unbox = TRUE)))
    }
}


isMultipleParameter <- function(functionName, parameterName) {
    multiple <- unlist(getRstoxFrameworkDefinitions("processPropertyFormats")$multiple)
    format <- unlist(getFunctionParameterPropertyFormats(functionName)[parameterName])
    isMultiple <- format %in% multiple
    return(isMultiple)
}


#' 
#' @export
#' 
getProcessPropertySheet <- function(projectPath, modelName, processID, outfile = NULL) {
    
    # The project properties contains the following elements:
    # 1. name
    # 2. displayName
    # 3. description
    # 4. type
    # 5. format
    # 6. defaultValue
    # 7. possibleValues
    # 8. value
    
    # Possible values of 'type':
    # "integer"
    # "double"
    # "logical"
    # "character"
    
    # Possible values of 'format':
    # "none"
    # "filterExpressionTable"
    # "filePath"
    # "filePaths"
    # "directoryPath"
    # "catchabilityTable"
    # "NASCTable"
    # "length2TSTable" 
    # "speciesCategoryTable"
    # "acousticCategoryTable" 
    
    
    # Function that gets the process names of the processes returning the specified data type
    getProcessNamesByDataType <- function(dataType, processTable) {
        hasRequestedDataType <- processTable$dataType == dataType
        if(any(hasRequestedDataType)) {
            processTable$processName[hasRequestedDataType]
        }
        else {
            NULL
        }
    }
    
    # Function to replace an empty object by double(0) or character(1), which results in [] in JSON (since OpenCPU uses auto-unbox = TRUE):
    replaceEmpty <- function(x, vector = TRUE) {
        areEmpty <- lengths(x) == 0
        if(any(areEmpty)) {
            if(vector) {
                x[areEmpty] <- rep(list(double(0)), sum(areEmpty))
            }
            else {
                x[areEmpty] <- rep(list(character(1)), sum(areEmpty))
            }
        }
        x   
    }
    
    #######################
    ##### 1. Process: #####
    #######################
    
    functionName <- getFunctionName(projectPath = projectPath, modelName = modelName, processID = processID)
    processName <- getProcessName(projectPath = projectPath, modelName = modelName, processID = processID)
    processParameters <- getProcessParameters(projectPath = projectPath, modelName = modelName, processID = processID)
    functionInputs <- getFunctionInputs(projectPath = projectPath, modelName = modelName, processID = processID)
    functionParameters <- getFunctionParameters(projectPath = projectPath, modelName = modelName, processID = processID)
    
    # Get the process properties depending on the processPropertyName:
    processParameters <- getRstoxFrameworkDefinitions("processParameters")
    processParametersDisplayNames <- getRstoxFrameworkDefinitions("processParametersDisplayNames")
    processParametersDescriptions <- getRstoxFrameworkDefinitions("processParametersDescriptions")
    processParameterNames <- names(processParameters)
    
    ##### Define the process name, the function name and the process parameters as the process property "process": #####
    processArguments <- data.table::data.table(
        # 1. name:
        name = as.list(c(
            "processName", 
            "functionName", 
            processParameterNames
        )), 
        # 2. displayName:
        displayName = as.list(c(
            "Process name", 
            "Function", 
            unname(unlist(processParametersDisplayNames))
        )), 
        # 3. description:
        description = as.list(c(
            "The name of the process, which must be unique within each model", 
            "The name of the function called by the process", 
            processParametersDescriptions
        )), 
        # 4. type:
        type = as.list(c(
            "character", 
            "character", 
            sapply(processParameters, class)
        )), 
        # 5. format:
        format = as.list(rep("none", 2 + length(processParameters))), 
        # 6. default:
        default = c(
            list(
                character(1), 
                character(1)
            ), 
            unname(processParameters)
        ), 
        # 7. possibleValues:
        possibleValues = c(
            list(character(0)), 
            # Set this as list to ensure that we keep the square brackets "[]" in the JSON string even with auto_unbox = TRUE.
            as.list(getAvailableStoxFunctionNames(modelName)), 
            rep(list(c(FALSE, TRUE)), length(processParameters))
        ), 
        # 8. value:
        value = c(
            processName, 
            # Remove the package address and only use the function name:
            getFunctionNameFromPackageFunctionName(functionName), 
            processParameters
        )
    )
    
    # Remove the showInMap argument if not relevant:
    if(!getCanShowInMap(functionName)) {
        keep <- c(
            TRUE, 
            TRUE, 
            processParameterNames != "showInMap"
        )
        processArguments <- processArguments[keep, ]
    }
    #######################
    
    ## Declare functionInputs and functionParameters and 
    #functionInputs <- data.table::data.table()
    #functionParameters <- data.table::data.table()
    
    if(length(functionName)) {
        
        ##############################
        ##### 2. FunctionInputs: #####
        ##############################
        # Run only if there are function inputs:
        if(length(functionInputs)) {
            # Get the process table, which is needed to get the output data types from the prior processes for use in the function inputs:
            processTable <- getProcessTable(projectPath, modelName)
            thisProcessIndex <- which(processTable$processID == processID)
            processTable <- processTable[seq_len(thisProcessIndex), ]
            functionInputNames <- names(functionInputs)
            
            # Define the function inputs:
            functionInputs <- data.table::data.table(
                # 1. name:
                name = as.list(functionInputNames), 
                # 2. displayName:
                displayName = as.list(functionInputNames), 
                # 3. description:
                description = replaceEmpty(getStoxFunctionMetaData(functionName, "functionArgumentDescription")[functionInputNames]), 
                # 4. type:
                type = as.list(rep("character", length(functionInputNames))),
                # 5. format:
                format = as.list(rep("none", length(functionInputNames))),
                # 6. default:
                default = rep(list(character(1)), length(functionInputNames)), 
                # 7. possibleValues:
                #possibleValues = lapply(functionInputNames, getProcessNamesByDataType, processTable = processTable),
                # Set each element (using as.list()) as list to ensure that we keep the square brackets "[]" in the JSON string even with auto_unbox = TRUE.
                possibleValues = lapply(lapply(functionInputNames, getProcessNamesByDataType, processTable = processTable), as.list),
                # 8. value:
                value = replaceEmpty(functionInputs, vector = FALSE)
            )
        }
        ##############################
        
        
        ##################################
        ##### 3. FunctionParameters: #####
        ##################################
        
        # Run only if there are function parameters (which there always will be):
        if(length(functionParameters)) {
            # Get the names of the function parameters:
            functionParameterNames <- names(functionParameters)
            
            # Define the function parameters:
            functionParameters <- data.table::data.table(
                # 1. name:
                name = as.list(functionParameterNames), 
                # 2. displayName:
                displayName = as.list(functionParameterNames), 
                # 3. description:
                description = replaceEmpty(getStoxFunctionMetaData(functionName, "functionArgumentDescription")[functionParameterNames]), 
                # 4. type:
                type = replaceEmpty(getStoxFunctionParameterPropertyTypes(functionName)[functionParameterNames]),
                # 5. format:
                format = replaceEmpty(getFunctionParameterPropertyFormats(functionName)[functionParameterNames]),
                # 6. default:
                default = replaceEmpty(getStoxFunctionParameterDefaults(functionName)[functionParameterNames], vector = FALSE),
                # 7. possibleValues:
                # Set this as list to ensure that we keep the square brackets "[]" in the JSON string even with auto_unbox = TRUE.
                possibleValues = lapply(replaceEmpty(getStoxFunctionParameterPossibleValues(functionName)[functionParameterNames]), as.list),
                # 8. value:
                value = replaceEmpty(functionParameters, vector = FALSE)
            )
            
            # Convert to a JSON string ifs of non-simple type (length >= 1):
            nonSimple <- isMultipleParameter(functionName, unlist(functionParameters$name))
            if(any(nonSimple)) {
                functionParameters$value[nonSimple] = parameter2JSONString(functionParameters$value[nonSimple])
            }
        }
        
        # Apply the StoX funciton argument hierarcy here using getStoxFunctionMetaData("functionArgumentHierarchy"):
        argumentsToShow <- getArgumentsToShow(
            projectPath = projectPath, 
            modelName = modelName, 
            processID = processID
        )
        
        # Select only the items to show in the GUI:
        if(length(functionParameters) && any(!functionParameters$name %in% argumentsToShow)) {
            functionParameters <- subset(functionParameters, name %in% argumentsToShow)
        }
        if(length(functionInputs) && any(!functionInputs$name %in% argumentsToShow)) {
            functionInputs <- subset(functionInputs, name %in% argumentsToShow)
        }
        
        
        ##############################
    }
    
    # Create a list of the different properties, adding category and displayName:
    propertySheet <- list(
        list(
            groupName = "processArguments", 
            displayName = "Process", 
            properties = processArguments
        ), 
        list(
            groupName = "functionInputs", 
            displayName = "Function inputs", 
            properties = functionInputs
        ), 
        list(
            groupName = "functionParameters", 
            displayName = "Function parameters", 
            properties = functionParameters
        )
    )
    
    output <- list(
        propertySheet = propertySheet, 
        #help = getFunctionHelpAsHtml(
        #    projectPath = projectPath, 
        #    modelName = modelName, 
        #    processID = processID, 
        #    outfile = outfile
        #), 
        activeProcessID = getActiveProcessID(
            projectPath = projectPath, 
            modelName = modelName
        )
    )
    
    # Return the list of process property groups (process property sheet):
    output
}

#' 
#' @export
#' 
setProcessPropertyValue <- function(groupName, name, value, projectPath, modelName, processID) {
    
    # Parse the value (this takes care of converting true to TRUE, interpret integers and strings, and even to parse JSON strings to R objects):
    value <- parseParameter(value)
    
    # If the process property 'processArguments' is given, modify the process name, function name or process parameters:
    if(groupName == "processArguments") {
        # Modify process name:
        if(name == "processName") {
            modifyProcessName(
                projectPath = projectPath, 
                modelName = modelName, 
                processID = processID, 
                newProcessName = value
            )
        }
        # Modify function name:
        else if(name == "functionName") {
            # Get the full address to the function using getPackageFunctionName():
            modifyFunctionName(
                projectPath = projectPath, 
                modelName = modelName, 
                processID = processID, 
                newFunctionName = getPackageFunctionName(value)
            )
        }
        # Modify process parameters:
        else {
            modifyProcessParameters(
                projectPath = projectPath, 
                modelName = modelName, 
                processID = processID, 
                newProcessParameters = setNames(list(value), name)
            )
        }
    }
    # If the process property 'functionInputs' is given, modify the function inputs:
    if(groupName == "functionInputs") {
        modifyFunctionInputs(
            projectPath = projectPath, 
            modelName = modelName, 
            processID = processID, 
            newFunctionInputs = setNames(list(value), name)
        )
    }
    # If the process property 'functionInputs' is given, modify the function parameters:
    if(groupName == "functionParameters") {
        modifyFunctionParameters(
            projectPath = projectPath, 
            modelName = modelName, 
            processID = processID, 
            newFunctionParameters = setNames(list(value), name)
        )
    }
    
    # Reset the active process ID to the process before the modified process:
    activeProcessID = resetModel(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID
    )
    
    # Return the modified process properties:
    getProcessPropertySheet(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID
    )
    
    # Return the flags for changed process data and process property
}



##########


getPathToSingleFunctionPDF <- function(functionName) {
    # Extract the package name:
    packageName <- getPackageNameFromPackageFunctionName(functionName)
    # Build the path to the function PDF:
    pathToSingleFunctionPDF <- file.path(
        system.file("extdata", "singleFunctionPDFs", package = packageName), 
        paste(functionName, "pdf", sep = ".")
    )
    pathToSingleFunctionPDF
}



#' 
#' @export
#' 
getFunctionHelpAsHtml <- function(projectPath, modelName, processID, outfile = NULL, stylesheet = "") {
    # Extract the packageName::functionName:
    packageName_functionName <- getFunctionName(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID
    )
    # Get the package and function name:
    packageName <- getPackageNameFromPackageFunctionName(packageName_functionName)
    functionName <- getFunctionNameFromPackageFunctionName(packageName_functionName)
    # Get the help:
    html <- getObjectHelpAsHtml(packageName = packageName, objectName = functionName, outfile = outfile, stylesheet = stylesheet)
    html
}


#' 
#' @export
#' 
getObjectHelpAsHtml <- function(packageName, objectName, outfile = NULL, stylesheet = "") {
    
    # Read the documentation database:
    db <- tools::Rd_db(packageName)
    # Write the help to file as html and read back:
    objectName.Rd <- paste0(objectName, ".Rd")
    
    # Get the links of the package:
    Links <- tools::findHTMLlinks(pkgDir = find.package(packageName))
    # Return empty string if the function 
    if(! objectName %in% names(Links)) {
        return("")
    }
    
    # Write to a temporary file
    if(length(outfile) == 0) {
        outfile <- tempfile(fileext = ".html")
    }
    tools::Rd2HTML(db[[objectName.Rd]], out = outfile, Links = Links, stylesheet = stylesheet)
    html <- paste(readLines(outfile), collapse="\n")
    unlink(outfile, force = TRUE)
    html
}


#' 
#' @export
#' 
getFilterOptions <- function(projectPath, modelName, processID, tableName) {
    
    # Get the process output:
    processOutput <- getProcessOutput(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID, 
        tableName = tableName, 
        drop = TRUE
    )
    
    # Convert to a list of tables:
    #processOutput <- unlistToDataType(processOutput)
    
    ## Get a vector of table names:
    #tableNames <- names(processOutput)
    ## Select the requested table:
    #if(! tableName %in% tableNames) {
    #    stop("Invalid table. Choose one of the following: ", paste(tableNames, collapse = ", "))
    #}
    
    # Get the column names:
    #name <- names(processOutput[[tableName]])
    name <- names(processOutput)
    
    # Get the data types:
    #type <- sapply(processOutput[[tableName]], firstClass)
    type <- sapply(processOutput, firstClass)
    
    # Get the operators:
    operators <- getRstoxFrameworkDefinitions("filterOperators")[type]
    
    # Get a list of unique values for each column of each table:
    #options <- getPossibleValuesOneTable(processOutput[[tableName]])
    options <- getPossibleValuesOneTable(processOutput)
    options <- lapply(options, getOptionList)
    
     
    output <- mapply(
        list, 
        name = name, 
        type = type, 
        operators = operators, 
        options = options, 
        SIMPLIFY = FALSE
    )
    # Add the fields level:
    output <- list(
        fields = output
    )
    
    
    # Return a list of the tableNames, columnNames and possibleValues:
    return(output)
}

getOptionList <- function(option, digits = 6) {
    option <- data.table::data.table(
        name = format(option, digits = digits), 
        value = option
    )
    output <- unname(split(option, seq_len(nrow(option))))
    output <- lapply(output, as.list)
    return(output)
}


getPossibleValuesOneTable <- function(table) {
    # Unique and then sort each column:
    sortUnique <- function(y) {
        sort(unique(y))
    }
    lapply(table, sortUnique)
}


#' 
#' @export
#' 
getParameterTableTitle <- function(format) {
    getRstoxFrameworkDefinitions("parameterTableTitle")[[format]]
}

#' 
#' @export
#' 
getParameterTableColumnNames <- function(format) {
    getRstoxFrameworkDefinitions("parameterTableColumnNames")[[format]]
}
    
#' 
#' @export
#' 
getParameterTablePossibleValues <- function(projectPath, modelName, processID, format) {
    columnNames <- getParameterTableColumnNames(format)
    vector("list", length(columnNames))
}

#' 
#' @export
#' 
getParameterTableInfo <- function(projectPath, modelName, processID, format) {
    list(
        parameterTableTitle = getParameterTableTitle(format), 
        parameterTableColumnNames = getParameterTableColumnNames(format), 
        arameterTablePossibleValues = getParameterTablePossibleValues(
            projectPath = projectPath, 
            modelName = modelName, 
            processID = processID, 
            format = format
        )
    )
}



