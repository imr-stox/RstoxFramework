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
getCanShowInMap <- function(functionName, dataType = NULL) {
    # Get the data types returned by the functions of the processes:
    if(length(dataType) == 0) {
        dataType <- getStoxFunctionMetaData(functionName, metaDataName = "functionOutputDataType")
    }
    
    # Is the datatype of the dataTypesToShowInMap?:
    if(length(dataType) == 0 || nchar(dataType) == 0) {
        return(FALSE)
    }
    else {
        return(dataType %in% getRstoxFrameworkDefinitions("dataTypesToShowInMap"))
    }
}

##########


##### Interactive: #####

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
    else if(dataType %in% getRstoxFrameworkDefinitions("bioticPSUDataType")) {
        "bioticPSU"
    }
    else if(dataType %in% getRstoxFrameworkDefinitions("acousticLayerDataType")) {
        "acousticLayer"
    }
    else if(dataType %in% getRstoxFrameworkDefinitions("bioticLayerDataType")) {
        "bioticLayer"
    }
    else if(dataType %in% getRstoxFrameworkDefinitions("bioticAssignmentDataType")) {
        "bioticAssignment"
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
    else if(interactiveMode == "bioticAssignment") {
        getBioticAssignmentData(
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
    else if(interactiveMode == "bioticPSU") {
        getBioticPSUData(
            projectPath = projectPath, 
            modelName = modelName, 
            processID = processID
        )
    }
    else if(interactiveMode == "acousticLayer") {
        getAcousticLayerData(
            projectPath = projectPath, 
            modelName = modelName, 
            processID = processID
        )
    }
    else if(interactiveMode == "bioticLayer") {
        getBioticLayerData(
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
        warning("StoX: No map data available from the process ", processID, " of model ", modelName, " of project ", projectPath)
        geojsonio::geojson_json(getRstoxFrameworkDefinitions("emptyStratumPolygon"))
    }
}


# Individual get data functions:
getStratumData <- function(projectPath, modelName, processID) {
    
    # Get the process data:
    processData <- getProcessData(projectPath, modelName, processID)
    # Return an empty StratumPolygon if processData is empty:
    # Change this to an error?????????????????
    if(length(processData) == 0) {
        return(getRstoxFrameworkDefinitions("emptyStratumPolygon"))
    }
    
    # Issue an error of the process data are not of StratumPolygon type:
    if(names(processData) != "StratumPolygon"){
        processName <- getProcessName(projectPath, modelName, processID)
        warning("StoX: The process ", processName, " does not return process data of type StratumPolygon")
        return(NULL)
    }
    
    # Create the objects EDSU_PSU, PSU_Stratum and Stratum
    stratumPolygon <- geojsonio::geojson_json(processData$StratumPolygon)
    #stratum <- data.table::data.table(
    #    stratum = names(processData), 
    #    includeInTotal = 
    #)
    
    return(
        list(
            stratumPolygon = stratumPolygon
        )
    )
    #stratumPolygon
}

# Function to get acousic PSU data:
getAcousticPSUData <- function(projectPath, modelName, processID) {
    
    # Get the process data:
    processData <- getProcessData(projectPath, modelName, processID)
    # Issue an error of the process data are not of AcousticPSU type:
    if(! "EDSU_PSU" %in% names(processData)){
        processName <- getProcessName(projectPath, modelName, processID)
        warning("StoX: The process ", processName, " does not return process data of type AcousticPSU")
        return(NULL)
    }
    
    return(processData)
}

# Function to get swept-area PSU data:
getBioticPSUData <- function(projectPath, modelName, processID) {
    
    # Get the process data:
    processData <- getProcessData(projectPath, modelName, processID)
    # Issue an error of the process data are not of BioticPSU type:
    if(! "Station_PSU" %in% names(processData)){
        processName <- getProcessName(projectPath, modelName, processID)
        warning("StoX: The process ", processName, " does not return process data of type BioticPSU")
        return(NULL)
    }
    
    return(processData)
}

# Function to get acousic PSU data:
getAcousticLayerData <- function(projectPath, modelName, processID) {
    
    # Get the process data:
    processData <- getProcessData(projectPath, modelName, processID)
    # Issue an error of the process data are not of AcousticPSU type:
    if(! "AcousticLayer" %in% names(processData)){
        processName <- getProcessName(projectPath, modelName, processID)
        warning("StoX: The process ", processName, " does not return process data of type AcousticLayer")
        return(NULL)
    }
    
    return(processData)
}

# Function to get swept-area PSU data:
getBioticLayerData <- function(projectPath, modelName, processID) {
    
    # Get the process data:
    processData <- getProcessData(projectPath, modelName, processID)
    # Issue an error of the process data are not of BioticPSU type:
    if(! "BioticLayer" %in% names(processData)){
        processName <- getProcessName(projectPath, modelName, processID)
        warning("StoX: The process ", processName, " does not return process data of type BioticLayer")
        return(NULL)
    }
    
    return(processData)
}

# Function to get biotic assignment data:
getBioticAssignmentData <- function(projectPath, modelName, processID) {
    
    # Get the process data:
    processData <- getProcessData(projectPath, modelName, processID)
    # Issue an error of the process data are not of BioticAssignment type:
    if(names(processData) != "BioticAssignment"){
        processName <- getProcessName(projectPath, modelName, processID)
        warning("StoX: The process ", processName, " does not return process data of type BioticAssignment")
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
    return(processData)
}


# Function to get a list of strata names:
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
        warning("StoX: The process ", processName, " does not return process data of type StratumPolygon")
        return(list())
    }
    
    # Create the objects EDSU_PSU, PSU_Stratum and Stratum
    stratumList <- as.list(RstoxBase::getStratumNames(processData$StratumPolygon))
    #stratum <- data.table::data.table(
    #    stratum = names(processData), 
    #    includeInTotal = 
    #)
    
    #list(stratumList)
    return(stratumList)
}

# Function to get a list of station data:
getStationData <- function(projectPath, modelName, processID) {
    # Get the station data:
    Cruise <- getProcessOutput(projectPath, modelName, processID, tableName = "Cruise")$Cruise
    Station <- getProcessOutput(projectPath, modelName, processID, tableName = "Station")$Station
    CruiseStation <- merge(Cruise, Station, by = intersect(names(Cruise), names(Station)))
    
    Haul <- getProcessOutput(projectPath, modelName, processID, tableName = "Haul")$Haul
    Station_Haul <- merge(Station, Haul, by = intersect(names(Station), names(Haul)))
    
    # Split the Station table into the coordinates and the properties:
    coordinateNames <- c("Longitude", "Latitude")
    coordinates <- CruiseStation[, ..coordinateNames]
    
    #rownames(coordinates) <- Station$Station
    #infoToKeep <- c("CruiseKey", "Platform", "StationKey", "Station", "CatchPlatform", "DateTime", "Longitude", "Latitude", "BottomDepth")
    stationInfoToKeep <- c("Station", "Platform", "DateTime", "Longitude", "Latitude", "BottomDepth")
    stationInfo <- CruiseStation[, ..stationInfoToKeep]
    properties <- stationInfo[, "Station"]
    
    haulInfoToKeep <- c("Station", "Haul", "Gear", "EffectiveTowedDistance", "MinHaulDepth", "MaxHaulDepth")
    haulInfo <- Station_Haul[, ..haulInfoToKeep]
    
    
    #properties <- Station[, !(colnames(Station) %in% c("Longitude", "Latitude")), with = FALSE]
    #rownames(properties) <- Station$Station
    
    # Add the haul info as a property, wrapped in a JSON string:
    #HaulInfo <- Station_Haul[, .(HaulInfo = jsonlite::toJSON(.SD)), .SDcols = names(Haul), by = Station]
    #properties$HaulInfo <- HaulInfo$HaulInfo
    
    # Create a spatial points data frame and convert to geojson:
    stationPoints <- sp::SpatialPointsDataFrame(coordinates, properties, match.ID = TRUE)
    stationPoints <- geojsonio::geojson_json(stationPoints)
    
    return(
        list(
            stationPoints = stationPoints, 
            stationInfo = stationInfo, 
            haulInfo = haulInfo
        )
    )
}

# Function to get EDSU data:
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
    EDSUInfoToKeep <- c("EDSU", "Platform", "Log", "DateTime", "Longitude", "Latitude", "EffectiveLogDistance", "BottomDepth")
    EDSUInfo <- CruiseLog[, ..EDSUInfoToKeep]
    properties <- EDSUInfo[, "EDSU"]
    
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
    
    ## List the points and lines and return:
    #EDSUData <- list(
    #    EDSUPoints = EDSUPoints, 
    #    EDSULines = EDSULines
    #)
    
    return(
        list(
            EDSUPoints = EDSUPoints, 
            EDSULines = EDSULines, 
            EDSUInfo = EDSUInfo
        )
    )
    #return(EDSUData)
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
        Log$interpolated[onlyMiddle] <- TRUE
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


formatJSONString <- function(parameter) {
    as.character(jsonlite::toJSON(parameter, auto_unbox = TRUE))
}


#isMultipleParameter <- function(functionName, parameterName) {
#    multiple <- unlist(getRstoxFrameworkDefinitions("processPropertyFormats")$multiple)
#    format <- unlist(getFunctionParameterFormats(functionName)[parameterName])
#    isMultiple <- format %in% multiple
#    return(isMultiple)
#}
isVectorParameter <- function(format) {
    # Find those formats that are vector:
    processPropertyFormats <- getRstoxFrameworkDefinitions("processPropertyFormats")
    isVector <- sapply(format, isVectorParameterOne, processPropertyFormats)
    return(isVector)
}

isVectorParameterOne <- function(format, processPropertyFormats) {
    # 2020-06-24: Changed from == to identical (Jira, STOX-225):
    #identical(processPropertyFormats[[format]]$type, "vector")
    identical(processPropertyFormats[[format]]$class, "vector")
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
    # 6. possibleValues
    # 7. value
    
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
    
    #######################
    ##### 1. Process: #####
    #######################
    
    # Get the process properties to return, which are all but the proccessData:
    functionName <- getFunctionName(projectPath = projectPath, modelName = modelName, processID = processID)
    processName <- getProcessName(projectPath = projectPath, modelName = modelName, processID = processID)
    processParameters <- getProcessParameters(projectPath = projectPath, modelName = modelName, processID = processID)
    functionInputs <- getFunctionInputs(projectPath = projectPath, modelName = modelName, processID = processID)
    functionParameters <- getFunctionParameters(projectPath = projectPath, modelName = modelName, processID = processID)
    
    # Get the process properties depending on the processPropertyName:
    #processParameters <- getRstoxFrameworkDefinitions("processParameters")
    processParametersDisplayNames <- getRstoxFrameworkDefinitions("processParametersDisplayNames")
    processParametersDescriptions <- getRstoxFrameworkDefinitions("processParametersDescriptions")
    processParameterNames <- names(processParameters)
    
    
    ##### Define the process name, the function name and the process parameters as the process property "process": #####
    processArgumentsToReturn <- data.table::data.table(
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
            sapply(processParameters, firstClass)
        )), 
        # 5. format:
        # The number 2 is functionName and processName:
        format = as.list(rep("none", 2 + length(processParameters))), 
        # 6. possibleValues:
        possibleValues = c(
            list(NULL), 
            # Set this as list to ensure that we keep the square brackets "[]" in the JSON string even with auto_unbox = TRUE.
            as.list(getAvailableStoxFunctionNames(modelName)), 
            # Removed the possible values for logicals, since these are not used as dropdown in the GUI, but rather as a checkbox:
            rep(list(c(FALSE, TRUE)), length(processParameters))
            #rep(list(character(1)), length(processParameters))
        ), 
        # 7. value:
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
        processArgumentsToReturn <- processArgumentsToReturn[keep, ]
    }
    
    # Convert all possibleValues and value to character:
    toJSONString(processArgumentsToReturn)
    #######################
    
    
    # Declare functionInputs and functionParameters and 
    functionInputsToReturn <- data.table::data.table()
    functionParametersToReturn <- data.table::data.table()
    
    if(length(functionName)) {
        
        ##############################
        ##### 2. FunctionInputs: #####
        ##############################
        # Run only if there are function inputs:
        if(length(functionInputs)) {
            # Get the process table, which is needed to get the output data types from the prior processes for use in the function inputs:
            # scanForModelError is enough...
            #processTable <- getProcessTable(projectPath = projectPath, modelName = modelName, beforeProcessID = processID)
            #processTable <- scanForModelError(projectPath = projectPath, modelName = modelName, beforeProcessID = processID)
            processTable <- scanForModelError(projectPath = projectPath, modelName = NULL, beforeProcessID = processID)
            
            #thisProcessIndex <- which(processTable$processID == processID)
            #processTable <- processTable[seq_len(thisProcessIndex), ]
            functionInputNames <- names(functionInputs)
            
            # Define the function inputs:
            functionInputsToReturn <- data.table::data.table(
                # 1. name:
                name = as.list(functionInputNames), 
                # 2. displayName:
                displayName = as.list(functionInputNames), 
                # 3. description:
                description = getStoxFunctionMetaData(functionName, "functionArgumentDescription")[functionInputNames], 
                # 4. type:
                type = as.list(rep("character", length(functionInputNames))),
                # 5. format:
                format = as.list(rep("none", length(functionInputNames))),
                # 6. possibleValues:
                #possibleValues = lapply(functionInputNames, getProcessNamesByDataType, processTable = processTable),
                # Set each element (using as.list()) as list to ensure that we keep the square brackets "[]" in the JSON string even with auto_unbox = TRUE.
                #possibleValues = lapply(lapply(functionInputNames, getProcessNamesByDataType, processTable = processTable), as.list),
                possibleValues = lapply(functionInputNames, getProcessNamesByDataType, processTable = processTable),
                # 7. value:
                value = functionInputs
            )
            
            # Convert all possibleValues and value to character:
            toJSONString(functionInputsToReturn)
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
            functionParametersToReturn <- data.table::data.table(
                # 1. name:
                name = as.list(functionParameterNames), 
                # 2. displayName:
                displayName = as.list(functionParameterNames), 
                # 3. description:
                description = getStoxFunctionMetaData(functionName, "functionArgumentDescription")[functionParameterNames], 
                # 4. type:
                type = getStoxFunctionParameterTypes(functionName)[functionParameterNames],
                # 5. format:
                format = getFunctionParameterFormats(functionName)[functionParameterNames],
                # 6. possibleValues:
                # Set this as list to ensure that we keep the square brackets "[]" in the JSON string even with auto_unbox = TRUE.
                possibleValues = getStoxFunctionParameterPossibleValues(functionName)[functionParameterNames],
                # 7. value:
                value = functionParameters
            )
            
            # Convert to a JSON string if the parameter has a format:
            hasFormat <- functionParametersToReturn$format != "none"
            #if(any(hasFormat)) {
            #    functionParametersToReturn$value[hasFormat] = lapply(functionParametersToReturn$value[hasFormat], formatJSONString)
            #}
            
            # Convert all possibleValues and value to character:
            toJSONString(functionParametersToReturn)
        }
        
        # Apply the StoX funciton argument hierarcy here using getStoxFunctionMetaData("functionArgumentHierarchy"):
        argumentsToShow <- getArgumentsToShow(
            projectPath = projectPath, 
            modelName = modelName, 
            processID = processID
        )
        
        # Select only the items to show in the GUI:
        if(length(functionParametersToReturn) && any(!functionParametersToReturn$name %in% argumentsToShow)) {
            functionParametersToReturn <- subset(functionParametersToReturn, name %in% argumentsToShow)
        }
        if(length(functionInputsToReturn) && any(!functionInputsToReturn$name %in% argumentsToShow)) {
            functionInputsToReturn <- subset(functionInputsToReturn, name %in% argumentsToShow)
        }
    }
    
    # Create a list of the different properties, adding category and displayName:
    propertySheet <- list(
        list(
            groupName = "processArguments", 
            displayName = "Process", 
            properties = processArgumentsToReturn
        ), 
        list(
            groupName = "functionInputs", 
            displayName = "Function inputs", 
            properties = functionInputsToReturn
        ), 
        list(
            groupName = "functionParameters", 
            displayName = "Function parameters", 
            properties = functionParametersToReturn
        )
    )
    
    # Set the propertyDirty flag to FALSE, so that a GUI can update the properties:
    writeActiveProcessID(projectPath, modelName, propertyDirty = FALSE)
    
    # Return the list of process property groups (process property sheet):
    output <- list(
        propertySheet = propertySheet, 
        activeProcess = getActiveProcess(projectPath = projectPath, modelName = modelName)
    )
    output
}


# Function that gets the process names of the processes returning the specified data type
getProcessNamesByDataType <- function(dataType, processTable) {
    hasRequestedDataType <- processTable$functionOutputDataType == dataType
    if(any(hasRequestedDataType)) {
        output <- processTable$processName[hasRequestedDataType]
    }
    else {
        output <- NULL
    }
    
    ### This is a trick to keep arrays through jsonlite::toJSON, and must happen here (before the data.table is created):
    #as.list(output)
    return(output)
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


# Function to convert to JSON string, used to send only strings and arrays of strings to the GUI:
toJSONString <- function(DT) {
    # Convert the possible values, which can have 0 or positive length:
    ####possibleValuesToJSONString(DT)
    #DT[, possibleValues := lapply(possibleValues, vectorToJSONStringOne, stringifyVector = FALSE)]
    DT[, possibleValues := lapply(possibleValues, possibleValuesToJSONStringOne, nrow = nrow(DT))]
    
    # Convert vector value:
    atVector <- isVectorParameter(DT$format)
    if(any(atVector)) {
        DT[atVector, value := lapply(value, vectorToJSONStringOne)]
    }
    
    # Convert all the other columns, which are required to have length 1:
    other <- setdiff(names(DT), "possibleValues")
    cellToJSONString(DT, cols = other)
    DT[]
}



cellToJSONString <- function(DT, cols) {
    DT[, (cols) := lapply(.SD, cellToJSONStringOneColumn), .SDcols = cols]
}
cellToJSONStringOne <- function(x) {
    if(length(x) == 0) {
        #warning("StoX: Length 1 required for process properties except possibleValues.")
        x <- ""
    }
    if(!is.character(x)) {
        x <- as.character(jsonlite::toJSON(x, auto_unbox = TRUE))
    }
    return(x)
}
cellToJSONStringOneColumn <- function(x) {
    lapply(x, cellToJSONStringOne)
}




#vectorToJSONString <- function(DT) {
#    output <- DT[, possibleValues := lapply(possibleValues, vectorToJSONStringOne, nrow = nrow(DT))]
#    return(output)
#}

# The parameter nrow is needed to ensure that data.table does not intruduce an extra list when there is more than one row and one of the cells has only one element:
vectorToJSONStringOne <- function(x, stringifyVector = TRUE) {
    # Set empty possible values to numeric(), which ensures [] in OpenCPUs conversion to JSON (jsonlite::toJSON with auto_unbox = TRUE):
    if(length(x) == 0) {
        x <- numeric()
        #as.character(jsonlite::toJSON(x, auto_unbox = TRUE))
    }
    
    ## If data.table, simply convert to JSON string:
    #else if(data.table::is.data.table(x)) {
    #    as.character(jsonlite::toJSON(x, auto_unbox = TRUE))
    #}
    
    # Convert to JSON string for each element if not already character:
    else if(!data.table::is.data.table(x)) {
        if(!is.character(x)) {
            x <- sapply(x, function(y) as.character(jsonlite::toJSON(y, auto_unbox = TRUE)))
        }
        if(length(x) == 1) {
            # This trick with a double list is to ensure that data.table actually converts to a list so that jsonlite returns square brackets (do not change this unless you really know what you are doing!!!!!!!!!!):
            x <- list(x)
        }
    }
    
    if(stringifyVector) {
        x <- as.character(jsonlite::toJSON(x, auto_unbox = TRUE))
    }
    return(x)
}


# The parameter nrow is needed to ensure that data.table does not intruduce an extra list when there is more than one row and one of the cells has only one element:
possibleValuesToJSONStringOne <- function(x, nrow) {
    # Set empty possible values to numeric(), which ensures [] in OpenCPUs conversion to JSON (jsonlite::toJSON with auto_unbox = TRUE):
    if(length(x) == 0) {
        x <- numeric()
    }
    # Convert to JSON string for each element if not already character:
    else {
        if(!is.character(x)) {
            x <- sapply(x, function(y) as.character(jsonlite::toJSON(y, auto_unbox = TRUE)))
        }
        if(length(x) == 1) {
            # This trick with a double list is to ensure that data.table actually converts to a list so that jsonlite returns square brackets (do not change this unless you really know what you are doing!!!!!!!!!!):
            if(nrow == 1) {
                x <- list(list(x))
            }
            else {
                x <- list(x)
            }
        }
    }
    return(x)
}





#' GUI function: Set procecss properties.
#' 
#' @param groupName The name of the property group, one of "processArguments", "functionInputs" and "functionParameters".
#' @param name The name of the property, such as "processName", "functionName", one of the process parameters ("enabled", "showInMap" and "fileOutput"), the name of a funciton input, or the name of a function parameter. 
#' @param value The value to set to the property (string).
#' @inheritParams general_arguments
#' 
#' @export
#' 
setProcessPropertyValue <- function(groupName, name, value, projectPath, modelName, processID) {
    
    # Parse the value (this takes care of converting true to TRUE, interpret integers and strings, and even to parse JSON strings to R objects):
    value <- parseParameter(value)
    #value <- jsonlite::fromJSON(value)
    
    # The flag updateHelp is TRUE only if the functionName is changed:
    updateHelp <- FALSE
    
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
            # Set updateHelp to TRUE, so that the GUI can update the help page only when needed:
            updateHelp <- TRUE
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
            # All process parameters are logical:
            value <- as.logical(value)
            modifyProcessParameters(
                projectPath = projectPath, 
                modelName = modelName, 
                processID = processID, 
                newProcessParameters = stats::setNames(list(value), name)
            )
        }
    }
    # If the process property 'functionInputs' is given, modify the function inputs:
    if(groupName == "functionInputs") {
        modifyFunctionInputs(
            projectPath = projectPath, 
            modelName = modelName, 
            processID = processID, 
            newFunctionInputs = stats::setNames(list(value), name)
        )
    }
    # If the process property 'functionInputs' is given, modify the function parameters:
    if(groupName == "functionParameters") {
        ### This is unnecessary, since jsonlite::fromJSON takes care of the types in parseParmeter() ###
        # Convert to R object based on the type:
        value <- convertFunctionParameter(
            functionParameterName = name, 
            functionParameterValue = value, 
            functionName = getFunctionName(projectPath, modelName, processID))
        # Modify the process parameter:
        modifyFunctionParameters(
            projectPath = projectPath, 
            modelName = modelName, 
            processID = processID, 
            newFunctionParameters = stats::setNames(list(value), name)
        )
    }
    
    # Reset the active process ID to the process before the modified process:
    resetModel(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID, 
        processDirty = TRUE
    )
    
    # Return the modified process properties:
    output <- getProcessPropertySheet(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID
    )
    
    # Add the process table, so that the GUI can update the list of processes, and all its symbols:
    output <- c(
        list(processTable = getProcessTable(projectPath = projectPath, modelName = modelName)), 
        output
    )
    
    # Add updateHelp:
    output <- c(
        list(updateHelp = updateHelp), 
        output
    )
    # Add also the saved status:
    output$saved <- isSaved(projectPath)
    
    return(output)
}

# Convert to the type of the parameters:
convertFunctionParameter <- function(functionParameterName, functionParameterValue, functionName) {
    # Get the primitive type and the format:
    type <- getStoxFunctionParameterTypes(functionName)[functionParameterName]
    format = getFunctionParameterFormats(functionName)[functionParameterName]
    
    # Apply the conversion function:
    #if(format %in% c("single", "vector")) {
    if(format %in% "single") {
        # If empty string, convert to NULL for non-character type:
        if(is.character(functionParameterValue) && nchar(functionParameterValue) == 0 && type != "character") {
            functionParameterValue <- NULL
        }
        fun <- paste0("as.", type)
        functionParameterValue <- do.call(fun, list(functionParameterValue))
    }
    
    return(functionParameterValue)
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
    # Return empty string if the function name is missing:
    if(length(packageName_functionName) == 0 || nchar(packageName_functionName) == 0) {
        return("")
    }
    
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



#' GUI function: Get possible tables, operators and unique values for use in the filter expression builder.
#' 
#' @inheritParams general_arguments
#' 
#' @export
#' 
getFilterOptionsAll <- function(projectPath, modelName, processID, include.numeric = TRUE) {

    # Run the process without saving and without filter:
    # Add a stop if the previous process has not been run!!!!!!!!!!!!!
    processOutput <- runProcess(projectPath, modelName, processID, msg = FALSE, returnProcessOutput = TRUE, replaceArgs = list(FilterExpression = list()))
    # If the process output is a list of lists, unlist the top level and add names separated by slash:
    processOutput <- unlistProcessOutput(processOutput)
    
    # Get the column names:
    name <- lapply(processOutput, names)
    
    # Get the data types:
    type <- lapply(processOutput, function(x) sapply(x, firstClass))
    
    # Get the operators:
    operators <- lapply(type, function(x) if(length(x)) getRstoxFrameworkDefinitions("filterOperators")[x] else NULL)
    
    # Get a list of unique values for each column of each table:
    #options <- lapply(processOutput, getPossibleValuesOneTable, include.numeric = include.numeric)
    options <- mapply(getPossibleValuesOneTable, processOutput, type, include.numeric = include.numeric, SIMPLIFY = FALSE)
    options <- lapply(options, function(x) lapply(x, getOptionList))
    
    # Return the
    output <- lapply(
        seq_along(options),
        function(ind)
            structure(
                list(
                    mapply(
                        list,
                            name = name[[ind]],
                            type = type[[ind]],
                            operators = operators[[ind]],
                            options = options[[ind]],
                            SIMPLIFY = FALSE
                        )
                    ), 
                names = "fields"
                )
            )
    
    names(output) <- names(options)
    
    # Add the fields level:
    output <- list(
        tableNames = names(output),
        allFields = output
    )
    
    # Return a list of the tableNames, columnNames and possibleValues:
    return(output)
}

    

getOptionList <- function(option, digits = 6) {
    lapply(option, function(x) list(name = if(is.numeric(x)) format(x, digits = digits) else x, value = x))
}


getPossibleValuesOneTable <- function(table, type, include.numeric = FALSE, include.POSIXct = FALSE) {
    # Return empty named list if no input:
    if(length(table) == 0) {
        return(list(a = 1)[0])
    }
    
    # Get the indices of the variables to get possible values from:
    validInd <- seq_len(ncol(table))
    if(!include.numeric) {
        validInd <- setdiff(validInd, which(type %in% c("numeric", "integer", "double")))
    }
    if(!include.POSIXct) {
        validInd <- setdiff(validInd, which(type %in% c("POSIXct")))
    }
    
    
    #if(include.numeric) {
    #    validInd <- seq_len(ncol(table))
    #}
    #else {
    #    validInd <- which(! type %in% c("numeric", "integer", "double"))
    #}
    
    # Declare a list for the output, with empty on numeric type if include.numeric = FALSE
    output <- vector("list", ncol(table))
    # Unique and then sort each column:
    output[validInd] <- lapply(table[, ..validInd], sortUnique)
    
    #lapply(table, sortUnique)
    output
}

# Simple function to sort the unique values:
sortUnique <- function(y) {
    # 2020-06-18 Added na.last = FALSE to include NAs in the filter options:
    #sort(unique(y), na.last = FALSE)
    
    # Get first the unique values, then check that the length of these are not identical to the length of the vector, and then sort:
    uniquey <- unique(y)
    if(length(uniquey) < length(y)) {
        sort(uniquey, na.last = FALSE)
    }
    else {
        NULL
    }
}






##### Handle parameter tables: #####

getParameterTableElement <- function(projectPath, modelName, processID, format, element) {
    # Get the parameterTableInfo
    parameterTableInfo <- getRstoxFrameworkDefinitions("parameterTableInfo")
    
    # If given as a function, apply that function to the function arguments:
    if(is.function(parameterTableInfo[[format]][[element]])) {
        # Get the function arguments:
        functionArguments <- getFunctionArguments(
            projectPath = projectPath, 
            modelName = modelName, 
            processID = processID
        )$functionArguments
        # Apply the function:
        output <- do.call_robust(
            parameterTableInfo[[format]][[element]], 
            functionArguments
        )
    }
    else {
        output <- parameterTableInfo[[format]][[element]]
    }
    
    return(output)
}

# Get the title of a parameter table:
getParameterTableTitle <- function(projectPath, modelName, processID, format) {
    getParameterTableElement(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID, 
        format = format, 
        element = "title"
    )
}

# Get the column names of a parameter table:
getParameterTableColumnNames <- function(projectPath, modelName, processID, format) {
    getParameterTableElement(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID, 
        format = format, 
        element = "columnNames"
    )
}

# Get the variable types of a parameter table:
getParameterTableVariableTypes <- function(projectPath, modelName, processID, format) {
    getParameterTableElement(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID, 
        format = format, 
        element = "variableTypes"
    )
}
    
# Get the possible values of a parameter table:
# Unfinished!!!!!!!!!!!!!!!
getParameterTablePossibleValues <- function(projectPath, modelName, processID, format) {
    #getParameterTableElement(
    #    projectPath = projectPath, 
    #    modelName = modelName, 
    #    processID = processID, 
    #    format = format, 
    #    element = "possibleValues"
    #)
    columnNames <- getParameterTableColumnNames(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID, 
        format = format
    )
    rep(list(list()), length(columnNames))
}

#' GUI function: Function to get the info required for populating a parameter table builder in the GUI
#' 
#' @inheritParams general_arguments
#' @param format A character string naming the format to get info for.
#' 
#' @export
#' 
getParameterTableInfo <- function(projectPath, modelName, processID, format) {
    list(
        parameterTableTitle = getParameterTableTitle(
            projectPath = projectPath, 
            modelName = modelName, 
            processID = processID, 
            format = format
        ), 
        parameterTableColumnNames = getParameterTableColumnNames(
            projectPath = projectPath, 
            modelName = modelName, 
            processID = processID, 
            format = format
        ), 
        parameterTableVariableTypes = getParameterTableVariableTypes(
            projectPath = projectPath, 
            modelName = modelName, 
            processID = processID, 
            format = format
        ), 
        parameterTablePossibleValues = getParameterTablePossibleValues(
            projectPath = projectPath, 
            modelName = modelName, 
            processID = processID, 
            format = format
        )
    )
}






##' GUI function: Get a table of all parameter table info, holding tha name and type of all columns defined in parameter tables in all #Rstox packages
##' 
##' @export
##' 
#getAllParameterTableInfo <- function() {
#    parameterTableInfo <- getRstoxFrameworkDefinitions("parameterTableInfo")
#    info <- lapply(parameterTableInfo, function(x) x$info)
#    info <- unique(data.table::rbindlist(info))
#    return(info)
#}
######


