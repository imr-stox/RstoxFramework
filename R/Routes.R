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
    prior <- seq_len(ind - 1)
    # Discard processes with enabled = FALSE:
    prior <- subset(prior, processTable$enabled[prior])
    
    # Select the prior processes:
    priorProcesses <- processTable$processName[prior]
    
    # Get the names of the processes from which funciton intpu is requested:
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


getCanShowInMap <- function(functionNames) {
    # Get the data types returned by the functions of the processes:
    dataTypes <- sapply(functionNames, getStoxFunctionMetaData, metaDataName = "functionOutputDataType")
    
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
    
    # Select the type if interactive mode depending on the output data type from the process:
    if(dataType %in% getRstoxFrameworkDefinitions("stratumDataType")) {
        "stratum"
    }
    else if(dataType %in% getRstoxFrameworkDefinitions("asouticPSUDataType")) {
        "asouticPSU"
    }
    else if(dataType %in% getRstoxFrameworkDefinitions("sweptAreaPSUDataType")) {
        "sweptAreaPSU"
    }
    else if(dataType %in% getRstoxFrameworkDefinitions("assignmentDataType")) {
        "assignment"
    }
    else if(dataType %in% getRstoxFrameworkDefinitions("stationDataType")) {
        "station"
    }
    else if(dataType %in% getRstoxFrameworkDefinitions("EDSUDataType")) {
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
        getSweptAreaPSUData(
            projectPath = projectPath, 
            modelName = modelName, 
            processID = processID
        )
    }
    else {
        geojsonio::geojson_json(getRstoxFrameworkDefinitions("emptyStratumPolygon"))
    }
}


# Individual get data functions:
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

#getEDSUData <- function(projectPath, modelName, processID) {
#    # Get the EDSU data:
#    Log <- getProcessOutput(projectPath, modelName, processID, tableName = "Log")$Log
#    extrapolateEDSU(Log)
#}

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
    
    process <- getProcess(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID
    )
    
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
        )
    )
    # 8. value:
    processArguments$value <- c(
        process["processName"], 
        # Remove the package address and only use the function name:
        getFunctionNameFromPackageFunctionName(process["functionName"]), 
        process[["processParameters"]]
    )
    
    # Add help on the processArguments:
    #processArguments$help <- NULL
    
    # Remove the showInMap argument if not relevant:
    if(!getCanShowInMap(process$functionName)) {
        keep <- c(
            TRUE, 
            TRUE, 
            processParameterNames != "showInMap"
        )
        processArguments <- processArguments[keep, ]
    }
    #######################
    
    # Declare functionInputs and functionParameters and 
    functionInputs <- data.table::data.table()
    functionParameters <- data.table::data.table()
    
    if(length(process$functionName)) {
        
        # Get the function argument hierarchy:
        functionArgumentHierarchy = getStoxFunctionMetaData(process$functionName, "functionArgumentHierarchy", showWarnings = FALSE)
        
        ##############################
        ##### 2. FunctionInputs: #####
        ##############################
        # Run only if there are function inputs:
        if(length(process$functionInputs)) {
            # Get the process table, which is needed to get the output data types from the prior processes for use in the function inputs:
            processTable <- getProcessTable(projectPath, modelName)
            thisProcessIndex <- which(processTable$processID == processID)
            processTable <- processTable[seq_len(thisProcessIndex), ]
            functionInputNames <- names(process$functionInputs)
            
            # Define the function inputs:
            functionInputs <- data.table::data.table(
                # 1. name:
                name = as.list(functionInputNames), 
                # 2. displayName:
                displayName = as.list(functionInputNames), 
                # 3. description:
                description = replaceEmpty(getStoxFunctionMetaData(process$functionName, "functionArgumentDescription")[functionInputNames]), 
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
                value = replaceEmpty(process$functionInputs, vector = FALSE)
            )
            
            # Apply the StoX funciton argument hierarcy here using getStoxFunctionMetaData("functionArgumentHierarchy"):
            argumentsToShow <- getArgumentsToShow(
                functionName = process$functionName, 
                functionArguments = functionInputs$value, 
                functionArgumentHierarchy = functionArgumentHierarchy
            )
            # Select only the items to show in the GUI:
            if(!all(argumentsToShow)) {
                functionInputs <- lapply(functionInputs, "[[", argumentsToShow)
            }
        }
        ##############################
        
        
        ##################################
        ##### 3. FunctionParameters: #####
        ##################################
        
        # Run only if there are function parameters (which there always will be):
        if(length(process$functionParameters)) {
            # Get the names of the function parameters:
            functionParameterNames <- names(process$functionParameters)
            
            # Define the function parameters:
            functionParameters <- data.table::data.table(
                # 1. name:
                name = as.list(functionParameterNames), 
                # 2. displayName:
                displayName = as.list(functionParameterNames), 
                # 3. description:
                description = replaceEmpty(getStoxFunctionMetaData(process$functionName, "functionArgumentDescription")[functionParameterNames]), 
                # 4. type:
                type = replaceEmpty(getStoxFunctionParameterPropertyTypes(process$functionName)[functionParameterNames]),
                # 5. format:
                format = replaceEmpty(getFunctionParameterPropertyFormats(process$functionName)[functionParameterNames]),
                # 6. default:
                default = replaceEmpty(getStoxFunctionParameterDefaults(process$functionName)[functionParameterNames], vector = FALSE),
                # 7. possibleValues:
                # Set this as list to ensure that we keep the square brackets "[]" in the JSON string even with auto_unbox = TRUE.
                possibleValues = lapply(replaceEmpty(getStoxFunctionParameterPossibleValues(process$functionName)[functionParameterNames]), as.list),
                # 8. value:
                value = replaceEmpty(process$functionParameters, vector = FALSE)
            )
            
            # Apply the StoX funciton argument hierarcy here using getStoxFunctionMetaData("functionArgumentHierarchy"):
            argumentsToShow <- getArgumentsToShow(
                functionName = process$functionName, 
                functionArguments = functionParameters$value, 
                functionArgumentHierarchy = functionArgumentHierarchy
            )
            # Select only the items to show in the GUI:
            if(!all(argumentsToShow)) {
                functionParameters <- lapply(functionParameters, "[[", argumentsToShow)
            }
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
        help = getFunctionHelpAsHtml(
            projectPath = projectPath, 
            modelName = modelName, 
            processID = processID, 
            outfile = outfile
        )
    )
    
    # Return the list of process property groups (process property sheet):
    output
}

#' 
#' @export
#' 
setProcessPropertyValue <- function(groupName, name, value, projectPath, modelName, processID) {
    
    # Parse the value:
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
    
    # Return the modified process properties:
    getProcessPropertySheet(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID
    )
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
getFunctionHelpAsHtml <- function(projectPath, modelName, processID, outfile = NULL) {
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
    html <- getObjectHelpAsHtml(packageName = packageName, objectName = functionName, outfile = outfile)
    html
}


#' 
#' @export
#' 
getObjectHelpAsHtml <- function(packageName, objectName, outfile = NULL) {
    
    # Read the documentation database:
    db <- tools::Rd_db(packageName)
    # Write the help to file as html and read back:
    objectName.Rd <- paste0(objectName, ".Rd")
    
    # Get the links of the package:
    Links <- tools::findHTMLlinks(pkgDir = find.package(packageName))
    if(! names(Links) %in% "ReadBiotic") {
        return("")
    }
    
    
    # Write to a temporary file
    if(length(outfile) == 0) {
        outfile <- tempfile(fileext = ".html")
    }
    tools::Rd2HTML(db[[objectName.Rd]], out = outfile, Links = Links)
    html <- paste(readLines(outfile), collapse="\n")
    unlink(outfile, force = TRUE)
    html
}
