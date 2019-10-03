#getModelNames
#getProcessPropertyCategories
#getProcessPropertyCategoriesItems
#setProcessPropertyItem


#' 
#' @export
#' 
getModelNames <- function() {
    getRstoxFrameworkDefinitions("stoxModelTypes")
    # convertToJSON(out)
}
#' 
#' @export
#' 
getModeInfo <- function() {
    getRstoxFrameworkDefinitions("stoxModelInfo")
}


#' 
#' @export
#' 
convertToJSON <- function(x) {
    jsonlite::toJSON(x)
}

getProcessPropertyCategories <- function(process) {
    out <- names(process)
    convertToJSON(out)
}

getProcessPropertyCategoriesItems <- function(process, category) {
    out <- process$category
    convertToJSON(out)
}

setProcessPropertyItem<- function(process, propertyItem, value) {
    process$propertyItem <- value
}

#' 
#' @export
#' 
getAvailableTemplatesDescriptions <- function() {
    availableTemplates <- getAvaiableTemplates(TRUE)
    
    df <- data.table::data.table(
        name = names(availableTemplates), 
        description = unname(sapply(availableTemplates, "[[", "description"))
    )
    
    # convertToJSON(df)
} 



# Reads a table of the following columns:
# 1. Process name
# 4. CanShowInMap
# 5. CanModify
# 6. ShowInMap

#' 
#' @export
#' 
getProcessTable <- function(projectPath, modelName) {
    
    getFunctionInputErrors <- function(ind, processTable, functionInputs) {
        
        #### Check wheter the processes from which process output is requested as funciton input exist prior to the current process: ####
        # Get names of processes prior to the current process:
        priorProcesses <- processTable$processName[seq_len(ind - 1)]
        # Get the names of the processes from which funciton intpu is requested:
        requestedProcessNames <- functionInputs[[ind]]
        requestedFunctionInputDataTypes <- names(requestedProcessNames)
        
        # Are all of the function inputs present in the prior processes?:
        requestedProcessesExist <- all(requestedProcessNames %in% priorProcesses)
        if(!requestedProcessesExist) {
            warning(
                "The following requested processes do not exist prior to the process", 
                processTable$processName[ind], 
                ": ", 
                paste(setdiff(requestedProcessNames, priorProcesses), collapse = ", ")
            )
        }
        
        #### Check also that the processes given by the function inputs acutally return the desired data type: ####
        
        
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
        
        
        requestedProcessesExist && all(correctDataTypeRequested)
    }
    
    # Get the current project description:
    projectDescription <- getCurrentProjectDescription(projectPath)
    
    # Get project name and function name:
    processNames <- sapply(projectDescription[[modelName]], "[[", "processName")
    functionNames <- sapply(projectDescription[[modelName]], "[[", "functionName")
    # Get the data types returned by the functions of the processes:
    dataTypes <- sapply(functionNames, getFunctionOutputDataType)
    # Check whether the data type can be shown in the map:
    canShowInMap <- getCanShowInMap(dataTypes)
    # ... and whether the user hat defined that the data from the process should be shown in the map:
    showInMap <- sapply(projectDescription[[modelName]], function(process) process$processParameters$showInMap)
    # Check whether the process returns process data:
    hasProcessData <- sapply(functionNames, isProcessDataFunction)
    
    # Group the info to a table for now:
    processTable <- data.table::data.table(
        processName = processNames, 
        functionName = functionNames, 
        dataType = dataTypes, 
        canShowInMap = canShowInMap, 
        showInMap = showInMap, 
        hasProcessData = hasProcessData
    )
    
    # Get the funciton inputs (as a list):
    functionInputs <- lapply(projectDescription[[modelName]], "[[", "functionInputs")
    
    # Get the processes that has errors:
    hasError <- sapply(
        seq_along(processNames), 
        getFunctionInputErrors, 
        processTable = processTable, 
        functionInputs = functionInputs
        )
    processTable$hasError <- hasError
    
    # Get also the current process, and define the column 'hasBeenRun':
    #currentProcess <- getCurrentProcess(projectPath, modelName)
    #hasBeenRun <- seq_along(processNames) <= which(processNames == currentProcess)
    #processTable$hasError <- hasBeenRun

    
    # Reads a table of the following columns:
    # 1. processName
    # 4. canShowInMap
    # 5. hasProcessData
    # 6. showInMap
    
    # 2. hasBeenRun
    # 3. hasError
    
    # 
    # There are two different types of actions, changing processes and changing parameters. Changing processes iduces reset of current process, whereas changing parameters do not. This will be added to the projectDescriptionIndex.txt. Errors given by HasError only occur when there are missing inputs, that is that the processes requersted in funciton inputs do not exist BEFORE the actual function. This will be a check to run in the route-funcitons Add-, Delete- and MoreProcess, which call the corresponding add-, delete- and moreProcess in Framework.R, and then calls getProjectList.
    
    processTable
}

getCurrentProcess <- function(projectPath, modelName) {
    
}


getCanShowInMap <- function(dataTypes) {
    dataTypes %in% getRstoxFrameworkDefinitions("dataTypesToShowInMap")
}


# Add stop position of eht EDSUs for plotting in the map:
getStopLongitudeLatitude <- function() {
    
}



getProcessStatus <- function(projectPath, modelName, processName) {
    
    # Reads a table of the following columns:
    # 1. Process name
    # 2. HasBeenRun
    # 3. HasError
    # 4. CanShowInMap
    # 5. HasProcessData
    # 6. ShowInMap

}

# To be run after each process. This is the actual geojson object to plot:
getPlottingProcessFeatures <- function(projectPath, modelName, processName) {
    
}



