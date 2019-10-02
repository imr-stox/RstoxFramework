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
    # browser()
    jsonlite::toJSON(x)
}

getProcessPropertyCategories <- function(process) {
    # browser() 
    out <- names(process)
    convertToJSON(out)
}

getProcessPropertyCategoriesItems <- function(process, category) {
    # browser()
    out <- process$category
    convertToJSON(out)
}

setProcessPropertyItem<- function(process, propertyItem, value) {
    process$propertyItem <- value
}

#' 
#' @export
getAvailableTemplatesDescriptions <- function() {
    availableTemplates <- getAvaiableTemplates(TRUE)
    
    df <- data.frame(
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

getProcessTable <- function(projectPath, modelName) {
    # Get the current project description:
    projectDescription <- getCurrentProjectDescription(projectPath)
    
    # Get project name, function name
    processNames <- sapply(projectDescription[[modelName]], "[[", "processName")
    functionNames <- sapply(projectDescription[[modelName]], "[[", "functionName")
    showInMap <- sapply(projectDescription[[modelName]], function(process) process$processParameters$showInMap)
    dataTypes <- sapply(functionNames, getFunctionOutputDataType)
    functionInputs <- lapply(projectDescription[[modelName]], "[[", "functionInputs")
    
    canShowInMap <- getCanShowInMap(dataTypes)
    
    hasProcessData <- sapply(functionNames, isProcessDataFunction)
    
    
    
    
    
    processTable <- data.frame(
        processName = processNames, 
        canShowInMap = canShowInMap, 
    )
    
    getFunctionInputErrors <- function(ind, processNames, functionNames, dataTypes, functionInputs) {
        # Check wheter the processes from which process output is requested as funciton input exist prior to the current process:
        requestedProcessesExist <- all(functionInputs[[ind]] %in% processNames[seq_len(ind - 1)])
        # Check also that the processes given by the function inputs acutally return the desired data type:
        requestedProcessNames <- names(functionInputs[[ind]])
        correctDataTypeRequested <- all(getFunctionOutputDataType(functionNames[]))
        
    }
    
    
    
    
    # Reads a table of the following columns:
    # 1. processName
    # 4. canShowInMap
    # 5. hasProcessData
    # 6. showInMap
    
    # 2. hasBeenRun
    # 3. hasError
    
    # 
    # There are two different types of actions, changing processes and changing parameters. Changing processes iduces reset of current process, whereas changing parameters do not. This will be added to the projectDescriptionIndex.txt. Errors given by HasError only occur when there are missing inputs, that is that the processes requersted in funciton inputs do not exist BEFORE the actual function. This will be a check to run in the route-funcitons Add-, Delete- and MoreProcess, which call the corresponding add-, delete- and moreProcess in Framework.R, and then calls getProjectList.
    
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



