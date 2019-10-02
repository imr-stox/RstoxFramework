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

getProcessPropertyCategories <- function(Process) {
    # browser() 
    out <- names(Process)
    convertToJSON(out)
}

getProcessPropertyCategoriesItems <- function(Process, Category) {
    # browser()
    out <- Process$Category
    convertToJSON(out)
}

setProcessPropertyItem<- function(Process, PropertyItem, value) {
    Process$PropertyItem <- value
}

#' 
#' @export
getAvailableTemplatesDescriptions <- function() {
    # browser()
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

getProcessList <- function(ProjectPath, ModelName) {
    projectDescription <- getCurrentProjectDescription(ProjectPath)
    names(projectDescription[[ModelName]])
    
    # Reads a table of the following columns:
    # 1. Process name
    # 4. CanShowInMap
    # 5. CanModify
    # 6. ShowInMap
    
    # 2. HasBeenRun
    # 3. HasError
    
    # 
    # There are two different types of actions, changing processes and changing parameters. Changing processes iduces reset of current process, whereas changing parameters do not. This will be added to the projectDescriptionIndex.txt. Errors given by HasError only occur when there are missing inputs, that is that the processes requersted in funciton inputs do not exist BEFORE the actual function. This will be a check to run in the route-funcitons Add-, Delete- and MoreProcess, which call the corresponding add-, delete- and moreProcess in Framework.R, and then calls getProjectList.
    
}

getCanShowInMap <- function() {
    
}







getProcessStatus <- function(ProjectPath, ModelName, ProcessName) {
    
    # Reads a table of the following columns:
    # 1. Process name
    # 2. HasBeenRun
    # 3. HasError
    # 4. CanShowInMap
    # 5. CanModify
    # 6. ShowInMap

}

# To be run after each process. This is the actual geojson object to plot:
getPlottingProcessFeatures <- function(ProjectPath, ModelName, ProcessName) {
    
}



