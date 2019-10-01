#getModelNames
#getProcessPropertyCategories
#getProcessPropertyCategoriesItems
#setProcessPropertyItem


#' 
#' @export
getModelNames <- function() {
    # browser()
    getRstoxFrameworkDefinitions("stoxModelTypes")
    # convertToJSON(out)
}

#' 
#' @export
#' @importFrom jsonlite toJSON fromJSON
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





getProcessNames <- function(ProjectPath, ModelName) {
    
}



getProcessStatus <- function(ProjectPath, ModelName, ProcessName) {
    
    # Reads a table of the following columns:
    # 1. Process name
    # 2. HasBeenRun
    # 3. HasError
    # 4. CanBePlotted
    # 5. ToBePlotted

}

# To be run after each process. This is the actual geojson object to plot:
getPlottingLayerFeatures <- function(ProjectPath, ModelName, ProcessName) {
    
}




# Define whether a plotting layer shoud be visible in the map:
setToBePlotted <- function(ProjectPath, ModelName, ProcessName, ToBePlotted = FALSE) {
    
}

# Get whether a plotting layer shoud be visible in the map:
getToBePlotted <- function(ProjectPath, ModelName, ProcessName) {
    
}


