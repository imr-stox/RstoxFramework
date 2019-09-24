#getModelNames
#getProcessPropertyCategories
#getProcessPropertyCategoriesItems
#setProcessPropertyItem


#' 
#' @export
getModelNames <- function() {
    # browser()
    out <- getRstoxFrameworkDefinitions("stoxModelTypes")
    convertToJSON(out)
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


