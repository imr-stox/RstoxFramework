getModelNames
getProcessPropertyCategories
getProcessPropertyCategoriesItems
setProcessPropertyItem


getModelNames <- function() {
    out <- getRstoxFrameworkDefinitions("stoxModelTypes")
    convertToJSON(out)
}

convertToJSON <- function(x) {
    # Something like this:
    toJSON( x, indent=0, method="C" )
}
