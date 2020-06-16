# A list of the attributes of the exported StoX functions:
# The format describes the actual content, such as catchabilityTable, filePath, filter, etc. These are used by StoX to choose action on these parameters.
# The primitive type (one of integer, double, logical, character) will be interpreted in the process property functions from the type of the function input or parameter.
#' 
#' @export
#' 
stoxFunctionAttributes <- list(
    # Bootstrap baseline:
    Bootstrap = list(
        functionType = "bootstrap", 
        functionCategory = "analysis", 
        functionOutputDataType = "BootstrapData", 
        functionParameterFormat = list(
            BootstrapMethodTable = "bootstrapMethodTable"
        )
    )
)

# Define the process property formats:
#' 
#' @export
#' 
processPropertyFormats <- list(
    bootstrapMethodTable = list(
        title = "Define the bootstrap method",
        type = "table", 
        info = data.table::data.table(
            name = c(
                "ProcessName", 
                "ResampleFunction", 
                "ResampleBy", 
                "Seed"
            ), 
            type = c(
                "character",
                "character", 
                "character",
                "integer"
            )
        )
    )
)
