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
    ), 
    
    ReportBootstrap = list(
        functionType = "modelData", 
        functionCategory = "report", 
        functionOutputDataType = "ReportBootstrapData", 
        # This is an example of using an expression to determine when to show a parameter:
        functionParameterFormat = list(
            #TargetVariable = "targetVariable_ReportBootstrap", 
            GroupingVariables = "groupingVariables_ReportBootstrap"
        ), 
        functionArgumentHierarchy = list(
            AggregationWeightingVariable = list(
                ReportFunction = expression(RstoxBase::getWeightingFunctions())
            ), 
            BootstrapReportWeightingVariable = list(
                ReportFunction = expression(RstoxBase::getWeightingFunctions())
            )
        )
    )
)

#' 
#' @export
#' 
getResamplableProcesses <- function(projectPath) {
   
    # Get the data types that can be resampled:
    resamplableDataTypes <- getRstoxFrameworkDefinitions("resamplableDataTypes")
    # Find the processes that can be resampled:
    stoxLibrary <- getRstoxFrameworkDefinitions("stoxLibrary")
    validFunctions <- names(stoxLibrary)[sapply(stoxLibrary, "[[", "functionOutputDataType")  %in% resamplableDataTypes]
    
    # Get the baseline processes with valid functions:
    baselineProcesses <- getProcessAndFunctionNames(
        projectPath = projectPath, 
        modelName = "baseline"
    )
    
    processNames <- baselineProcesses[getFunctionNameFromPackageFunctionName(functionName) %in% validFunctions, processName]
    
    return(processNames)
}

#' 
#' @export
#' 
getResampleFunctions <- function() {
    paste0("Resample", getRstoxFrameworkDefinitions("resamplableDataTypes"))
}

# Define the process property formats:
#' 
#' @export
#' 
processPropertyFormats <- list(
    bootstrapMethodTable = list(
        class = "table", 
        title = "Define the bootstrap method",
        columnNames = c(
            "ProcessName", 
            "ResampleFunction", 
            #"ResampleBy", 
            "Seed"
        ), 
        variableTypes = c(
            "character",
            "character", 
            #"character",
            "integer"
        ), 
        possibleValues = function(projectPath) {
            # Must be an unnamed list:
            possibleValues = list(
                getResamplableProcesses(projectPath), 
                getResampleFunctions(),
                NULL
            )
        }
        #possibleValues = list(
        #    NULL, 
        #    getResampleFunctions(),
        #    NULL
        #)
    ), 
    
    targetVariable_ReportBootstrap = list(
        class = "single", 
        possibleValues = function(BootstrapData, BaselineProcess) {
            sort(setdiff(names(BootstrapData[[BaselineProcess]]), "BootstrapID"))
        }
    ), 
    
    groupingVariables_ReportBootstrap = list(
        class = "vector", 
        title = "One or more variables to group super-individuals by when reporting BootstrapData", 
        possibleValues = function(BootstrapData, BaselineProcess) {
            sort(setdiff(names(BootstrapData[[BaselineProcess]]), "BootstrapID"))
        }, 
        variableTypes <- "character"
    )
)

