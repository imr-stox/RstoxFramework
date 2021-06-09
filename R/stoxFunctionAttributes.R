#' A list of the attributes of the exported StoX functions:
#' 
#' The format describes the actual content, such as catchabilityTable, filePath, filter, etc. These are used by StoX to choose action on these parameters.
#' The primitive type (one of integer, double, logical, character) will be interpreted in the process property functions from the type of the function input or parameter.
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
            BootstrapMethodTable = "bootstrapMethodTable", 
            OutputProcesses = "outputProcesses", 
            BaselineSeedTable = "baselineSeedTable"
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
                AggregationFunction = expression(RstoxBase::getWeightingFunctions())
            ), 
            BootstrapReportWeightingVariable = list(
                BootstrapReportFunction = expression(RstoxBase::getWeightingFunctions())
            )
        )
    )
)

#' Utility function for processPropertyFormats. This is exported in order for processPropertyFormats to be albe to use it:
#' 
#' @inheritParams general_arguments
#' 
#' @export
#' 
getResamplableProcesses <- function(projectPath) {
   
    # Get the data types that can be resampled:
    resamplableDataTypes <- getRstoxFrameworkDefinitions("resamplableDataTypes")
    # Find the functions that can be resampled:
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

#' Utility function for processPropertyFormats. This is exported in order for processPropertyFormats to be albe to use it:
#' 
#' @export
#' 
getResampleFunctions <- function() {
    paste0("Resample", getRstoxFrameworkDefinitions("resamplableDataTypes"))
}

#' Process property formats for RstoxFramework
#' 
#' @export
#' 
processPropertyFormats <- list(
    bootstrapMethodTable = list(
        class = "table", 
        title = "Define the bootstrap method",
        columnNames = c(
            "ResampleFunction", 
            "ProcessName", 
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
                getResampleFunctions(),
                getResamplableProcesses(projectPath), 
                NULL
            )
        }
        #possibleValues = list(
        #    NULL, 
        #    getResampleFunctions(),
        #    NULL
        #)
    ), 
    
    baselineSeedTable = list(
        class = "table", 
        title = "Define the seeds for Baseline processes with a Seed parameter",
        columnNames = c(
            "ProcessName", 
            "Seed"
        ), 
        variableTypes = c(
            "character",
            "integer"
        ), 
        possibleValues = function(projectPath, BootstrapMethodTable, OutputProcesses) {
            # Get the processes to run:
            processesSansProcessData <- getProcessesSansProcessData(projectPath, modelName = "baseline", startProcess = BootstrapMethodTable$ProcessName, endProcess = OutputProcesses, return.processIndex = TRUE)
            # Scan through the baseline processes to be run and look for processes with the parameter Seed:
            hasSeed <- sapply(processesSansProcessData$functionParameters, function(x) "Seed" %in% names(x))
            
            # Must be an unnamed list:
            possibleValues = list(
                processesSansProcessData$processName[hasSeed], 
                NULL
            )
        }
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
        #possibleValues = function(BootstrapData, BaselineProcess) {
        #    sort(setdiff(names(BootstrapData[[BaselineProcess]]), "BootstrapID"))
        #}, 
        possibleValues = list(), 
        variableTypes <- "character"
    ), 
    
    outputProcesses = list(
        class = "vector", 
        title = "One or more processes to store in BootstrapData", 
        possibleValues = function(projectPath, BootstrapMethodTable) {
            ## Get the reasmpled processes:
            #reasmpledProcesses <- BootstrapMethodTable$ProcessName
            #
            # Get the process table:
            processIndexTable <- readProcessIndexTable(projectPath, modelName = "baseline", startProcess = BootstrapMethodTable$ProcessName, endProcess = Inf)
            
            # Must be an unnamed list:
            possibleValues = as.list(processIndexTable$processName)
        }, 
        variableTypes <- "character"
    )
)

