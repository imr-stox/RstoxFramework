##################################################
##################################################
#' Run a model of a StoX project
#' 
#' This function runs and returns output from a model of a StoX project.
#' 
#' @param projectPath       The path to the StoX project, i.e., the folder of the project with the sub folders "input", "output" and "process". Can possibly be the path to a file inside the project folder.
#' @param modelName         xx
#' @param startProcess      xx
#' @param endProcess        Integer value speicfying the 
#' @param run               Logical: If TRUE run the model.
#' @param save              Logical: If TRUE save the project after running.
#' @param force.restart     Logical: If TRUE restart the model before running.
#' 
#' @return
#' A list of model output.
#' 
#' 
#' @export
#' 
runModel <- function(projectPath, modelName, startProcess = 1, endProcess = Inf, run = TRUE, save = TRUE, force.restart = FALSE, replaceDataList = list(), replaceArgs = list(), fileOutput = NULL, setUseProcessDataToTRUE = TRUE, purge.processData = FALSE, ...) {
    # Run the model if required:
    modelData <- NULL
    if(run) {
        if(isProject(projectPath)) {
            # Open the project if not open:
            if(!isOpenProject(projectPath)) {
                # No need for GUI here as this function should not be used by any GUI, but is merely a converience function replacing a GUI:
                openProject(projectPath)
            }
            # Run the model:
            runProcesses(
                projectPath = projectPath, 
                modelName = modelName, 
                startProcess = startProcess, 
                endProcess = endProcess, 
                save = save, 
                force.restart = force.restart, 
                replaceDataList = replaceDataList, 
                replaceArgs = replaceArgs, 
                fileOutput = fileOutput, 
                setUseProcessDataToTRUE = setUseProcessDataToTRUE, 
                purge.processData = purge.processData, 
                ...
            )
            # Get the model data:
            modelData <- getModelData(
                projectPath = projectPath, 
                modelName = modelName, 
                startProcess = startProcess, 
                endProcess = endProcess
            )
        }
    }
    
    return(modelData)
}


#' 
#' @export
#' 
runFunction <- function(what, args, package = "RstoxFramework", removeCall = TRUE, onlyStoxMessages = TRUE) {
    
    # Parse the args if given as a JSON string:
    args <- parseParameter(args)
    
    # Reset the warnings:
    assign("last.warning", NULL, envir = baseenv())
    
    # Run the function 'what' and store the warnings and error along with the result:
    warn <- character(0)
    err <- NULL
    msg <- capture.output({
        value <- withCallingHandlers(
            tryCatch(
                #do.call(what, args), 
                do.call(getExportedValue(package, what), args), 
                error = function(e) {
                    err <<- if(removeCall) conditionMessage(e) else e
                    NULL
                }
            ), 
            warning=function(w) {
                warn <<- append(warn, if(removeCall) conditionMessage(w) else w)
                invokeRestart("muffleWarning")
            }
        )
    }, type = "message")
    
    if(length(warn)) {
        warn <- as.character(warn)
    }
    if(length(err)) {
        err <- as.character(err)
    }
    if(onlyStoxMessages) {
        msg <- trimws(sub("StoX:", "", msg[startsWith(msg, "StoX:")], fixed = TRUE))
    }
    
    
    # Clean the warnings:
    #warn <- unname(unlist(warn[names(warn) == "message"]))
    
    # Return a list of warnings and error along with the result:
    list(
        #value = if(!is.list(value)) as.list(value) else value, 
        value = value, 
        message = as.list(msg), 
        warning = as.list(warn), 
        error = as.list(err)
    )
}

#' This function may be used in the GUI.
#' 
#' @export
#' 
runFunction.JSON <- function(cmd){
    # Service command/response handler
    tryCatch({
        cmdj <- jsonlite::fromJSON(cmd)
        res <- runFunction(cmdj$what, cmdj$args, cmdj$package)
        r <- jsonlite::toJSON(res, pretty=T, auto_unbox=T, na='string')
        r
    }, warning = function(warning_condition) {
        'warning'
    }, error = function(error_condition) {
        'error'
    })
}



### ##################################################
### ##################################################
### #' Get RstoxFramework version
### #' 
### #' This function declares the RstoxFramework environment and writes vital definitions to it.
### #' 
### #' @return
### #' A list of definitions.
### #' 
### #' @noRd
### #' @seealso Use \code{\link{getRstoxFrameworkDefinitions}} to get the definitions.
### #' 
### #' 
### #' @export
### #'
### getRstoxFrameworkVersion <- function() {
###     list(
###         packageName = "RstoxFramework", 
###         version = getPackageVersion("RstoxFramework", only.version = TRUE),
###         official = all(getOfficialRstoxPackageVersion()$AreOfficialRstoxPackageVersion)
###     )
### }


##################################################
##################################################
#' Get a list of Rstox pakcage versions
#' 
#' This function declares the RstoxFramework environment and writes vital definitions to it.
#' 
#' @return
#' A list of definitions.
#' 
#' @noRd
#' @seealso Use \code{\link{getRstoxFrameworkDefinitions}} to get the definitions.
#' 
#' 
#' @export
#'
getRstoxPackageVersions <- function() {
    getOfficialRstoxPackageVersion()$InstalledRstoxPackageVersion
}


##################################################
##################################################
#' Export StoX JSON schema
#' 
#' @param con A connection to which to write the schema. Returned as JSON if missing.
#' 
#' @export
#' 
writeStoxJsonSchema <- function(con) {
    schema <- getRstoxFrameworkDefinitions("schema")
    if(missing(con)) {
        return(schema)
    }
    else {
        writeLines(as.character(schema), con)
    }
}