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
runModel <- function(
    projectPath, modelName, 
    processes = NULL, startProcess = 1, endProcess = Inf, 
    run = TRUE, save = TRUE, force.restart = FALSE, 
    replaceDataList = list(), replaceArgs = list(), 
    fileOutput = NULL, 
    setUseProcessDataToTRUE = TRUE, purge.processData = FALSE, 
    try = TRUE, 
    close = FALSE, 
    ...
) {
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
                try = try, 
                ...
            )
            # Get the model data:
            modelData <- getModelData(
                projectPath = projectPath, 
                modelName = modelName, 
                processes = processes, 
                startProcess = startProcess, 
                endProcess = endProcess, 
                warn = FALSE
            )
        }
        else{
            warning("The path ", projectPath, " does not point to a valid StoX project.")
        }
        
        # Close after running if requested:
        if(close) {
            closeProject(projectPath)
        }
    }
    
    return(modelData)
}



##################################################
##################################################
#' Run all models of a StoX project
#' 
#' This function runs and returns output from all models of a StoX project.
#' 
#' @inheritParams runModel
#' 
#' @return
#' A list of model output.
#' 
#' @export
#' 
runProject <- function(
    projectPath, 
    modelNames = getRstoxFrameworkDefinitions("stoxModelNames"), 
    processes = NULL, startProcess = 1, endProcess = Inf, 
    run = TRUE, save = TRUE, force.restart = FALSE, 
    replaceDataList = list(), replaceArgs = list(), 
    fileOutput = NULL, 
    setUseProcessDataToTRUE = TRUE, purge.processData = FALSE, 
    try = TRUE, drop = TRUE, 
    close = FALSE, 
    ...
) {
    
    projectData <- mapply(
        runModel, 
        modelName = modelNames, 
        MoreArgs = list(
            projectPath, 
            startProcess = startProcess, 
            endProcess = endProcess, 
            run = run, 
            save = save, 
            force.restart = force.restart, 
            replaceDataList = replaceDataList, 
            replaceArgs = replaceArgs, 
            fileOutput = fileOutput, 
            processes = processes, 
            setUseProcessDataToTRUE = setUseProcessDataToTRUE, 
            purge.processData = purge.processData, 
            try = try, 
            close = FALSE, 
            ...
        ), 
        SIMPLIFY = FALSE
    )
    # Close after running if requested:
    if(close) {
        closeProject(projectPath)
    }
    
    # Drop the list over models:
    projectData <- unlist(unname(projectData), recursive = !drop)
    
    return(projectData)
}


##################################################
##################################################
#' Run all models of a StoX project
#' 
#' This function runs and returns output from all models of a StoX project.
#' 
#' @inheritParams runModel
#' 
#' @return
#' A list of model output.
#' 
#' @export
#' 
runProjects <- function(
    projectPaths, 
    modelNames = getRstoxFrameworkDefinitions("stoxModelNames"), 
    processes = NULL, startProcess = 1, endProcess = Inf, 
    run = TRUE, save = TRUE, force.restart = FALSE, 
    replaceDataList = list(), replaceArgs = list(), 
    fileOutput = NULL, 
    setUseProcessDataToTRUE = TRUE, purge.processData = FALSE, 
    try = TRUE, drop = TRUE, 
    close = FALSE, 
    ...
) {
    
    # Run all projects:
    output <- sapply(
        projectPaths, 
        runProject, 
        modelNames = modelNames, 
        processes = processes, startProcess = startProcess, endProcess = endProcess, 
        run = run, save = save, force.restart = force.restart, 
        replaceDataList = replaceDataList, replaceArgs = replaceArgs, 
        fileOutput = fileOutput, 
        setUseProcessDataToTRUE = setUseProcessDataToTRUE, purge.processData = purge.processData, 
        try = try, drop = drop, 
        close = close, 
        ..., 
        simplify = FALSE
    )
    
    output <- rbindListRecursive(output)
    
    
    return(output)
}

rbindListRecursive <- function(x) {
    
    x <- unlist(unname(x), recursive = FALSE)
    
    # Scan through the list and identify data.table(s):
    namesList <- namesRecursive(x)
    
    # Loop through the tables and rbind:
    output <- list()
    for(namesVector in namesList) {
        # Declare the list element:
        output <- createNestedListElement(output, namesVector)
        # Insert the rbinded table:
        output[[namesVector]] <- data.table::rbindlist(extractFromAllProcessOutputs(namesVector, x), fill = TRUE)
    }
        
    return(output)
}

extractFromAllProcessOutputs <- function(nameVector, x) {
    
    
    if(length(nameVector) == 1) {
        output <- x[names(x) == nameVector]
    }
    else if(length(nameVector) == 2) {
        output <- lapply(x[names(x) == nameVector[1]], "[[", nameVector[2])
    }
    else if(length(nameVector) == 3) {
        output <- lapply(x[names(x) == nameVector[1]], "[[", nameVector[2])
        output <- lapply(output, "[[", nameVector[3])
    }
    else {
        stop("Process outputs can only have three levels")
    }
    
    # Remove the empty ones:
    output <- output[lengths(output) > 0]
    
    return(output)
}

pasteNamesRecursive <- function (L, sep = "/") {
    # Return the names if all elements are not lists, and recurse futher if any are lists:
    areNotList <- sapply(L, inherits, c("data.table", "data.frame"))
    areList <- !areNotList
    
    if(any(areList)) {
        return(
            c(
                names(L)[areNotList], 
                mapply(paste,  names(L)[areList], sapply(L[areList], pasteNamesRecursive), sep = sep)
            )
        )
    }
    
    else {
        return(names(L)[areNotList])
    }
}
# Function to get the names of a list recursively:
namesRecursive <- function (L, uniquify = TRUE) {
    # Get the names pasted by "/"
    namesRecursivePasted <- unlist(pasteNamesRecursive(L, sep = "/"))
    
    # Split by "/":
    namesRecursiveSplit <- strsplit(namesRecursivePasted, "/")
    
    # Uniquify:
    if(uniquify) {
        namesRecursiveSplit <- unique(namesRecursiveSplit)
    }
    
    return(namesRecursiveSplit)
}

createNestedListElement <- function(x, namesVector) {
    if(!length(namesVector)) {
        return(x)
    }
    else if(length(namesVector) == 1) {
        if(!length(x[[namesVector]]) && !is.list(x[[namesVector]])) {
            x[[namesVector]] <- list()
        }
    }
    else {
        for(ind in seq_along(namesVector)) {
            thisNamesVector <- namesVector[seq_len(ind)]
            if(!length(x[[thisNamesVector]]) && !is.list(x[[thisNamesVector]])) {
                x[[thisNamesVector]] <- list()
            }
        }
    }
    return(x)
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