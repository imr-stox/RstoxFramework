##################################################
##################################################
#' Run a model of a StoX project
#' 
#' This function runs and returns output from a model of a StoX project.
#' 
#' @inheritParams general_arguments
#' @inheritParams getModelData
#' @inheritParams runProcesses
#' @param run Logical: If TRUE run the model.
#' @param save Logical: If TRUE save the project after running.
#' @param force.restart Logical: If TRUE restart the model before running.
#' @param close Logical: If TRUE close the project after running and getting the output.
#' 
#' @return
#' A list of model output.
#' 
#' @export
#' 
runModel <- function(
    projectPath, modelName, 
    processes = NULL, startProcess = 1, endProcess = Inf, 
    drop.datatype = TRUE, unlistDepth2 = FALSE, 
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
                drop.datatype = drop.datatype, 
                warn = FALSE, 
                unlistDepth2 = unlistDepth2
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
#' @inheritParams general_arguments
#' @inheritParams getModelData
#' @inheritParams runProcesses
#' @inheritParams runModel
#' @param unlist.models Logical: If TRUE unlist the top level so that all processes are in one list.
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
    drop.datatype  = TRUE, unlistDepth2 = FALSE, 
    run = TRUE, save = TRUE, force.restart = FALSE, 
    replaceDataList = list(), replaceArgs = list(), 
    fileOutput = NULL, 
    setUseProcessDataToTRUE = TRUE, purge.processData = FALSE, 
    try = TRUE, 
    close = FALSE, 
    unlist.models = TRUE, 
    ...
) {
    
    projectData <- mapply(
        runModel, 
        modelName = modelNames, 
        MoreArgs = list(
            projectPath, 
            startProcess = startProcess, 
            endProcess = endProcess, 
            drop.datatype = drop.datatype, unlistDepth2 = unlistDepth2, 
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
    if(unlist.models) {
        projectData <- unlist(unname(projectData), recursive = FALSE)
    }
    
    
    return(projectData)
}


##################################################
##################################################
#' Run all models of a StoX project
#' 
#' This function runs and returns output from all models of a StoX project.
#' 
#' @inheritParams general_arguments
#' @inheritParams getModelData
#' @inheritParams runProcesses
#' @inheritParams runModel
#' @inheritParams runProject
#' @param projectPaths The paths to the projects to run.
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
    drop.datatype = TRUE, unlistDepth2 = FALSE, 
    run = TRUE, save = TRUE, force.restart = FALSE, 
    replaceDataList = list(), replaceArgs = list(), 
    fileOutput = NULL, 
    setUseProcessDataToTRUE = TRUE, purge.processData = FALSE, 
    try = TRUE, 
    unlist.models = FALSE, 
    close = FALSE, 
    ...
) {
    
    # Run all projects:
    output <- sapply(
        projectPaths, 
        runProject, 
        modelNames = modelNames, 
        processes = processes, startProcess = startProcess, endProcess = endProcess, 
        drop.datatype = drop.datatype, unlistDepth2 = unlistDepth2, 
        run = run, save = save, force.restart = force.restart, 
        replaceDataList = replaceDataList, replaceArgs = replaceArgs, 
        fileOutput = fileOutput, 
        setUseProcessDataToTRUE = setUseProcessDataToTRUE, purge.processData = purge.processData, 
        try = try, 
        unlist.models = unlist.models, 
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




##################################################
##################################################
#' Read the output files of a project
#' 
#' This function reads all or some of the output files of a project, indicated by model and proccess names.
#' 
#' @inheritParams general_arguments
#' @inheritParams runProject
#' @param verifyFiles Logical: If TRUE verify that the files are from processes that exist in the project.
#' 
#' @return
#' A list of model output.
#' 
#' @export
#'  
readModelData <- function(projectPath, modelName = NULL, processName = NULL, verifyFiles = FALSE, unlist.models = FALSE) {
    # List the files of the project:
    if(isProject(projectPath)) {
        outputFolders <- getProjectPaths(projectPath)$outputFolders
        names(outputFolders) <- basename(outputFolders)
        outputFiles <- lapply(outputFolders, listOutputfiles)
        # Place the file paths into a recursive list:
        
        
        if(verifyFiles) {
            projectDescription <- readProjectDescription(projectPath, applyBackwardCompatibility = FALSE, formatProcesses = FALSE)$projectDescription
            
            processNames <- lapply(projectDescription, function(x) unname(sapply(x, "[[", "processName")))
            outputFolderNames <- lapply(outputFiles, names)
            
            invalidProcesses <- setdiff(
                unlistSep(outputFolderNames), 
                unlistSep(processNames)
            )
            
            if(any(invalidProcesses)) {
                warning("The following folders of the output are not present as processes in the project description file, and were discarded from the output:", paste(invalidProcesses, collapse = ", "))
                outputFiles <- lapply(outputFiles, function(x) x[!names(x) %in% basename(invalidProcesses)])
            }
        }
        
        # Read the files using the apropriate function:
        #output <- lapply(outputFiles, function(x) lapply(x, readStoxOutputFile))
        output <- rapply(outputFiles, readStoxOutputFiles, how = "replace")
        
        # Drop the list over models:
        if(unlist.models) {
            output <- unlist(unname(output), recursive = FALSE)
        }
        
        return(output)
    }
    else {
        stop("The projectPath ", projectPath, " is not a StoX project.")
    }
    
}


readStoxOutputFiles <- function(paths) {
    output <- structure(lapply(paths, readStoxOutputFile), names = basename(tools::file_path_sans_ext(paths)))
    # Unlist the top level of an RData file, as an RData file is a joint file of several outputs, and we do not want the extra BootstrapData level on top of this list:
    areRDataFiles <- tolower(tools::file_ext(paths)) == "rdata"
    # isTRUE is TRUE only for one TRUE:
    if(isTRUE(areRDataFiles)) {
        output <- output[[1]]
    }
    return(output)
}


readStoxOutputFile <- function(path) {
    # This function only read one file:
    if(length(path) != 1) {
        stop("Exactly one file required")
    }
    
    # Get file extension:
    ext <- tools::file_ext(path)
    
    if(tolower(ext) == "rdata") {
        output <- readOutputRDataFile(path)
    }
    else if(tolower(ext) %in% c("json", "geojson")) {
        output <- reasdGeoJSON(path)
    }
    else if(tolower(ext) == "txt") {
        output <- data.table::fread(path, na.strings = c("NA", ""), tz = "UTC")
        # If here are any keys that are time (such as LogKey of the StoxAcoustic format), convert these to character with 3 digits:
        areKeys <- endsWith(names(output), "Key")
        areDateTime <- sapply(output, firstClass) %in% "POSIXct"
        toConvertToCharacter <- areKeys & areDateTime
        if(any(toConvertToCharacter)) {
            for(col in names(output)[toConvertToCharacter]) {
                output[, (col) := format(get(col), format = "%Y-%m-%dT%H:%M:%OS3Z")]
            }
        }
    }
    else if(tolower(ext) == "nc") {
        stop("NetCDF4 file not yet implemented.")
    }
    
    return(output)
}

#POSIXctToCharacter <- function(x, digits = 3) {
#    browser()
#    formatString <- paste0("%Y-%m-%dT%H:%M:%OS", digits, "Z")
#    print(formatString)
#    format(x, format = formatString)
#}

readOutputRDataFile <- function(outputDataPath) {
    if(file.exists(outputDataPath)) {
        outputData <- tryCatch(
            get(load(outputDataPath)), 
            error = function(err) NULL
        )
    }
    else {
        outputData <- NULL
    }
    
    return(outputData)
}



listOutputfiles <- function(modelPath) {
    dirs <- list.dirs(modelPath, recursive = FALSE)
    structure(lapply(dirs, list.files, full.names = TRUE), names = basename(dirs))
}


unlistSep <- function(x, sep = "/") {
    paste(rep(names(processNames), lengths(processNames)), unlist(processNames), sep = sep)
}




#' General functions to run a function of an Rstox package, modeled by do.call().
#' 
#' \code{runFunction} runs a function using the \code{\link[base]{do.call}} syntax, whereas \code{runFunction.JSON} accepts a JSON string conntaining the parameters to pass on to \code{runFunction}.
#' 
#' @inheritParams base::do.call
#' @param package The name of the package holding the function given by \code{what}.
#' @param removeCall Logical: If FALSE, keep the call in the error message.
#' @param onlyStoxMessages Logical: If TRUE show only the StoX messages, which are those starting with "StoX: ".
#' @param cmd A JSON string containing parameters listed above.
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
#' @export
#' @rdname runFunction
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