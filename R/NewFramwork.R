listArgumentFilesWithBasenamesAsNames <- function(path) {
    basenames <- list.files(path)
    out <- as.list(file.path(path, basenames))
    names(out) <- tools::file_path_sans_ext(basenames)
    return(out)
}

listArgumentFiles <- function(dir, processID = NULL) {
    # Get processIDs:
    if(length(processID) == 0) {
        processID <- list.dirs(dir, recursive = FALSE, full.names = FALSE)
    }
    
    # Loop through the processIDs and list the argument files:
    sapply(
        processID, 
        function(x) listArgumentFilesWithBasenamesAsNames(file.path(dir, x)), 
        simplify = FALSE
    )
}

verifyPaths <- function(x) {
    valid <- file.exists(x)
    if(any(!valid)) {
        warning("The following files do not exist: ", paste(x[!valid], collapse = ", "), ".")
    }
    return(x[valid])
}

getArgumentFilesDir <- function(projectPath, modelName, processID) {
    # Get the folder of files holding the memory file paths:
    currentMemoryFolder <- getProjectPaths(projectPath, "currentMemoryFolder")
    # Get and return the directory of one process:
    dir <- file.path(currentMemoryFolder, modelName, processID)
    return(dir)
}


getArgumentFilePaths <- function(projectPath, modelName = NULL, processID = NULL, argumentName = NULL) {
    # Get the folder of files holding the memory file paths:
    currentMemoryFolder <- getProjectPaths(projectPath, "currentMemoryFolder")
    
    if(length(modelName) == 0 && length(processID) == 0 && length(argumentName) == 0) {
        modelName <- list.dirs(currentMemoryFolder, recursive = FALSE, full.names = FALSE)
    }
    
    if(length(modelName) == 1 && length(processID) == 1 && length(argumentName) > 0) {
        # Create a list named by the modelName:
        argumentFilePaths <- structure(
            list(
                # Create a list named by the processID:
                structure(
                    list(
                        # Create a list named by the argumentName:
                        structure(
                            as.list(
                                # Return only the existing files:
                                verifyPaths(
                                    # Build the paths:
                                    file.path(currentMemoryFolder, modelName, processID, paste0(argumentName, ".rds"))
                                )
                            ), 
                            names = argumentName
                        ) 
                    ), 
                    names = processID
                )
            ), 
            names = modelName
        )
    }
    else if(length(modelName) == 1 && length(processID) >= 1 && length(argumentName) == 0) {
        # Create a list named by the modelName:
        dir <- file.path(currentMemoryFolder, modelName)
        argumentFilePaths <- structure(
            list(
                # Loop through the processIDs and list the argument files:
                listArgumentFiles(dir, processID = processID)
            ), 
            names = modelName
        )
    }
    else if(length(modelName) >= 1 && length(processID) == 0 && length(argumentName) == 0) {
        # Create a list named by the modelName:
        dirs <- file.path(currentMemoryFolder, modelName)
        argumentFilePaths <- structure(
            lapply(
                dirs, 
                function(dir) listArgumentFiles(dir, processID = processID)
            ), 
            names = modelName
        )
    }
    else {
        stop("modelName must be given if any of processID and argumentName are given, and processID must be given if argumentName is given. Also when rewuesting more than one modelName or processID, the following parameter must be empty.")
    }
    
    return(argumentFilePaths)
}



#' 
#' @export
#' 
setProcessMemoryNew <- function(projectPath, modelName, processID, argumentName, argumentValue, process = NULL) {
    
    # Get the arguments and argument names from the process:
    if(length(process)) {
        argumentName <- names(process)
        argumentValue <- process
    }
    
    # Save all project arguments to files (shorter repeated to the longest):
    newArgumentFiles <- mapply(
        saveArgumentFile, 
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID, 
        argumentName = argumentName, 
        argumentValue = argumentValue
    )
    
    # Get the paths of the files to which to save the argument files:
    memoryPathFiles <- mapply(
        getMemoryPathFile, 
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID, 
        argumentName = argumentName
    )
    
    # Create the folder holding the files:
    folderPath <- dirname(memoryPathFiles[1])
    dir.create(folderPath, recursive = TRUE, showWarnings = FALSE)
    
    # Write the memory path files:
    mapply(
        saveRDS, 
        newArgumentFiles, 
        file = memoryPathFiles
    )
}


getMemoryPathFile <- function(projectPath, modelName, processID, argumentName) {
    # Get the folder of the current memory:
    currentMemoryFolder <- getProjectPaths(projectPath, "currentMemoryFolder")
    # Build the path to the memory path file:
    memoryPathFile <- file.path(currentMemoryFolder, modelName, processID, paste0(argumentName, ".rds"))
    return(memoryPathFile)
}


#' 
#' @export
#' 
removeProcessMemory_New <- function(projectPath, modelName, processID) {
    # Get the path to the directory of the process:
    dir <- getArgumentFilesDir(projectPath = projectPath, modelName = modelName, processID = processID)
    # Delete the process:
    unlink(dir, recursive = TRUE, force = TRUE)
    
    argumentFilePaths <- getArgumentFilePaths(projectPath, modelName = modelName, processID = processID, argumentName = NULL)
    file
    
    # Get the current table of process argument files:
    argumentFileTable <- getArgumentFileTable(projectPath)
    
    # Remove the process from the argument file table:
    argumentFileTable <- removeFromArgumentFileTable(argumentFileTable, modelName, processID)
    
    # Save the project memory:
    saveProjectMemory(projectPath, argumentFileTable)
}




memoryAddressTableToFiles <- function(projectPath, projectMemoryTable) {
    # Get the folder of files holding the memory file paths:
    currentMemoryFolder <- getProjectPaths(projectPath, "currentMemoryFolder")
    # Get the paths to the files holding the memory file paths:
    filePaths <- file.path(
        currentMemoryFolder, 
        projectMemoryTable$modelname, 
        projectMemoryTable$processID, 
        paste0(projectMemoryTable$argumentName, ".rds"), 
    )
    
    # Write the files with memory file paths:
    lapply(projectMemoryTable$argumentFile, projectMemoryTable, saveRDS)
}

memoryAddressFilesToTable <- function(projectPath, projectMemoryTable) {
    # Get the folder of files holding the memory file paths:
    currentMemoryFolder <- getProjectPaths(projectPath, "currentMemoryFolder")
    
    # Get the paths to the files holding the memory file paths:
    filePaths <- list.files(currentMemoryFolder, full.names = TRUE, recursive = TRUE)
    fileparts <- strsplit(filePaths, "/")
    filepartsRev <- lapply(fileparts, rev)
    modelname <- sapply(filepartsRev, "[", 3)
    processID <- sapply(filepartsRev, "[", 2)
    argumentName <- sapply(filepartsRev, "[", 1)
    argumentName <- sub(".rds", "", argumentName, fixed = TRUE)
    
    # Write the files with memory file paths:
    table <- data.table::data.table(
        argumentFile = filePaths,
        modelName = modelname,
        processID = processID,
        argumentName = argumentName
    )
    
    return(table)
}

memoryAddressTableToFiles <- function(projectPath, projectMemoryTable) {
    # Get the folder of files holding the memory file paths:
    currentMemoryFolder <- getProjectPaths(projectPath, "currentMemoryFolder")
    # Get the paths to the files holding the memory file paths:
    filePaths <- file.path(
        currentMemoryFolder, 
        projectMemoryTable$modelname, 
        projectMemoryTable$processID, 
        paste0(projectMemoryTable$argumentName, ".rds"), 
    )
    
    # Write the files with memory file paths:
    lapply(projectMemoryTable$argumentFile, projectMemoryTable, saveRDS)
}

memoryAddressFilesToTable <- function(projectPath, projectMemoryTable) {
    # Get the folder of files holding the memory file paths:
    currentMemoryFolder <- getProjectPaths(projectPath, "currentMemoryFolder")
    
    # Get the paths to the files holding the memory file paths:
    filePaths <- list.files(currentMemoryFolder, full.names = TRUE, recursive = TRUE)
    fileparts <- strsplit(filePaths, "/")
    filepartsRev <- lapply(fileparts, rev)
    modelname <- sapply(filepartsRev, "[", 3)
    processID <- sapply(filepartsRev, "[", 2)
    argumentName <- sapply(filepartsRev, "[", 1)
    argumentName <- sub(".rds", "", argumentName, fixed = TRUE)
    
    # Write the files with memory file paths:
    table <- data.table::data.table(
        argumentFile = filePaths,
        modelName = modelname,
        processID = processID,
        argumentName = argumentName
    )
    
    return(table)
}




