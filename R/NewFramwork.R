# The RstoxFramework memory file system works as follows:
#
# 1. All memory is saved, and only deleted if the project is closed. This involves the six process arguments "functionName", "processName", "processParameters", "processData", "functionInputs" and "functionParameters". A folder structure is created for each model, holding one folder for each process, which in turn contains one folder for each of the process arguments. These folders store the actual memory, and one new file is added every time there is a change in a particular process argument. These files are named argument files.
#
# 2. The state of a project is saved by pointer files holding the paths to the argument files comprising the project memory. These pointer files are saved in a folder structure similar to the argument files, with one folder per model, holding the folders of each process, which contains 6 pointer files "functionName.rds", "processName.rds", "processParameters.rds", "processData.rds", "functionInputs.rds" and "functionParameters.rds". When a process argument is modified, a new argument file is created and the pointer file is updated with the path to the new argument file. If a process is removed, the corresponding folder of pointer files is deleted.
#
# 3. Every time an argument file 







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
    
    # Save all the argument files (shorter repeated to the longest). Usually this is just one file, but when a project is created the archiving of memory takes one process at the time:
    newArgumentFiles <- mapply(
        saveArgumentFile, 
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID, 
        argumentName = argumentName, 
        argumentValue = argumentValue
    )
    
    # Save all the pointer files:
    mapply(
        savePointerFile, 
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID, 
        argumentName = argumentName, 
        argumentFilePath = newArgumentFiles
    )
    
                # Set the status as not saved (saving is done when running a process):
                setSavedStatus(projectPath, status = FALSE)
                
                # Save the project memory:
                saveProjectMemoryNew(projectPath, argumentFileTable)
}



# Function for saving an argument file table (defining the process memory files comprising the process memory):
archiveProject <- function(projectPath) {
    
    # Save the list of project argument files to the current project description file and to the new project description file:
    currentProjectMemoryFile <- getCurrentProjectMemoryFile(projectPath)
    newProjectMemoryFile     <- getNewProjectMemoryFile(projectPath)
    # Add the processIndexTable, the activeProcessID and the maxProcessIntegerID to the data to write:
    fullProjectMemory <- list(
        argumentFileTable = argumentFileTable, 
        processIndexTable = readProcessIndexTable(projectPath),  
        activeProcessIDTable = getActiveProcess(projectPath), 
        maxProcessIntegerIDTable = getMaxProcessIntegerID(projectPath)
    )
    # Write the project memory to the new file:
    saveRDS(fullProjectMemory, file = newProjectMemoryFile)
    
    # Write the project memory for the individual processes:
    saveRDS(fullProjectMemory, file = currentProjectMemoryFile)
    # Also write the current project memory as a folder structure of individual files with path to the memory file:
    
    # Update the projectDescriptionIndexFile:
    projectMemoryIndex <- readProjectMemoryIndex(projectPath)
    
    # Delete any files with positive index:
    hasPositiveIndex <- projectMemoryIndex$Index > 0
    if(any(hasPositiveIndex)) {
        #unlink(projectMemoryIndex$Path[hasPositiveIndex])
        deleteProjectMemoryFile(projectPath, projectMemoryIndex$Path[hasPositiveIndex])
        projectMemoryIndex <- projectMemoryIndex[!hasPositiveIndex, ]
    }
    # Subtract 1 from the indices, and add the new project description relative file path:
    newProjectMemoryFile_relativePath <- sub(projectPath, "", newProjectMemoryFile)
    projectMemoryIndex$Index <- projectMemoryIndex$Index - 1
    projectMemoryIndex <- rbind(
        projectMemoryIndex, 
        data.table::data.table(
            Index = 0, 
            Path = newProjectMemoryFile_relativePath
        ), 
        fill = TRUE
    )
    # Write the projectDescriptionIndex to file:
    writeProjectMemoryIndex(projectPath, projectMemoryIndex)
    
    # Return the new project description file path:
    newProjectMemoryFile
}





##### NEW: #####
# Function to save a single pointer file:
savePointerFile <- function(projectPath, modelName, processID, argumentName, argumentFilePath) {
    # Get the path to the pointer file:
    pointerFile <- getPointerFile(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID, 
        argumentName = argumentName 
    )
    
    # Create the folder holding the files if missing:
    folderPath <- dirname(pointerFile)
    dir.create(folderPath, recursive = TRUE, showWarnings = FALSE)
    
    # Save the pointer file:
    saveRDS(argumentFilePath, pointerFile)
}

##### NEW: #####
# Function to get the path to a single pointer file:
getPointerFile <- function(projectPath, modelName, processID, argumentName) {
    # Get the folder of the current memory:
    currentMemoryFolder <- getProjectPaths(projectPath, "currentMemoryFolder")
    # Build the path to the memory path file:
    pointerFile <- file.path(currentMemoryFolder, modelName, processID, paste0(argumentName, ".rds"))
    return(pointerFile)
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




