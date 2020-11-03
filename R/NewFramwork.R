# The RstoxFramework memory file system works as follows:
#
# 1. All memory is saved, and only deleted if the project is closed. This involves the six process arguments "functionName", "processName", "processParameters", "processData", "functionInputs" and "functionParameters". A folder structure is created for each model, holding one folder for each process, which in turn contains one folder for each of the process arguments. These folders store the actual memory, and one new file is added every time there is a change in a particular process argument. These files are named argument files.
#
# 2. The state of a project is saved by pointer files holding the paths to the argument files comprising the project memory. These pointer files are saved in a folder structure similar to the argument files, with one folder per model, holding the folders of each process, which contains 6 pointer files "functionName.rds", "processName.rds", "processParameters.rds", "processData.rds", "functionInputs.rds" and "functionParameters.rds". When a process argument is modified, a new argument file is created and the pointer file is updated with the path to the new argument file. If a process is removed, the corresponding folder of pointer files is deleted.
#
# 3. Every time an argument file 







listArgumentFilesWithBasenamesAsNames <- function(path) {
    basenames <- list.files(path, full.names = FALSE)
    out <- as.list(file.path(path, basenames))
    names(out) <- tools::file_path_sans_ext(basenames)
    return(out)
}




listArgumentFiles <- function(projectPath, modelName, processID = NULL) {
    
    # Get the folder of files holding the memory file paths:
    memoryCurrentModelsFolder <- getProjectPaths(projectPath, "memoryCurrentModelsFolder")
    
    # Get the path to the directory of the model:
    dir <- file.path(memoryCurrentModelsFolder, modelName)
    
    # Get processIDs:
    if(length(processID) == 0) {
        # Get the processIndexTable, holding the order of the processes:
        processIndexTable <- readProcessIndexTable(projectPath, modelName)
        # Get all processIDs from the processIndexTable:
        processID <- processIndexTable$processID
    }
    
    # Loop through the processIDs and list the argument files:
    sapply(
        processID, 
        function(x) listArgumentFilesWithBasenamesAsNames(file.path(dir, x)), 
        simplify = FALSE
    )
}




getArgumentFilesDir <- function(projectPath, modelName, processID) {
    # Get the folder of files holding the memory file paths:
    memoryCurrentModelsFolder <- getProjectPaths(projectPath, "memoryCurrentModelsFolder")
    # Get and return the directory of one process:
    dir <- file.path(memoryCurrentModelsFolder, modelName, processID)
    return(dir)
}


getArgumentFilePaths <- function(projectPath, modelName = NULL, processID = NULL, argumentName = NULL) {
    
    # Get the folder of files holding the memory file paths:
    memoryCurrentModelsFolder <- getProjectPaths(projectPath, "memoryCurrentModelsFolder")
    
    # The default is to get all models:
    if(length(modelName) == 0 && length(processID) == 0 && length(argumentName) == 0) {
        #modelName <- list.dirs(memoryCurrentModelsFolder, recursive = FALSE, full.names = FALSE)
        modelName <- getRstoxFrameworkDefinitions("stoxModelNames")
    }
    
    # If no models were detected, return an empty list:
    if(length(modelName) == 0 && length(processID) == 0 && length(argumentName) == 0) {
        return(list())
    }
    
    if(length(modelName) == 1 && length(processID) == 1 && length(argumentName) > 0) {
        # Create a list named by the modelName:
        pointerFilePaths <- structure(
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
                                    file.path(memoryCurrentModelsFolder, modelName, processID, paste0(argumentName, ".rds"))
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
        pointerFilePaths <- structure(
            list(
                listArgumentFiles(projectPath = projectPath, modelName = modelName, processID = processID)
            ), 
            names = modelName
        )
    }
    else if(length(modelName) >= 1 && length(processID) == 0 && length(argumentName) == 0) {
        # Create a list named by the modelName:
        pointerFilePaths <- structure(
            lapply(
                modelName,
                function(thisModelName) listArgumentFiles(projectPath = projectPath, modelName = thisModelName)
            ), 
            names = modelName
        )
    }
    else {
        stop("StoX: modelName must be given if any of processID and argumentName are given, and processID must be given if argumentName is given. Also when requesting more than one modelName or processID, the following parameter must be empty.")
    }
    
    # Read the pointer files:
    argumentFilePaths <- rapply(pointerFilePaths, readPointerFile, projectPath = projectPath, how = "replace")
    
    
    return(argumentFilePaths)
}






#' 
#' @export
#'
getProjectMemoryData <- function(projectPath, modelName = NULL, processID = NULL, argumentName = NULL, drop1 = FALSE, argumentFilePaths = NULL, named.list = TRUE) {
    
    # Get the argument files:
    if(length(argumentFilePaths) > 0 && length(modelName) == 1 && length(processID) == 1 && length(argumentName) == 1 ) {
        # Select the requested argument file path(s):
        argumentFilePaths <- argumentFilePaths[modelName]
        argumentFilePaths[[modelName]] <- argumentFilePaths[[modelName]][processID]
        argumentFilePaths[[modelName]][[processID]] <- argumentFilePaths[[modelName]][[processID]][argumentName]
    }
    else {
        argumentFilePaths <- getArgumentFilePaths(projectPath, modelName = modelName, processID = processID, argumentName = argumentName)
    }
    
    # Read the memory files:
    output <- rapply(argumentFilePaths, readRDS, how = "replace")
    
    # Drop the levels with only one element if requested:
    if(drop1) {
        if(length(modelName) == 1) {
            output <- output[[modelName]]
        }
        if(length(processID) == 1) {
            output <- output[[processID]]
        }
        if(length(argumentName) == 1) {
            output <- output[[argumentName]]
        }
    }
    
    if(!named.list) {
        # Remove the processIDs:
        for(modelName in names(output)) {
            output[[modelName]] <- unname(output[[modelName]])
        }
        # Put the models in an anonymous list, and add the modelName
        for(modelName in names(output)) {
            output[[modelName]]$modelName <- modelName
        }
    }
    
    return(output)
}


# Read the pointer files:
readPointerFile <- function(pointerFile, projectPath) {
    file.path(projectPath, readRDS(pointerFile))
}





#' 
#' @export
#' 
setProcessMemory <- function(projectPath, modelName, processID, argumentName, argumentValue, process = NULL, archive = TRUE) {
    
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
    if(archive) {
        archiveProject(projectPath)
    }
}



# Function for saving an argument file table (defining the process memory files comprising the process memory):
archiveProject <- function(projectPath) {
    
    # 1. Collect the current memory and other files into one list, and write this as a project memory file:
    fullProjectMemory <- list(
        pointerFilesTable = getPointerFilesTable(projectPath), 
        processIndexTable = readProcessIndexTable(projectPath),  
        activeProcessIDTable = getActiveProcess(projectPath), 
        maxProcessIntegerIDTable = getMaxProcessIntegerID(projectPath)
    )
    
    # Write the project memory to the new file, which is a rds file (since this is a list of R objects, which cannot be written as a columnar format such as fst or feather):
    newProjectMemoryFileSansExt <- getNewProjectMemoryFileSansExt(projectPath)
    newProjectMemoryFile <- writeMemoryFile(
        fullProjectMemory, 
        filePathSansExt = newProjectMemoryFileSansExt, 
        ext = "rds")
    
    
    # 2. Update the projectMemoryIndex file:
    # Read the list of process memory files:
    projectMemoryIndex <- readProjectMemoryIndex(projectPath)
    
    # Delete any states with positive index:
    hasPositiveIndex <- projectMemoryIndex$Index > 0
    if(any(hasPositiveIndex)) {
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
    return(newProjectMemoryFile)
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
    memoryCurrentModelsFolder <- getProjectPaths(projectPath, "memoryCurrentModelsFolder")
    # Build the path to the memory path file:
    pointerFile <- file.path(memoryCurrentModelsFolder, modelName, processID, paste0(argumentName, ".rds"))
    return(pointerFile)
}


#' 
#' @export
#' 
removeProcessMemory <- function(projectPath, modelName, processID) {
    # Get the path to the directory of the process:
    dir <- getArgumentFilesDir(projectPath = projectPath, modelName = modelName, processID = processID)
    # Delete the process:
    unlink(dir, recursive = TRUE, force = TRUE)
    
    # Archive the project:
    archiveProject(projectPath)
}




savePointerFilesTableAsPointerFiles <- function(projectPath, pointerFilesTable) {
    # Get the folder of files holding the memory file paths:
    memoryCurrentModelsFolder <- getProjectPaths(projectPath, "memoryCurrentModelsFolder")
    # Get the paths to the files holding the memory file paths:
    filePaths <- file.path(
        memoryCurrentModelsFolder, 
        pointerFilesTable$modelname, 
        pointerFilesTable$processID, 
        paste0(pointerFilesTable$argumentName, ".rds"), 
    )
    
    # Write the files with memory file paths:
    lapply(pointerFilesTable$argumentFile, pointerFilesTable, saveRDS)
}

getPointerFilesTable <- function(projectPath) {
    
    # Get the folder of files holding the memory file paths:
    memoryCurrentModelsFolder <- getProjectPaths(projectPath, "memoryCurrentModelsFolder")
    
    # Get the paths to the files holding the memory file paths:
    filePaths <- list.files(memoryCurrentModelsFolder, full.names = TRUE, recursive = TRUE)
    fileparts <- strsplit(filePaths, "/")
    filepartsRev <- lapply(fileparts, rev)
    modelname <- sapply(filepartsRev, "[", 3)
    processID <- sapply(filepartsRev, "[", 2)
    argumentName <- sapply(filepartsRev, "[", 1)
    argumentName <- sub(".rds", "", argumentName, fixed = TRUE)
    
    # Write the files with memory file paths:
    pointerFilesTable <- data.table::data.table(
        argumentFile = filePaths,
        modelName = modelname,
        processID = processID,
        argumentName = argumentName
    )
    
    return(pointerFilesTable)
}
