# We use CamelCase for StoX functions and the parameters shown in StoX, for data types, models, 
# We use camelCase for everything else.

# Should we use "~" for the workspace folder? Tilde gives the Documents folder on Windows, whereas StoX 2.7 uses the user folder.
#
# The data types for communication with StoX: Boolean, String, Numeric, NULL, List of predefined values, FileName, FileNames
# 
# There is a hierarchy FuncitonName - functionParameters - functionInputs. This needs to be taken into account in the categories and items
# 
# What to do with boolean, integer, numeric, string?
#
# - We will use functionInputs and functionParameters.
#
# - We skip the test Tobis project created by StoX (assuming this is never used)
# 
# 
# 
# 
# 
# 
# Things to do in RstoxFramework:
#
# StoX
# - Define the stox folder in the user home directory, since this avoids any global setting of StoX
#
# Project
# - Create project
# - Open project
# - Save project
# - SaveAs project: Creates a new project as a copy of the current status of the old project, and # keeps the old project open, but shifts focus to the new.
# - Close project (1. Ask the user)
# - Reset project
#
# Process
# 
# 
# 
# # Functions for an isolated project object:
# createEmptyBaselineProcess
# 
# modifyBaselineProcessName
# modifyBaselineFunctionName
#     modifyBaselineFunctionInputs
#     modifyBaselineFunctionParameters
# modifyBaselineProcessParameters
# modifyBaselineProcessData



##################################################
##################################################
#' Definitions stored in the RstoxFramework environment
#' 
#' This function declares the RstoxFramework environment and writes vital definitions to it.
#' 
#' @return
#' A list of definitions.
#' 
#' @noRd
#' @seealso Use \code{\link{getRstoxFrameworkDefinitions}} to get the definitions.
#' 
initiateRstoxFramework <- function(){
    
    #### Fundamental settings of StoX: ####
    # Define the valid output data classes:
    validOutputDataClasses <- c(
        "data.table", 
        "json", 
        "geojson"
    )
    
    stoxFolders <- c(
        Input = "Input", 
        Output = "Output", 
        Process = "Process"
    )
    stoxDataSources <- c(
        Acoustic = "Acoustic", 
        Biotic = "Biotic", 
        Landing = "Landing"
    )
    stoxModelNames <- c(
        Baseline = "Baseline", 
        Statistics = "Statistics", 
        Report = "Report"
    )
    stoxModelDisplayNames <- c(
        Baseline = "Baseline", 
        Statistics = "Statistics", 
        Report = "Report"
    )
    stoxModelDescriptions <- c(
        Baseline = "Baseline: The estimation model", 
        Statistics = "Statistics: Processes that run Baseline to generate statistcs such as estimates of variation", 
        Report = "Report: Processes that run Baseline or Statistics processes to generate reports"
    )
    stoxModelInfo <- data.frame(
        modelName = stoxModelNames, 
        displayName = stoxModelDisplayNames, 
        description = stoxModelDescriptions
    )
    
    # Define data types which can be plotted in the map (includes also changing colour etc, such as assigned stations of an acoustic PSU):
    dataTypesToShowInMap <- c(
        StoxBioticData = "StoxBioticData", 
        StoxAcousticData = "StoxAcousticData", 
        StratumPolygon = "StratumPolygon", 
        Assignment = "Assignment", 
        AcousticPSU = "AcousticPSU", 
        SweptAreaPSU = "SweptAreaPSU"
    )
    
    # Define the process parameters with default values:
    processParameters <- list(
        enabled = TRUE, 
        showInMap = FALSE, 
        fileOutput = TRUE
    )
    # Process arguments:
    processDefaultFull <- list(
        processName = NULL, 
        functionName = NULL, 
        processParameters = processParameters,
        processData = list(), 
        functionParameters = list(), 
        functionInputs = list()
    )
    processDefaultSansProcessData <- processDefaultFull[names(processDefaultFull) != "processData"]
    processDefault <- list(
        Baseline = processDefaultFull, 
        Statistics = processDefaultSansProcessData, 
        Report = processDefaultSansProcessData
    )
    
    #### Define the folder structure of StoX: ####
    stoxFolderStructure <- list(
        stoxDataSources, 
        stoxModelNames, 
        ""
    )
    #names(stoxFolderStructure) <- stoxFolders
    stoxFolderStructureNames <- unname(unlist(mapply(paste, stoxFolders, stoxFolderStructure, sep = "_")))
    stoxFolderStructure <- unname(unlist(mapply(file.path, stoxFolders, stoxFolderStructure)))
    stoxFolderStructure <- gsub('\\/$', '', stoxFolderStructure)
    names(stoxFolderStructure) <- stoxFolderStructureNames
    stoxFolderStructureList <- as.list(stoxFolderStructure)
    
    
    #### Define the folders and paths used when a project is open: ####
    projectSessionFolder <- file.path(stoxFolders["Process"], "projectSession")
    # Sub folders:
    dataFolder <- file.path(projectSessionFolder, "data")
    GUIFolder <- file.path(projectSessionFolder, "GUI")
    projectDescriptionFolder <- file.path(projectSessionFolder, "projectDescription")
    statusFolder <- file.path(projectSessionFolder, "status")
    # Return also a vector of all session folders, to generate the folder structure recursively:
    projectSessionFolderStructure <- c(
            dataFolder, 
            GUIFolder, 
            projectDescriptionFolder, 
            statusFolder
    )
    
    #### Project description: ####
    projectRDataFile = file.path(stoxFolders["Process"], "project.RData")
    projectXMLFile = file.path(stoxFolders["Process"], "project.xml")
    projectSavedStatusFile = file.path(statusFolder, "projectSavedStatus.txt")
    projectStatusFile = file.path(statusFolder, "projectStatus.txt")
    
    # Memory files:
    originalProjectDescriptionFile <- file.path(projectDescriptionFolder, "originalProjectDescription.rds")
    currentProjectDescriptionFile <- file.path(projectDescriptionFolder, "currentProjectDescription.rds")
    projectDescriptionIndexFile <- file.path(projectDescriptionFolder, "projectDescriptionIndex.txt")
    
    #### Data types: ####
    stoxModelDataTypes <- c(
        "AcousticData", 
        "StoxAcousticData", 
        "MergedStoxAcousticData", 
        "NASCData", 
        "LandingData", 
        "LandingCovariateData", 
        "LandingWeightCovariateData", 
        "BioticData", 
        "StoxBioticData", 
        "MergedStoxBioticData", 
        "BioticCovariateData", 
        "LengthDistributionData", 
        "AssignmentLengthDistributionData", 
        "Density", 
        "StratumArea", 
        "Abundance", 
        "AssignedIndividuals", 
        "AssignedStations", 
        "SuperIndividuals"
    )
    stoxProcessDataTypes <- c(
        "AcousticPSU", 
        "AcousticLayer", 
        "SweptAreaPSU", 
        "SweptAreaLayer", 
        "Assignment", 
        "Survey", 
        "SpeciesCategoryDefinition", 
        "AcousticCategoryDefinition", 
        "StratumPolygon", 
        "TemporalCovariate", 
        "GearCovariate", 
        "SpatialCovariate", 
        "PlatformCovariate", 
        "AgeError", 
        "StratumNeighbour"
    )
    stoxDataTypes <- c(
        stoxModelDataTypes, 
        stoxProcessDataTypes
    )
    
    #### Define an object with all path objects for convenience in getProjectPaths(): ####
    paths <- c(
        stoxFolderStructureList, 
        list(
            # Folders:
            stoxFolderStructure = stoxFolderStructure, 
            
            # Project session:
            projectSessionFolder = projectSessionFolder, 
            dataFolder = dataFolder, 
            GUIFolder = GUIFolder, 
            projectDescriptionFolder = projectDescriptionFolder, 
            statusFolder = statusFolder, 
            projectSessionFolderStructure = projectSessionFolderStructure, 
            
            # Project description:
            projectRDataFile = projectRDataFile, 
            projectXMLFile = projectXMLFile, 
            projectSavedStatusFile = projectSavedStatusFile, 
            projectStatusFile = projectStatusFile, 
            originalProjectDescriptionFile = originalProjectDescriptionFile, 
            currentProjectDescriptionFile = currentProjectDescriptionFile, 
            projectDescriptionIndexFile = projectDescriptionIndexFile
        )
    )
    
    #### Assign to RstoxEnv and return the definitions: ####
    definitions <- c(
        list(
            # Fundamental settings:
            validOutputDataClasses = validOutputDataClasses, 
            stoxFolders = stoxFolders, 
            stoxDataSources = stoxDataSources, 
            stoxModelNames = stoxModelNames, 
            stoxModelDisplayNames = stoxModelDisplayNames, 
            stoxModelDescriptions = stoxModelDescriptions, 
            stoxModelInfo = stoxModelInfo,
            processParameters = processParameters, 
            processDefaultFull = processDefaultFull, 
            processDefaultSansProcessData = processDefaultSansProcessData, 
            processDefault = processDefault
        ), 
        paths, 
        list(# Parameters and data types:
            stoxModelDataTypes = stoxModelDataTypes, 
            stoxProcessDataTypes = stoxProcessDataTypes, 
            stoxDataTypes = stoxDataTypes, 
            
            # This is defined in the file Templates.R:
            stoxTemplates = stoxTemplates, 
            
            # Repeat the paths for convenience:
            paths = paths
        )
    )
    
    #### Create the RstoxFrameworkEnv environment, holding definitions on folder structure and all the projects. This environment cna be accesses using RstoxFramework:::RstoxFrameworkEnv: ####
    utils::globalVariables("RstoxFrameworkEnv")
    assign("RstoxFrameworkEnv", new.env(), parent.env(environment()))
    
    assign("definitions", definitions, envir=get("RstoxFrameworkEnv"))
    assign("projects", list(), envir=get("RstoxFrameworkEnv"))
    
    #### Return the definitions: ####
    definitions
}


##################################################
##################################################
#' Get RstoxFramework definitions
#' 
#' This function gets vital definitions from the RstoxFramework environment.
#' 
#' @param name  An optional string vector denoting which definitions to extract.
#' @param ...   values overriding the values of definitions.
#' 
#' @return
#' A list of definitions.
#' 
#' @examples
#' getRstoxFrameworkDefinitions()
#' 
#' @export
#' 
getRstoxFrameworkDefinitions <- function(name = NULL, ...) {
    
    # Save the optional inputs for overriding the output:
    l <- list(...)
    
    # Get all or a subset of the definitions:
    definitions <- get("RstoxFrameworkEnv")$definitions
    if(length(name)){
        definitions <- definitions[[name]]
    }
    
    l <- l[names(l) %in% names(definitions)]
    if(length(l)){
        definitions <- utils::modifyList(definitions, l)
    }
    
    definitions
}

#' 
#' @export
#'
getAvaiableTemplates <- function(list.out = FALSE) {
    # Get the templates:
    out <- getRstoxFrameworkDefinitions("stoxTemplates")
    # Return only the names if specified:
    if(!list.out) {
        out <- names(out)
    }
    out
}

#' 
#' @export
#' 
getProjectPaths <- function(projectPath, name = NULL) {
    # Paste the project path to the relevant folders:
    paths <- getRstoxFrameworkDefinitions("paths")
    # Add the project path to all paths:
    paths <- lapply(paths, function(x) if(is.list(x)) lapply(x, function(y) file.path(projectPath, y)) else file.path(projectPath, x))
    if(length(name)) {
        paths <- paths[[name]]
    }
    paths
}


##################################################
##################################################
#' Create the StoX directories
#' 
#' This function creates the "stox" folder and the "project" and "reference" sub folders.
#' 
#' @return
#' A list of paths to the "stox" folder and sub folders.
#' 
#' @noRd
#' @seealso Use \code{\link{getStoxSkeletonPaths}} to get the folder paths.
#' 
createProjectSkeleton <- function(projectPath, ow = FALSE) {
    
    # Check whether the project exists:
    if(dir.exists(projectPath)) {
        if(!ow) {
            stop("The project '", projectPath, "' exists. Choose a different project path.")
        }
    }
    
    # Get the paths of the root directory and StoX skeleton:
    stoxFolderStructure <- getProjectPaths(projectPath, "stoxFolderStructure")
    # Create the folders:
    lapply(stoxFolderStructure, dir.create, showWarnings = FALSE, recursive = TRUE)
    
    # Return the paths:
    stoxFolderStructure
}
#' 
#' @noRd
#' 
createProjectSessionFolderStructure <- function(projectPath, showWarnings = FALSE) {
    # Create the project session folder structure:
    projectSessionFolderStructure <- getProjectPaths(projectPath, "projectSessionFolderStructure")
    lapply(projectSessionFolderStructure, dir.create, recursive = TRUE, showWarnings = showWarnings)
}


#' 
#' @export
#' 
createProject <- function(projectPath, template = "EmptyTemplate", ow = FALSE, showWarnings = FALSE, open = TRUE) {
    
    # Create the project folder structure:
    projectSkeleton <- createProjectSkeleton(projectPath, ow = ow)
    
    # Get the tempaltes:
    templates <- getAvaiableTemplates(TRUE)
    thisTemplate <- templates[[template]]
    if(length(thisTemplate) == 0) {
        stop("The requested template does not exist. See getAvaiableTemplates() for a list of the available templates (with list.out = TRUE if you wish to see what the dirrefent templates are.)")
    }
    
    # Create the project session folder structure:
    createProjectSessionFolderStructure(projectPath, showWarnings = showWarnings)
    
    # Create an empty ProjectDescription:
    projectDescription <- createEmptyProjectDescription(projectPath)
    
    # Fill inn the processes:
    stoxModelNames <- getRstoxFrameworkDefinitions("stoxModelNames")
    for(modelName in stoxModelNames){
        for(processName in names(thisTemplate[[modelName]])){
            addProcess(
                projectPath = projectPath, 
                modelName = modelName, 
                processName = processName, 
                values = thisTemplate[[modelName]][[processName]], 
                only.current = TRUE
            )
        }
    }
    
    # Save the project, close it, and open:
    closeProject(projectPath, save = TRUE)
    if(open) {
        openProject(projectPath)
    }
}
#' 
#' @export
#' 
openProject <- function(projectPath, showWarnings = FALSE) {
    
    # Create the project session folder structure:
    createProjectSessionFolderStructure(projectPath, showWarnings = showWarnings)
    
    # Read the project description file:
    projectDescription <- readProjectDescription(projectPath)
    
    # Save the original and current projectDescription:
    setOriginalProjectDescription(projectPath, projectDescription)
    setProjectDescriptionAsCurrent(projectPath, projectDescription)
    
    # Set the status of the projcet as saved:
    setSavedStatus(projectPath, status = TRUE)
    
    TRUE
}
#' 
#' @export
#' 
closeProject <- function(projectPath, save = NULL) {
    
    # Check that the project has been saved:
    if(!isSaved(projectPath)) {
        if(isTRUE(save)) {
            saveProject(projectPath)
        }
        else if(!isFALSE(save)) {
            answer <- readline(paste("The project", projectPath, "has not been saved. Do you with to save before closing (y/n)?"))
            if(identical(tolower(answer), "y")) {
                saveProject(projectPath)
            }
        }
    }
    
    # Delete the project session folder structure:
    projectSessionFolderStructure <- getProjectPaths(projectPath, "projectSessionFolderStructure")
    unlink(projectSessionFolderStructure, recursive = TRUE, force = TRUE)
}
#' 
#' @export
#' 
saveProject <- function(projectPath) {
    # Get the current project description and save it to the project.RData file:
    writeProjectDescription(projectPath)
    # Set the status of the projcet as saved:
    setSavedStatus(projectPath, status = TRUE)
}
#' 
#' @export
#' 
saveAsProject <- function(projectPath, newProjectPath, ow = FALSE) {
    
    # Copy the current project and save it:
    copyProject(projectPath, newProjectPath, ow = ow)
    saveProject(newProjectPath)
    
    # Close the current project without saving
    closeProject(projectPath, save = FALSE)
    
    newProjectPath
}
#' 
#' @export
#' 
copyProject <- function(projectPath, newProjectPath, ow = FALSE) {
    if(ow) {
        unlink(newProjectPath, force = TRUE, recursive = TRUE)
    }
    dir.create(newProjectPath)
    lapply(list.dirs(projectPath, recursive = FALSE), file.copy, newProjectPath, recursive = TRUE)
    #file.copy(projectPath, newProjectPath, recursive=TRUE)
}


setSavedStatus <- function(projectPath, status) {
    writeLines(as.character(status), getProjectPaths(projectPath, "projectSavedStatusFile"))
}


isSaved <- function(projectPath) {
    as.logical(readLines(getProjectPaths(projectPath, "projectSavedStatusFile"))[1])
}

#' 
#' @export
#' 
isOpenProject <- function(projectPath) {
    all(sapply(getProjectPaths(projectPath, "projectSessionFolderStructure"), file.exists))
}

readProjectDescription <- function(projectPath) {
    # Get the path to the project description file:
    projectRDataFile <- getProjectPaths(projectPath, "projectRDataFile")
    load(projectRDataFile) # Creates the object
    projectDescription
    
}

writeProjectDescription <- function(projectPath) {
    # Get the current project description:
    projectDescription <- getCurrentProjectDescription(projectPath)
    
    # Get the path to the project description file, and save the current project description:
    projectRDataFile <- getProjectPaths(projectPath, "projectRDataFile")
    save(projectDescription, file = projectRDataFile)
}

createEmptyProjectDescription <- function(projectPath) {
    # Get the model types, and populate a list with these:
    modelTypes <- getRstoxFrameworkDefinitions("stoxModelNames")
    projectDescription <- vector("list", length(modelTypes))
    names(projectDescription) <- modelTypes
    setProjectDescriptionAsCurrent(projectPath, projectDescription = projectDescription, only.current = TRUE)
    projectDescription
}


##### Manage (write, read, undo, redo) the project description: #####

# 1. Funciton to get the path to a new project description file:
#' 
#' @export
#' 
getNewProjectDescriptionFilePath <- function(projectPath) {
    # Get the folder holding the project descriptions:
    projectDescriptionFolder <- getProjectPaths(projectPath, "projectDescriptionFolder")
    # Define a string with time in ISO 8601 format:
    timeString <- format(Sys.time(), tz = "UTC", format = "%Y%m%dT%H%M%OS3Z")
    # Define the file name including the time string, and build the path to the file:
    fileName <- paste0("projectDescription", "_", timeString, ".rds")
    filePath <- file.path(projectDescriptionFolder, fileName)
    filePath
}

# 4. Function to write the original project description:
#' 
#' @export
#' 
setOriginalProjectDescription <- function(projectPath, projectDescription) {
    # Get the path to the originalProjectDescriptionFile, and write the input projectDescription to it:
    originalProjectDescriptionFile <- getProjectPaths(projectPath, "originalProjectDescriptionFile")
    saveRDS(projectDescription, file = originalProjectDescriptionFile)
}

# 5. Function to read the current project description:
#' 
#' @export
#' 
getCurrentProjectDescription <- function(projectPath) {
    currentProjectDescriptionFile <- getProjectPaths(projectPath, "currentProjectDescriptionFile")
    readRDS(currentProjectDescriptionFile)
}

# 6. Function to write the current project description:
#' 
#' @export
#' 
setProjectDescriptionAsCurrent <- function(projectPath, projectDescription, only.current = FALSE) {
    
    # Save to the currentProjectDescriptionFile:
    currentProjectDescriptionFile <- getProjectPaths(projectPath, "currentProjectDescriptionFile")
    saveRDS(projectDescription, file = currentProjectDescriptionFile)
    
    if(!only.current) {
        # Get the new project description file path and write the project description to this file:
        newProjectDescriptionFilePath <- getNewProjectDescriptionFilePath(projectPath)
        saveRDS(projectDescription, file = newProjectDescriptionFilePath)
        
        # Update the projectDescriptionIndexFile:
        projectDescriptionIndex <- readProjectDescriptionIndexFile(projectPath)
        # Delete any files with positive index:
        hasPositiveIndex <- projectDescriptionIndex$Index > 0
        if(any(hasPositiveIndex)) {
            unlink(projectDescriptionIndex$Path[hasPositiveIndex])
            projectDescriptionIndex <- projectDescriptionIndex[!hasPositiveIndex, ]
        }
        # Subtract 1 from the indices, and add the new project description file path:
        projectDescriptionIndex$Index <- projectDescriptionIndex$Index - 1
        projectDescriptionIndex <- rbind(
            projectDescriptionIndex, 
            data.frame(
                Index = 0, 
                Path = newProjectDescriptionFilePath
            )
        )
        # Write the projectDescriptionIndex to file:
        writeProjectDescriptionIndexFile(projectPath, projectDescriptionIndex)
    }
    
}

# 7. Function to undo or redo, i.e., reset the current project description file and change the indices. There will be separate GUI functions for undo and redo:
#' 
#' @export
#' 
unReDoProject <- function(projectPath, shift = 0) {
    # Read the projectDescriptionIndexFile, and add the shift value to the index:
    projectDescriptionIndex <- readProjectDescriptionIndexFile(projectPath)
    projectDescriptionIndex$Index <- projectDescriptionIndex$Index - shift
    writeProjectDescriptionIndexFile(projectPath, projectDescriptionIndex)
    
    # Copy the current projectDescription (with index = 0) to the currentProjectDescriptionFile:
    fileWithCurrentProjectDescription  <- projectDescriptionIndex$Path[projectDescriptionIndex$Index == 0]
    file.copy(
        from = fileWithCurrentProjectDescription, 
        to = getProjectPaths(projectPath, "currentProjectDescriptionFile"), 
        overwrite = TRUE, 
        copy.date = TRUE
    )
}

# 7.1 Function to read the projectDescriptionIndexFile:
#' 
#' @export
#' 
readProjectDescriptionIndexFile <- function(projectPath) {
    # Read the projectDescriptionIndexFile:
    projectDescriptionIndexFile <- getProjectPaths(projectPath, "projectDescriptionIndexFile")
    
    # If missing, create the file as an empty file:
    if(!file.exists(projectDescriptionIndexFile)) {
        NULL
    }
    else {
        data.table::fread(projectDescriptionIndexFile, sep = "\t")
    }
}

# 7.2 Function to write the projectDescriptionIndexFile:
#' 
#' @export
#' 
writeProjectDescriptionIndexFile <- function(projectPath, projectDescriptionIndex) {
    # Read the projectDescriptionIndexFile:
    projectDescriptionIndexFile <- getProjectPaths(projectPath, "projectDescriptionIndexFile")
    data.table::fwrite(projectDescriptionIndex, file =  projectDescriptionIndexFile, sep = "\t")
}















getFunctionOutputDataType <- function(functionName) {
    stoxFunctionAttributes[[functionName]]$FunctionOutputDataType
}

isProcessDataFunction <- function(functionName) {
    functionOutputDataType <- getFunctionOutputDataType(functionName)
    functionOutputDataType %in% getRstoxFrameworkDefinitions("stoxProcessDataTypes")
}

getFunctionCategory <- function(functionName) {
    stoxFunctionAttributes[[functionName]]$FunctionCategory
}

getFunctionParameterHierarchy <- function(functionName) {
    stoxFunctionAttributes[[functionName]]$FunctionParameterHierarchy
}

getParametersToShowInStoX <- function(functionName) {
    functionParameterHierarchy <- getFunctionParameterHierarchy(functionName)
    names(functionParameterHierarchy)
}

getFunctionDefaults <- function(functionName) {
    
    # Get the formals:
    f <- formals(functionName)
    
    # Convert missing inputs to NULL, to preserve the name-value-pair convention, and to allow evaluating the calls returned by formals():
    areMissing <- sapply(f, class) == "name" & sapply(f, function(x) length(x) > 0 & sum(nchar(x)) == 0)
    f[areMissing] <- vector("list", sum(areMissing))
    
    # Evaluate and return:
    f <- lapply(f, eval)
    f
}


            #' 
            #' @export
            #' 
            getStoxFunctions <- function(modelName) {
                
                # Finish this!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                
                # Get the categories:
                stoxModelNames <- getRstoxFrameworkDefinitions("stoxModelNames")
                # Get the names of the available functions:
                availableFunctions <- names(functionAttributes)
                # Get the category of each funciton, and split by category:
                functionCategories <- lapply(functionAttributes, "[[", "FunctionCategory")
                functionAttributesByCategory <-split(functionAttributes, functionCategories)
                # Keep only the valid categories:
                functionAttributesByCategory <- functionAttributesByCategory[stoxModelNames]
                
            }















isFunctionInput <- function(parameter) {
    # Get the valid data types (model data and process data), and check whether the inputs are in these:
    stoxDataTypes <- getRstoxFrameworkDefinitions("stoxDataTypes")
    parameter %in% stoxDataTypes
}


createEmptyProcess <- function(modelName = "Baseline") {
    # Get the default process with empty fields for project and funciton name, process data, and function parameters and inputs:
    getRstoxFrameworkDefinitions("processDefault")[[modelName]]
}



#' 
#' @export
#' 
modifyFunctionName <- function(projectPath, modelName, processName, newFunctionName, only.current = FALSE) {
    
    # Get the project description:
    projectDescription <- getCurrentProjectDescription(projectPath)
    # Convert form possible JSON input:
    newFunctionName <- parseParameter(newFunctionName)
    
    # Set the function name:
    if(!identical(projectDescription[[modelName]][[processName]]$functionName, newFunctionName)) {
        
        projectDescription[[modelName]][[processName]]$functionName <- newFunctionName
        
        # Get the parameters to display:
        parametersToShowInStoX <- getParametersToShowInStoX(newFunctionName)
        
        # Detect which parameters are data types, which identifies them as function inputs (outputs from other processes):
        areInputs <- isFunctionInput(parametersToShowInStoX)
        
        # Get the default values:
        defaults <- getFunctionDefaults(newFunctionName)
        defaults <- defaults[parametersToShowInStoX]
        
        # Split the defaults into function parameters and function inputs:
        functionParameters <- defaults[!areInputs]
        functionInputs <- defaults[areInputs]
        
        # Change the function parameters and inputs:
        projectDescription[[modelName]][[processName]]$functionParameters <- functionParameters
        projectDescription[[modelName]][[processName]]$functionInputs <- functionInputs
    }
    
    # Store the changes:
    setProjectDescriptionAsCurrent(projectPath, projectDescription, only.current = only.current)
}
#' 
#' @export
#' 
modifyFunctionParameters <- function(projectPath, modelName, processName, newFunctionParameters, only.current = FALSE) {
    
    # Get the project description:
    projectDescription <- getCurrentProjectDescription(projectPath)
    # Convert form possible JSON input:
    newFunctionParameters <- parseParameter(newFunctionParameters)
    
    # Report a warning for function parameters not present in the process:
    valid <- names(newFunctionParameters) %in% names(projectDescription[[modelName]][[processName]]$functionParameters)
    if(any(!valid)) {
        warning(
            "The following function parameters are not present for the function ", 
            projectDescription[[modelName]][[processName]]$functionName, 
            " of the process ", 
            projectDescription[[modelName]][[processName]]$processName, 
            ": ", 
            paste(names(newFunctionParameters)[!valid], collapse = ", ")
        )
        
        # Keep only the newFunctionParameters present in the existing functionParameters:
        newFunctionParameters <- newFunctionParameters[valid]
    }
    
    # Insert the function parameters (one by one for safety):
    for(ind in seq_along(newFunctionParameters)) {
        projectDescription[[modelName]][[processName]]$functionParameters[[names(newFunctionParameters[ind])]] <- newFunctionParameters[[ind]]
    }
    
    # Store the changes:
    setProjectDescriptionAsCurrent(projectPath, projectDescription, only.current = only.current)
}
#' 
#' @export
#' 
modifyFunctionInputs <- function(projectPath, modelName, processName, newFunctionInputs, only.current = FALSE) {
    
    # Get the project description:
    projectDescription <- getCurrentProjectDescription(projectPath)
    # Convert form possible JSON input:
    newFunctionInputs <- parseParameter(newFunctionInputs)
    
    # Report a warning for function inputs not present in the process:
    valid <- names(newFunctionInputs) %in% names(projectDescription[[modelName]][[processName]]$functionInputs)
    if(any(!valid)) {
        warning(
            "The following function parameters are not present for the function ", 
            projectDescription[[modelName]][[processName]]$functionName, 
            " of the process ", 
            projectDescription[[modelName]][[processName]]$processName, 
            ": ", 
            paste(names(functionInputs)[!valid], collapse = ", ")
        )
        
        # Keep only the newFunctionParameters present in the existing functionParameters:
        newFunctionInputs <- newFunctionInputs[valid]
    }
    
    # Insert the function inputs:
    for(ind in seq_along(newFunctionInputs)) {
        projectDescription[[modelName]][[processName]]$functionInputs[[names(newFunctionInputs[ind])]] <- newFunctionInputs[[ind]]
    }
    
    
    # Store the changes:
    setProjectDescriptionAsCurrent(projectPath, projectDescription, only.current = only.current)
}
#' 
#' @export
#' 
modifyProcessName <- function(projectPath, modelName, processName, newProcessName, only.current = FALSE) {
    
    # Get the project description:
    projectDescription <- getCurrentProjectDescription(projectPath)
    # Convert form possible JSON input:
    newProcessName <- parseParameter(newProcessName)
    
    # Set the process name:
    projectDescription[[modelName]][[processName]]$processName <- newProcessName
    # Rename the process in the list of processes of the model:
    names(projectDescription[[modelName]])[names(projectDescription[[modelName]]) == processName] <- newProcessName
    
    # Store the changes:
    setProjectDescriptionAsCurrent(projectPath, projectDescription, only.current = only.current)
}
#' 
#' @export
#' 
modifyProcessParameters <- function(projectPath, modelName, processName, newProcessParameters, only.current = FALSE) {
    
    # Get the project description:
    projectDescription <- getCurrentProjectDescription(projectPath)
    # Convert form possible JSON input:
    newProcessParameters <- parseParameter(newProcessParameters)
    
    # Get names of the process parameters:
    validProcessParameterNames = names(getRstoxFrameworkDefinitions("processParameters"))
    
    # Report a warning for non-existing process parameters:
    valid <- names(newProcessParameters) %in% validProcessParameterNames
    if(any(!valid)) {
        warning("The following process parameters are not valid: ", paste(names(newProcessParameters)[!valid], collapse = ", "))
        newProcessParameters <- newProcessParameters[valid]
    }
    
    # Insert the process parameters:
    for(ind in seq_along(newProcessParameters)) {
        projectDescription[[modelName]][[processName]]$processParameters[[names(newProcessParameters[ind])]] <- newProcessParameters[[ind]]
    }
    
    
    # Store the changes:
    setProjectDescriptionAsCurrent(projectPath, projectDescription, only.current = only.current)
}
#' 
#' @export
#' 
modifyProcess <- function(projectPath, modelName, processName, newValues, only.current = FALSE) {
    
    # The values of the process must be changed in the following order:
    # 1. Function name
    # 2. Function parameters
    # 2. Function inputs
    # 1. Process name
    # 1. Process parameters
    
    # Convert form possible JSON input:
    newValues <- parseParameter(newValues)
    
    # Process name:
    if(length(newValues$processName)) {
        modifyProcessName(
            projectPath = projectPath, 
            modelName = modelName, 
            processName = processName, 
            newProcessName = newValues$ProcessName, 
            only.current = only.current
        )
    }
    
    # Process parameters:
    if(length(newValues$processParameters)) {
        modifyProcessParameters(
            projectPath = projectPath, 
            modelName = modelName, 
            processName = processName, 
            newProcessParameters = newValues$processParameters, 
            only.current = only.current
        )
    }
    
    # Function name:
    if(length(newValues$functionName)) {
        modifyFunctionName(
            projectPath = projectPath, 
            modelName = modelName, 
            processName = processName, 
            newFunctionName = newValues$functionName, 
            only.current = only.current
        )
    }
    
    # Function parameters:
    if(length(newValues$functionParameters)) {
        modifyFunctionParameters(
            projectPath = projectPath, 
            modelName = modelName, 
            processName = processName, 
            newFunctionParameters = newValues$functionParameters, 
            only.current = only.current
        )
    }
    
    # Function inputs:
    if(length(newValues$functionInputs)) {
        modifyFunctionInputs(
            projectPath = projectPath, 
            modelName = modelName, 
            processName = processName, 
            newFunctionInputs = newValues$functionInputs, 
            only.current = only.current
        )
    }
    
    # Set the status as not saved (saving is done when running a process):
    setSavedStatus(projectPath, status = FALSE)
}
# Convert JSON input to list:
parseParameter <- function(parameter) {
    # If the parameter is JSON, convert to list:
    if("json" %in% class(parameter)) {
        parameter <- jsonlite::fromJSON(parameter)
    }
    parameter
}



#' 
#' @export
#' 
addEmptyProcess <- function(projectPath, modelName, processName = NULL, only.current = FALSE) {
    
    # Get the project description:
    projectDescription <- getCurrentProjectDescription(projectPath)
    
    # Get all process names of the specified model:
    processNames <- as.character(names(projectDescription[[modelName]]))
    
    # If processName is given, check that it is not already in use:
    if(length(processName)) {
        if(processName %in% processNames) {
            stop(paste("The specified process name", processName, "is already in use."))
        }
    }
    else {
        # Identify all process names starting with "Process_":
        process_Prefix <- "Process_"
        startsWithProcess_Prefix <- startsWith(processNames, process_Prefix)
        
        # Get the lowest index which is not occupied:
        if(any(startsWithProcess_Prefix)) {
            # Extract the integers after the underscore:
            process_Index <- as.numeric(substring(processNames[startsWithProcess_Prefix], nchar(process_Prefix) + 1))
            process_Index <- min(seq_len(max(process_Index)))
        }
        else {
            process_Index <- 1
        }
        
        # Create the name of the new project:
        processName <- paste0(process_Prefix, process_Index)
    }
    
    # Create an empty process:
    projectDescription[[modelName]][[processName]] <- createEmptyProcess()
    
    # Store the changes:
    setProjectDescriptionAsCurrent(projectPath, projectDescription, only.current = only.current)
    
    # Set also the process name (must be done after saving the project description, as the function modifyProcessName reads and writes the currentProjectDescription.rds file):
    modifyProcessName(projectPath, modelName, processName, processName, only.current = FALSE)
}



#' 
#' @export
#' 
addProcess <- function(projectPath, modelName, processName, values, only.current = FALSE) {
    
    # Create an empty process:
    addEmptyProcess(projectPath = projectPath, modelName = modelName, processName = processName, only.current = only.current)
    
    # Apply the arguments:
    modifyProcess(projectPath = projectPath, modelName = modelName, processName = processName, newValues = values, only.current = only.current)
}


# rearrangeProcesses <- function(projectPath, modelName, processNames, MoveTo = NULL) {
#     
#     # Get the project description:
#     projectDescription <- getCurrentProjectDescription(projectPath)
#     
#     # Get the names of all processes, and the indices of the selected processes:
#     originalxProcessNames <- names(projectDescription[[modelName]])
#     indexOfProcessesToMove <- match(processNames, originalxProcessNames)
#     
#     
# }




#### Functions to run models: ####



#' 
#' @export
#' 
getProcess <- function(projectPath, modelName, processName) {
    projectDescription <- getCurrentProjectDescription(projectPath)
    process <- projectDescription[[modelName]][[processName]]
    process$processIndex <- match(processName, names(projectDescription[[modelName]]))
    process
}
#' 
#' @export
#' 
runProcess <- function(projectPath, modelName, processName) {
    
    # Get the process:
    process <- getProcess(projectPath, modelName, processName)
    
    # If not not enabled, return immediately:
    if(!process$processParameters$enabled) {
        return(NULL)
    }
    
    # Build a list of the arguments to the function:
    functionArguments <- list()
    # Add the processData if a processData function:
    if(isProcessDataFunction(process$functionName)) {
        functionArguments$processData <- process$processData
    }
    
    # Get the function input as output from the previously run processes:
    functionInputs <- lapply(process$functionInputs, getProcessOutput, projectPath = projectPath, modelName = modelName)
    #names(functionInputs) <- names(process$functionInputs)
    # Warning, the data type is included in the process output files as the top level of the list, so we need to unlist to get the functionInputs (non-recursively):
    #functionInputs <- unlist(lapply(process$functionInputs, getProcessOutput, projectPath = projectPath, modelName = modelName), recursive = FALSE, use.names = FALSE)
    
    
    # Add functionInputs and functionParameters:
    functionArguments <- c(
        functionArguments, 
        functionInputs, 
        process$functionParameters
    )
    
    # Run the function:
    processOutput <- do.call(
        process$functionName, 
        functionArguments
    )
    # Wrap the function output to a list named with the data type:
    processOutput <- list(processOutput)
    names(processOutput) <- getFunctionOutputDataType(process$functionName)
    
    ## Wrap the function output to a list (for functions returning only one output):
    #processOutput <- wrapProcessOutputToList(processOutput)
    
    ## Define the names of the function outputs as processName + name of the function output as taken directly from the function:
    #processOutput <- setProcessOutputNames(processOutput, processName)
    
    # Store the processData (this must be a named list of only one data table):
    if(isProcessDataFunction(process$functionName)) {
        process$processData <- processOutput
    }
    
    # Write to memory files:
    writeProcessOutputMemoryFile(processOutput = processOutput, process = process, projectPath = projectPath, modelName = modelName)
    
    # Write to text files:
    if(process$processParameters$FileOutput) {
        writeProcessOutputTextFile(processOutput = processOutput, process = process, projectPath = projectPath, modelName = modelName)
    }
    
    invisible(processOutput)
}


#wrapProcessOutputToList <- function(processOutput) {
#    if(is.data.frame(processOutput) || "SpatialPolygons" %in% class(processOutput)) {
#        processOutput <- list(processOutput)
#    }
#    processOutput
#}

#setProcessOutputNames <- function(processOutput, processName) {
#    processOutputNames <- paste(processName, names(processOutput), sep = "_")
#    processOutputNames <- gsub('\\_$', '', processOutputNames)
#    names(processOutput) <- processOutputNames
#    processOutput
#}

#' 
#' @export
#' 
getProcessOutput <- function(projectPath, modelName, processName) {
    folderName <- getProcessOutputFolder(projectPath = projectPath, modelName = modelName, processName = processName)
    # There will always be only one file in each folder of process data, so we pick the first element:
    filePath <- list.files(folderName, full.names = TRUE, pattern = "\\.rds$")[1]
    readRDS(filePath)
}


getProcessOutputFolder <- function(projectPath, modelName, processName) {
    file.path(getProjectPaths(projectPath, "dataFolder"), modelName, processName)
}




#' 
#' @export
#' 
writeProcessOutputTextFile <- function(processOutput, process, projectPath, modelName) {
    
    # Function for writing one element of the function output list:
    reportFunctionOutputOne <- function(processOutputOne, filePathSansExt) {
        
        
        if("SpatialPolygons" %in% class(processOutputOne)) {
            # Add file extension:
            filePath <- paste(filePathSansExt, "geojson", sep = ".")
            # Write the file:
            jsonlite::write_json(geojsonio::geojson_json(processOutputOne), path = filePath)
        }
        else if("data.table" %in% class(processOutputOne)) {
            # Add file extension:
            filePath <- paste(filePathSansExt, "txt", sep = ".")
            # Write the file:
            data.table::fwrite(processOutputOne, filePath, sep = "\t")
        }
        else {
            stop("Unknown function output")
        }
    }
    
    # Flatten the list and add names from the levels of the list:
    unlistToDataType <- function(processOutput) {
        
        areAllValidOutputDataClasses <- function(processOutput) {
            validOutputDataClasses <- getRstoxFrameworkDefinitions("validOutputDataClasses")
            classes <- lapply(processOutput, class)
            classes <- unlist(lapply(classes, "[[", 1))
            all(classes %in% validOutputDataClasses)
        }
        
        
        unlistOne <- function(processOutput) {
            if(!areAllValidOutputDataClasses(processOutput)){
                processOutput <- unlist(processOutput, recursive = FALSE)
            }
            processOutput
        }
        
        for(i in seq_len(2)) {
            processOutput <- unlistOne(processOutput)
        }

                
        processOutput
    }
    
    processOutput <- unlistToDataType(processOutput)
    names(processOutput) <- gsub(".", "_", names(processOutput), fixed = TRUE)
    
    folderName <- getProjectPaths(projectPath, paste("Output", modelName, sep = "_"))
    fileNamesSansExt <- paste(process$processIndex, names(processOutput), sep = "_")
    filePathsSansExt <- file.path(folderName, paste(fileNamesSansExt))
    
    # Set the file name:
    mapply(reportFunctionOutputOne, processOutput, filePathsSansExt)
    # lapply(processOutput, reportFunctionOutputOne)
}

#' 
#' @export
#' 
writeProcessOutputMemoryFile <- function(processOutput, process, projectPath, modelName) {
    
    # Set the file name:
    folderName <- getProcessOutputFolder(projectPath = projectPath, modelName = modelName, processName = process$processName)
    fileNameSansExt <- paste(process$processIndex, names(processOutput), sep = "_")
    fileName <- paste(fileNameSansExt, "rds", sep = ".")
    filePath <- file.path(folderName, fileName)
    
    # Create the folder and save the process output as one file containing a list of DataType, ans possible sublists specified by the function producing the output:
    dir.create(dirname(filePath), recursive = TRUE, showWarnings = FALSE)
    # Drop the top level, since this is the data type, and this is known in runProcess:
    saveRDS(processOutput[[1]], filePath)
}


#' 
#' @export
#' 
runModel <- function(projectPath, modelName, startProcess = 1, endProcess = 3) {
    processNames <- getProcessList(projectPath, modelName)[seq(startProcess, endProcess)]
    for(processName in processNames) {
        temp <- runProcess(projectPath = projectPath, modelName = modelName, processName = processName)
    }
}
















            # This function checks that 
            checkModel <- function(projectPath, modelName) {
    
                # Function to check that the function inputs of one process are all process names existing prior to that function:
                checkFunctionInputs <- function(ind, functionInputs, processNames) {
                    all(functionInputs %in% processNames[seq_len(ind - 1)])
                }
                
                # Get the processes of the model:
                processes <- getProcesses(projectPath, modelName)
                
                # (1) Check process names:
                processNames <- names(processes)
                duplicatedProcessNames <- processNames[duplicated(processNames)]
                if(length(duplicatedProcessNames)) {
                    message("The following process names are not unique: ", paste(duplicatedProcessNames))
                    return(FALSE)
                }
                
                # (2) Check that all function inputs are existing process names prior to the current process:
                functionInputs <- lapply(processes, "[[", "functionInputs")
                areValidFunctionInputs <- sapply(seq_along(functionInputs), checkFunctionInputs, functionInputs = functionInputs,           processNames = processNames)
                processesWithInvalidFunctionInputs <- processNames[!areValidFunctionInputs]
                if(length(processesWithInvalidFunctionInputs)) {
                    message("The following processes have function inputs that are not the name of a prior process: ", paste         (processesWithInvalidFunctionInputs))
                    return(FALSE)
                }
            }
            
            
            # Function to update all relevant function inputs of a model to the new process name when a process has changed name:
            updateFunctionInputs <- function() {
                
            }

