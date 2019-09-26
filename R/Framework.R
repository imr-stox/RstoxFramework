# The data types for communication with StoX: Boolean, String, Numeric, NULL, List of predefined values, FileName, FileNames
# 
# There is a hierarchy FuncitonName - FunctionParameters - FunctionInputs. This needs to be taken into account in the categories and items
# 
# What to do with boolean, integer, numeric, string?
#
# - We will use FunctionInputs and FunctionParameters.
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
    stoxModelTypes <- c(
        Baseline = "Baseline", 
        Statistics = "Statistics", 
        Report = "Report"
    )
    # Define the process parameters with default values:
    processParameters <- list(
        Enabled = TRUE, 
        BreakInGUI = FALSE, 
        FileOutput = TRUE
    )
    
    #### Define the folder structure of StoX: ####
    stoxFolderStructure <- list(
        stoxDataSources, 
        stoxModelTypes, 
        ""
    )
    names(stoxFolderStructure) <- stoxFolders
    stoxFolderStructure <- unname(unlist(mapply(file.path, names(stoxFolderStructure), stoxFolderStructure)))
    
    #### Define the folders and paths used when a project is open: ####
    projectSessionFolder <- file.path(stoxFolders["Process"], "projectSession")
    # Sub folders:
    dataFolder <- file.path(projectSessionFolder, "data")
    GUIFolder <- file.path(projectSessionFolder, "GUI")
    projectDescriptionFolder <- file.path(projectSessionFolder, "projectDescription")
    settingsFolder <- file.path(projectSessionFolder, "settings")
    # Return also a vector of all session folders, to generate the folder structure recursively:
    projectSessionFolderStructure <- c(
            dataFolder, 
            GUIFolder, 
            projectDescriptionFolder, 
            settingsFolder
    )
    
    #### Project description: ####
    projectRDataFile = file.path(stoxFolders["Process"], "project.RData")
    projectXMLFile = file.path(stoxFolders["Process"], "project.xml")
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
    paths <- list(
        # Folders:
        stoxFolderStructure = stoxFolderStructure, 
        
        # Project session:
        projectSessionFolder = projectSessionFolder, 
        dataFolder = dataFolder, 
        GUIFolder = GUIFolder, 
        projectDescriptionFolder = projectDescriptionFolder, 
        settingsFolder = settingsFolder, 
        projectSessionFolderStructure = projectSessionFolderStructure, 
        
        # Project description:
        projectRDataFile = projectRDataFile, 
        projectXMLFile = projectXMLFile, 
        originalProjectDescriptionFile = originalProjectDescriptionFile, 
        currentProjectDescriptionFile = currentProjectDescriptionFile, 
        projectDescriptionIndexFile = projectDescriptionIndexFile
    )
    
    #### Assign to RstoxEnv and return the definitions: ####
    definitions <- c(
        list(
            # Fundamental settings:
            stoxFolders = stoxFolders, 
            stoxDataSources = stoxDataSources, 
            stoxModelTypes = stoxModelTypes, 
            processParameters = processParameters
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
#' @param ...   Values overriding the values of definitions.
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
getProjectPaths <- function(ProjectPath, name = NULL) {
    # Paste the project path to the relevant folders:
    paths <- getRstoxFrameworkDefinitions("paths")
    # Add the project path to all paths:
    paths <- lapply(paths, function(x) if(is.list(x)) lapply(x, function(y) file.path(ProjectPath, y)) else file.path(ProjectPath, x))
    if(length(name)) {
        paths <- paths[[name]]
    }
    paths
}


createProjectSkeleton <- function(ProjectPath, ow = FALSE) {
    
    # Check whether the project exists:
    if(dir.exists(ProjectPath)) {
        if(!ow) {
            warning("The project '", ProjectPath, "' exists. Choose a different project path.")
            stop(NULL) 
        }
        
    }
    else {
        # Get the paths of the root directory and StoX skeleton:
        stoxFolderStructure <- getProjectPaths(ProjectPath, "stoxFolderStructure")
        # Create the folders:
        lapply(stoxFolderStructure, dir.create, showWarnings = FALSE, recursive = TRUE)
    }
    
    # Return the paths:
    stoxFolderStructure
}


#' 
#' @export
#' 
createProject <- function(ProjectPath, Template = "EmptyTemplate", ow = FALSE) {
    
    # Create the project folder structure:
    projectSkeleton <- createProjectSkeleton(ProjectPath, ow = ow)
    
    # Get the tempaltes:
    templates <- getAvaiableTemplates(TRUE)
    thisTemplate <- templates[[Template]]
    if(length(thisTemplate) == 0) {
        stop("The requested template does not exist. See getAvaiableTemplates() for a list of the available templates (with list.out = TRUE if you wish to see what the dirrefent templates are.)")
    }
    
    # Create an empty ProjectDescription:
    projectDescription <- createEmptyProjectDescription()
    
    browser()
    # Fill inn the processes:
    stoxModelTypes <- getRstoxFrameworkDefinitions("stoxModelTypes")
    for(ModelName in stoxModelTypes){
        for(ProcessName in names(thisTemplate[[ModelName]])){
            ModifyProcess(
                ProcessName = ProcessName, 
                ModelName = ModelName, 
                ProjectPath = ProjectPath, 
                NewValues = thisTemplate[[ModelName]][[ProcessName]], 
                only.current = TRUE
            )
        }
    }
    
    
}

#' 
#' @export
#' 
openProject <- function(ProjectPath, showWarnings = FALSE) {
    
    # Create the project session folder structure:
    projectSessionFolderStructure <- getProjectPaths(ProjectPath, "projectSessionFolderStructure")
    lapply(projectSessionFolderStructure, dir.create, recursive = TRUE, showWarnings = showWarnings)
    
    # Read the project description file:
    projectDescription <- readProjectDescription(ProjectPath)
    
    # Save the original and current projectDescription:
    setOriginalProjectDescription(ProjectPath, projectDescription)
    setProjectDescriptionAsCurrent(ProjectPath, projectDescription)
    
    TRUE
}

#' 
#' @export
#' 
closeProject <- function(ProjectPath) {
    # Create the project session folder structure:
    projectSessionFolderStructure <- getProjectPaths(ProjectPath, "projectSessionFolderStructure")
    unlink(projectSessionFolderStructure, recursive = TRUE, force = TRUE)
}




readProjectDescription <- function(ProjectPath) {
    # Get the path to the project description file:
    projectRDataFile <- getProjectPaths(ProjectPath, "projectRDataFile")
    load(projectRDataFile) # Creates the object
    projectDescription
    
}

writeProjectDescription <- function(ProjectPath) {
    # Get the current project description:
    projectDescription <- getCurrentProjectDescription(ProjectPath)
    
    # Get the path to the project description file, and save the current project description:
    projectRDataFile <- getProjectPaths(ProjectPath, "projectRDataFile")
    save(projectDescription, file = projectRDataFile)
}

createEmptyProjectDescription <- function() {
    # Get the model types, and populate a list with these:
    modelTypes <- getRstoxFrameworkDefinitions("stoxModelTypes")
    projectDescription <- vector("list", length(modelTypes))
    names(projectDescription) <- modelTypes
    projectDescription
}


##### Manage (write, read, undo, redo) the project description: #####

# 1. Funciton to get the path to a new project description file:
#' 
#' @export
#' 
getNewProjectDescriptionFilePath <- function(ProjectPath) {
    # Get the folder holding the project descriptions:
    projectDescriptionFolder <- getProjectPaths(ProjectPath, "projectDescriptionFolder")
    # Define a string with time in ISO 8601 format:
    timeString <- format(Sys.time(), tz = "UTC", format = "%y-%m-%dT%H:%M:%OS3Z")
    # Define the file name including the time string, and build the path to the file:
    fileName <- paste0("projectDescription", "_", timeString, ".rds")
    filePath <- file.path(projectDescriptionFolder, fileName)
    filePath
}

# 4. Function to write the original project description:
#' 
#' @export
#' 
setOriginalProjectDescription <- function(ProjectPath, projectDescription) {
    # Get the path to the originalProjectDescriptionFile, and write the input projectDescription to it:
    originalProjectDescriptionFile <- getProjectPaths(ProjectPath, "originalProjectDescriptionFile")
    saveRDS(projectDescription, file = originalProjectDescriptionFile)
}

# 5. Function to read the current project description:
#' 
#' @export
#' 
getCurrentProjectDescription <- function(ProjectPath) {
    currentProjectDescriptionFile <- getProjectPaths(ProjectPath, "currentProjectDescriptionFile")
    readRDS(currentProjectDescriptionFile)
}

# 6. Function to write the current project description:
#' 
#' @export
#' 
setProjectDescriptionAsCurrent <- function(ProjectPath, projectDescription, only.current = FALSE) {
    
    # Save to the currentProjectDescriptionFile:
    print(only.current)
    browser()
    currentProjectDescriptionFile <- getProjectPaths(ProjectPath, "currentProjectDescriptionFile")
    saveRDS(projectDescription, file = currentProjectDescriptionFile)
    
    if(!only.current) {
        # Get the new project description file path and write the project description to this file:
        newProjectDescriptionFilePath <- getNewProjectDescriptionFilePath(ProjectPath)
        saveRDS(projectDescription, file = newProjectDescriptionFilePath)
        
        # Update the projectDescriptionIndexFile:
        projectDescriptionIndex <- readProjectDescriptionIndexFile(ProjectPath)
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
        writeProjectDescriptionIndexFile(ProjectPath, projectDescriptionIndex)
    }
    
}

# 7. Function to undo or redo, i.e., reset the current project description file and change the indices. There will be separate GUI functions for undo and redo:
#' 
#' @export
#' 
unReDoProject <- function(ProjectPath, shift = 0) {
    # Read the projectDescriptionIndexFile, and add the shift value to the index:
    projectDescriptionIndex <- readProjectDescriptionIndexFile(ProjectPath)
    projectDescriptionIndex$Index <- projectDescriptionIndex$Index + shift
    
    # Copy the current projectDescription (with index = 0) to the currentProjectDescriptionFile:
    fileWithCurrentProjectDescription  <- projectDescriptionIndex$Path[projectDescriptionIndex$Index == 0]
    file.copy(fileWithCurrentProjectDescription, getProjectPaths(ProjectPath, "currentProjectDescriptionFile"))
}

# 7.1 Function to read the projectDescriptionIndexFile:
#' 
#' @export
#' 
readProjectDescriptionIndexFile <- function(ProjectPath) {
    # Read the projectDescriptionIndexFile:
    projectDescriptionIndexFile <- getProjectPaths(ProjectPath, "projectDescriptionIndexFile")
    
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
writeProjectDescriptionIndexFile <- function(ProjectPath, projectDescriptionIndex) {
    # Read the projectDescriptionIndexFile:
    projectDescriptionIndexFile <- getProjectPaths(ProjectPath, "projectDescriptionIndexFile")
    data.table::fwrite(projectDescriptionIndex, file =  projectDescriptionIndexFile, sep = "\t")
}














createEmptyBaselineProcess <- function() {
    list(
        ProcessName = NULL, 
        FunctionName = NULL, 
        ProcessParameters = getRstoxFrameworkDefinitions("processParameters"),
        ProcessData = list(), 
        FunctionParameters = list(), 
        FunctionInputs = list()
    )
}

getFunctionOutputDataType <- function(FunctionName) {
    attr(get(FunctionName), "FunctionOutputDataType")
}

getFunctionCategory <- function(FunctionName) {
    attr(get(FunctionName), "FunctionCategory")
}

getFunctionParameterParents <- function(FunctionName) {
    attr(get(FunctionName), "FunctionParameterParents")
}

getParametersToShowInStoX <- function(FunctionName) {
    functionParameterParents <- getFunctionParameterParents(FunctionName)
    names(functionParameterParents)
}

getFunctionDefaults <- function(FunctionName) {
    
    # Get the formals:
    f <- formals(FunctionName)
    
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
getStoxFunctions <- function(ModelName) {
    
    # Finish this!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    # Get the categories:
    stoxModelTypes <- getRstoxFrameworkDefinitions("stoxModelTypes")
    # Get the names of the available functions:
    availableFunctions <- names(functionAttributes)
    # Get the category of each funciton, and split by category:
    functionCategories <- lapply(functionAttributes, "[[", "FunctionCategory")
    functionAttributesByCategory <-split(functionAttributes, functionCategories)
    # Keep only the valid categories:
    functionAttributesByCategory <- functionAttributesByCategory[stoxModelTypes]
    
}

























ModifyFunctionName <- function(ProcessName, ModelName, ProjectPath, NewFunctionName, only.current = FALSE) {
    
    # Get the project description:
    projectDescription <- getCurrentProjectDescription(ProjectPath)
    
    # Set the function name:
    if(!identical(projectDescription[[ModelName]][[ProcessName]]$FunctionName, FunctionName)) {
        projectDescription[[ModelName]][[ProcessName]]$FunctionName <- FunctionName
        
        # Get the parameters to display:
        parametersToShowInStoX <- getParametersToShowInStoX(FunctionName)
        
        # Detect which parameters are data types, identifying them as function inputs (outputs from other processes):
        areInputs <- isFunctionInput(parametersToShowInStoX)
        
        # Get the default values:
        defaults <- getFunctionDefaults(FunctionName)
        defaults <- defaults[parametersToShowInStoX]
        
        # Split the defaults into function parameters and function inputs:
        FunctionParameters <- defaults[!areInputs]
        FunctionInputs <- defaults[areInputs]
        
        # Change the function parameters and inputs:
        projectDescription[[ModelName]][[ProcessName]]$FunctionParameters <- FunctionParameters
        projectDescription[[ModelName]][[ProcessName]]$FunctionInputs <- FunctionInputs
    }
    
    # Store the changes:
    setProjectDescriptionAsCurrent(ProcessName, projectDescription, only.current = only.current)
}

ModifyFunctionParameters <- function(ProcessName, ModelName, ProjectPath, NewFunctionParameters, only.current = FALSE) {
    
    # Get the project description:
    projectDescription <- getCurrentProjectDescription(ProjectPath)
    
    # Report a warning for function parameters not present in the process:
    valid <- names(NewFunctionParameters) %in% names(projectDescription[[ModelName]][[ProcessName]]$FunctionParameters)
    if(any(!valid)) {
        warning(
            "The following function parameters are not present for the function ", 
            projectDescription[[ModelName]][[ProcessName]]$FunctionName, 
            " of the process ", 
            projectDescription[[ModelName]][[ProcessName]]$ProcessName, 
            ": ", 
            paste(names(NewFunctionParameters)[!valid], collapse = ", ")
        )
        
        # Keep only the NewFunctionParameters present in the existing FunctionParameters:
        NewFunctionParameters <- NewFunctionParameters[valid]
    }
    
    # Insert the function parameters (one by one for safety):
    for(ind in seq_along(NewFunctionParameters)) {
        projectDescription[[ModelName]][[ProcessName]]$FunctionParameters[[names(NewFunctionParameters[ind])]] <- NewFunctionParameters[[ind]]
    }
    
    # Store the changes:
    setProjectDescriptionAsCurrent(ProcessName, projectDescription, only.current = only.current)
}

ModifyFunctionInputs <- function(ProcessName, ModelName, ProjectPath, NewFunctionInputs, only.current = FALSE) {
    
    # Report a warning for function inputs not present in the process:
    valid <- names(NewFunctionInputs) %in% names(projectDescription[[ModelName]][[ProcessName]]$FunctionInputs)
    if(any(!valid)) {
        warning(
            "The following function parameters are not present for the function ", 
            projectDescription[[ModelName]][[ProcessName]]$FunctionName, 
            " of the process ", 
            projectDescription[[ModelName]][[ProcessName]]$ProcessName, 
            ": ", 
            paste(names(FunctionInputs)[!valid], collapse = ", ")
        )
        
        # Keep only the NewFunctionParameters present in the existing FunctionParameters:
        NewFunctionInputs <- NewFunctionInputs[valid]
    }
    
    # Insert the function inputs:
    for(ind in seq_along(NewFunctionInputs)) {
        projectDescription[[ModelName]][[ProcessName]]$FunctionInputs[[names(NewFunctionInputs[ind])]] <- NewFunctionInputs[[ind]]
    }
    
    # Store the changes:
    setProjectDescriptionAsCurrent(ProcessName, projectDescription, only.current = only.current)
}

ModifyProcessName <- function(ProcessName, ModelName, ProjectPath, NewProcessName, only.current = FALSE) {
    
    # Get the project description:
    projectDescription <- getCurrentProjectDescription(ProjectPath)
    
    # Set the process name:
    projectDescription[[ModelName]][[ProcessName]]$ProcessName <- NewProcessName
    
    # Store the changes:
    setProjectDescriptionAsCurrent(ProcessName, projectDescription, only.current = only.current)
}

ModifyProcessParameters <- function(ProcessName, ModelName, ProjectPath, NewProcessParameters, only.current = FALSE) {
    
    # Get names of the process parameters:
    validProcessParameterNames = names(getRstoxFrameworkDefinitions("processParameters"))
    
    # Report a warning for non-existing process parameters:
    valid <- names(NewProcessParameters) %in% validProcessParameterNames
    if(any(!valid)) {
        warning("The following process parameters are not valid: ", paste(names(NewProcessParameters)[!valid], collapse = ", "))
        NewProcessParameters <- NewProcessParameters[valid]
    }
    
    # Insert the process parameters:
    for(ind in seq_along(NewProcessParameters)) {
        projectDescription[[ModelName]][[ProcessName]]$ProcessParameters[[names(NewProcessParameters[ind])]] <- NewProcessParameters[[ind]]
    }
    
    # Store the changes:
    setProjectDescriptionAsCurrent(ProcessName, projectDescription, only.current = only.current)
}

ModifyProcess <- function(ProcessName, ModelName, ProjectPath, NewValues, only.current = FALSE) {
    
    # The values of the process must be changed in the following order:
    # 1. Function name
    # 2. Function parameters
    # 2. Function inputs
    # 1. Process name
    # 1. Process parameters
    
    # Process name:
    if(length(NewValues$NewProcessName)) {
        ModifyProcessName(
            ProcessName = ProcessName, 
            ModelName = ModelName, 
            ProjectPath = ProjectPath, 
            NewProcessName = NewValues$ProcessName, 
            only.current = only.current
        )
    }
    
    # Process parameters:
    if(length(NewValues$ProcessParameters)) {
        ModifyProcessParameters(
            ProcessName = ProcessName, 
            ModelName = ModelName, 
            ProjectPath = ProjectPath, 
            NewProcessParameters = NewValues$ProcessParameters, 
            only.current = only.current
        )
    }
    
    # Function name:
    if(length(NewValues$FunctionName)) {
        ModifyFunctionName(
            ProcessName = ProcessName, 
            ModelName = ModelName, 
            ProjectPath = ProjectPath, 
            NewFunctionName = NewValues$FunctionName, 
            only.current = only.current
        )
    }
    
    # Function parameters:
    if(length(NewValues$NewFunctionParameters)) {
        ModifyFunctionParameters(
            ProcessName = ProcessName, 
            ModelName = ModelName, 
            ProjectPath = ProjectPath, 
            NewFunctionParameters = NewValues$FunctionParameters, 
            only.current = only.current
        )
    }
    
    # Function inputs:
    if(length(NewValues$NewFunctionInputs)) {
        ModifyFunctionInputs(
            ProcessName = ProcessName, 
            ModelName = ModelName, 
            ProjectPath = ProjectPath, 
            NewFunctionInputs = NewValues$FunctionInputs, 
            only.current = only.current
        )
    }
}




# This function checks that 
checkModel <- function(ProjectPath, ModelName) {
    
    # Function to check that the function inputs of one process are all process names existing prior to that function:
    checkFunctionInputs <- function(ind, functionInputs, processNames) {
        all(functionInputs %in% processNames[seq_len(ind - 1)])
    }
    
    # Get the processes of the model:
    processes <- getProcesses(ProjectPath, ModelName)
    
    # (1) Check process names:
    processNames <- names(processes)
    duplicatedProcessNames <- processNames[duplicated(processNames)]
    if(length(duplicatedProcessNames)) {
        message("The following process names are not unique: ", paste(duplicatedProcessNames))
        return(FALSE)
    }
    
    # (2) Check that all function inputs are existing process names prior to the current process:
    functionInputs <- lapply(processes, "[[", "FunctionInputs")
    areValidFunctionInputs <- sapply(seq_along(functionInputs), checkFunctionInputs, functionInputs = functionInputs, processNames = processNames)
    processesWithInvalidFunctionInputs <- processNames[!areValidFunctionInputs]
    if(length(processesWithInvalidFunctionInputs)) {
        message("The following processes have function inputs that are not the name of a prior process: ", paste(processesWithInvalidFunctionInputs))
        return(FALSE)
    }
}


# Function to update all relevant function inputs of a model to the new process name when a process has changed name:
updateFunctionInputs <- function() {
    
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

# Function 1:
CreateBaselineProcess <- function(
    ProcessName = NULL, 
    FunctionName = NULL, 
    ProcessParameters = list(
        Enabled = TRUE, 
        BreakInGUI = FALSE, 
        FileOutput = TRUE
    ),
    ProcessData = list(), 
    FunctionParameters = list(), 
    FunctionInputs = list()
    ) {
    
    process <- list(
        ProcessName = ProcessName, 
        ProcessParameters = ProcessParameters, 
        ProcessData = ProcessData, 
        FunctionName = FunctionName, 
        FunctionParameters = FunctionParameters, 
        FunctionInputs = FunctionInputs
    )
    process
}


ModifyBaselineProcess <- function(
    Process,
    ProcessName = NULL, 
    FunctionName = NULL, 
    ProcessParameters = NULL,
    ProcessData = NULL, 
    FunctionParameters = NULL, 
    FunctionInputs = NULL
) {
    
    process <- list(
        ProcessName = ProcessName, 
        ProcessParameters = ProcessParameters, 
        ProcessData = ProcessData, 
        FunctionName = FunctionName, 
        FunctionParameters = FunctionParameters, 
        FunctionInputs = FunctionInputs
    )
}






GetProjectDescription <- function(ProjectPath, position = 0) {
    
}


#GetProjectDescriptionName <- 



SaveProjectDescription <- function(ProjectPath, ProjectDescription, mode = c("Memory", "File")) {
    
}
    



AddBaselineProcess <- function(
    ProcessName = NULL, 
    FunctionName = NULL, 
    ProcessParameters = list(
        Enabled = TRUE, 
        BreakInGUI = FALSE, 
        FileOutput = TRUE
    ),
    FunctionInputs = list(), 
    FunctionParameters = list()
    ) {
    
    BaselineProcess <- CreateBaselineProcess(
        ProcessName = ProcessName, 
        FunctionName = FunctionName, 
        ProcessParameters = ProcessParameters, 
        FunctionInputs = FunctionInputs, 
        FunctionParameters = FunctionParameters
    )
}







# We will build up the following infrastrukture for RstoxFramework:

# 1. Define an environment RstoxEnv as in the current Rstox
# 2. Define the lists Definitons and Projects in RstoxEnv
# 3. The function initiateRstox() defines the global settings of Rstox, such as folder structure, 




# StoX GUI needs the following:

# View output:
# 1. getProcessOutputTableCount(projectName)
# 2. getProcessOutputTableName(projectName, processName)
# 3. getProcessOutputTable(projectName, processName, tableName)

# List of processes:
# getModelTypes(projectName)
# getModelName(modelType)
# getProcessesByModelType(projectName, modelType)

# getFunctionsByModelType()
# getFunctionParameterNames()
# getFunctionParameterPossibleValues()
# getFunctionParameterDefaultValue()

# open
# save
# runModel(projectName, modelName, startProcess, endProcess)
# 



# 
getOutputFileNames <- function(processName, ProjectPath, fileExt="txt") {
    
    # Get the status of the project:
    status <- getProjectStatus(ProjectPath)
    # Get the function name from the status:
    functionName <- status[[processName]]$functionName
    # Get the process index:
    processIndex <- status[[processName]]$processIndex
    
    # Get meta information about of function:
    meta <- do.call(functionName, list())
    # Get the table names of the data type of the function:
    dataType <- meta$outputDataType
    # Get the output table names:
    outputTableNames <- "***********************************************"
        
        # Concatinate the index of the process, the process name, the data type, and the output tables:
        outputFileNames <- paste(processIndex, processName, dataType, outputTableNames, sep="_")
    
    # Append file extension:
    outputFileNames <- paste(outputFileNames, fileExt, sep=".")
    
    outputFileNames
}
# 
# 
# Status: 
#     - hasError
# - isPerformed
# 
# FromFormals: 
#     - parameterName
# - parameterDefaultValue
# - parameterPossibleValues
# - parameterDescription
# 
# 


projectDescription <- list(
    Description = "fasdvabadf", 
    Baseline = list(
		ReadAcoustic = list(
		    ProcessName = "ReadAcoustic", 
		    FunctionName = "ReadAcoustic", 
		    ProcessParameters = list(
				Enabled = TRUE, 
				BreakInGUI = FALSE, 
				FileOutput = TRUE
			), 
			ProcessData = list(), 
			FunctionParameters = list(
				FileNames = c(
					"input/acoustic/Echosounder-1618.xml", 
					"input/acoustic/Echosounder-201605.xml", 
					"input/acoustic/Echosounder-2016205.xml", 
					"input/acoustic/Echosounder-2016857.xml", 
					"input/acoustic/Echosounder-A6-2016.xml"
				)
			), 
			FunctionInputs = list(
				BioticData = "FilterBiotic", 
				Density = "AcousticDensity"
			)
		), 
		DefineStrata = list(
		    ProcessName = "DefineStrata", 
		    FunctionName = "ReadAcoustic", 
		    ProcessParameters = list(
		        Enabled = TRUE, 
		        BreakInGUI = FALSE, 
		        FileOutput = TRUE
		    ), 
		    ProcessData = list("MUKLTIPOLYGIN((25)6(6)6rger)"), 
		    FunctionParameters = list(
		        FileNames = c(
		            "input/acoustic/Echosounder-1618.xml"
		        ), 
		        UseProcessData = TRUE
		    ), 
		    FunctionInputs = list(
		        BioticData = "FilterBiotic", 
		        Density = "AcousticDensity", 
		        StoxAcousticData = NA
		    )
		), 
		StoxAcoustic = list(
		    ProcessName = "StoxAcoustic", 
		    FunctionName = "StoxAcoustic", 
		    ProcessParameters = list(
		        Enabled = TRUE, 
		        BreakInGUI = FALSE, 
		        FileOutput = TRUE
		    ), 
		    ProcessData = list(), 
		    FunctionParameters = list(
		        FileNames = c(
		            "input/acoustic/Echosounder-1618.xml", 
		            "input/acoustic/Echosounder-201605.xml", 
		            "input/acoustic/Echosounder-2016205.xml", 
		            "input/acoustic/Echosounder-2016857.xml", 
		            "input/acoustic/Echosounder-A6-2016.xml"
		        )
		    ), 
		    FunctionInputs = list(
		        BioticData = "FilterBiotic"
		    )
		)
	),

    Statistics = list(
		runBootstrap = list(
		    ProcessName = "runBootstrap", 
		    FunctionName = "runBootstrap", 
		    ProcessParameters = list(
				Enabled = TRUE, 
				FileOutput = TRUE
			), 
			FunctionParameters = list(
				bootstrapMethod = "AcousticTrawl", 
				acousticMethod = "PSU~Stratum", 
				bioticMethod = "PSU~Stratum", 
				startProcess = "TotalLengthDist", 
				endProcess = "SuperIndAbundance", 
				nboot = 50, 
				seed = 1234, 
				cores = 1
			)
		)
	),


    Reports = list(
		reportAbundance = list(
		    ProcessName = "reportAbundance", 
		    FunctionName = "reportAbundance", 
		    ProcessParameters = list(
				Enabled = TRUE, 
				FileOutput = TRUE
			), 
			FunctionParameters = list(
				var = "count", 
				grp1 = "age",
				grp2 = "sex"
			)
		)
	)

)









##############################################################
##############################################################
########## 2019-07-18, Creating the RstoxFramework: ##########
##############################################################
##############################################################




##################################################
##################################################
#' Get RstoxFramework definitions
#' 
#' This function gets vital definitions from the RstoxFramework environment.
#' 
#' @param name  An optional string vector denoting which definitions to extract.
#' @param ...   Values overriding the values of definitions.
#' 
#' @return
#' A list of vital definitions in RstoxFramework.
#' 
#' @examples
#' getRstoxFrameworkDefinitions()
#' 
#' @export
#' 
setStoxFunctionAttributes <- function(x, FunctionCategory, FunctionParameterParents, FunctionOutputDataType) {
        
    # Check that the given function category is valid:
    checkFunctionCategory(FunctionCategory)
    
    ### # Check that FunctionInputs only contains required parameters:
    ### checkFunctionInputs(FunctionInputs, fun = x)
    
    ### # Check also that the parameters to show in StoX are actual parameters:
    ### checkFunctionParametersInStoX(FunctionParametersInStoX, fun = x)
    
    # Check that output is one of the allowed data types:
    checkFunctionOutputDataType(FunctionOutputDataType)
    
    attr(x, "FunctionCategory") <- FunctionCategory
    attr(x, "FunctionParameterParents") <- FunctionParameterParents
    #attr(x, "FunctionInputs") <- FunctionInputs
    #attr(x, "FunctionParametersInStoX") <- FunctionParametersInStoX
    attr(x, "FunctionOutputDataType") <- FunctionOutputDataType
    x
}
#' 
#' @export
#' 
checkFunctionCategory <- function(FunctionCategory) {
    # Get the defined model types and match the function category against these:
    stoxModelTypes <- getRstoxFrameworkDefinitions("stoxModelTypes")
    out <- FunctionCategory %in% stoxModelTypes
    if(!out) {
        stop(paste0("FunctionCategory must be one of ", paste(stoxModelTypes, collapse = ", "), ". Was ", FunctionCategory, "."))
    }
}

checkFunctionInputs <- function(FunctionInputs, fun) {
    # Get the arguments:
    f <- formals(fun)
    # Discard any "...":
    f <- subset(f, names(f) != "...")
    # Get the empty formals
    empty <- sapply(f, is.name)
    # Get the names of the inputs:
    inputs <- names(empty)[empty]
    # Check whether all given FunctionInputs are  actual inputs (non-default parameters):
    valid <- FunctionInputs %in% inputs
    if(!all(valid)) {
        stop(paste0("FunctionInputs must all be required parameters: ", paste(inputs, collapse = ", "), ". Was ", paste(FunctionInputs, collapse = ", "), "."))
    }
}

checkFunctionParametersInStoX <- function(FunctionParametersInStoX, fun) {
    # Get the arguments:
    f <- formals(fun)
    # Discard any "...":
    f <- setdiff(names(f), "...")
    # Check whether all given FunctionInputs are  actual inputs (non-default parameters):
    valid <- FunctionParametersInStoX %in% f
    if(!all(valid)) {
        stop(paste0("FunctionParametersInStoX must all be parameters: ", paste(f, collapse = ", "), ". Was ", paste(FunctionParametersInStoX, collapse = ", "), "."))
    }
}

checkFunctionOutputDataType <- function(FunctionOutputDataType) {
    # Get the defined model types and match the function category against these:
    stoxDataTypes <- getRstoxFrameworkDefinitions("stoxDataTypes")
    out <- FunctionOutputDataType %in% stoxDataTypes
    
    if(!out) {
        stop("FunctionOutputDataType must be one of the valid data types. See getRstoxFrameworkDefinitions('stoxDataTypes')")
    }
}






##################################################
##################################################
#' Get paths to the StoX directories
#' 
#' This function gets the paths to the "stox" folder and the "project" and "reference" sub folders.
#' 
#' @param ProjectDirectory   The directory in which to put the "stox" folder, defaulted to the "workspace" folder in the home directory.
#' 
#' @return
#' A list of paths to the "stox" folder and sub folders.
#' 
#' @examples
#' getStoxSkeletonPaths()
#' 
#' @noRd
#' @seealso Use \code{\link{createStoxSkeleton}} to create the folders.
#' 
getStoxSkeletonPaths <- function(ProjectDirectory = NULL) {
	
    # If missing, set the path to the stox folder, which conatins the project folder and the reference folder:
	if(length(ProjectDirectory) == 0) {
	    ProjectDirectory <- file.path(path.expand("~"), "workspace")
	}
	
	# Get and return in a list the paths to the project folder and the reference folder:
	stox <- file.path(ProjectDirectory, "stox")
	project <- file.path(stox, "project")
	reference <- file.path(stox, "reference")
	
	list(stox = ProjectDirectory, project = project, reference = reference)
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
#' @inheritParams getStoxSkeletonPaths
#' @seealso Use \code{\link{getStoxSkeletonPaths}} to get the folder paths.
#' 
createStoxSkeleton <- function(ProjectDirectory = NULL) {
	
    # Get the paths of the StoX skeleton:
	paths <- getStoxSkeletonPaths(ProjectDirectory = ProjectDirectory)
	
	# Create the "stox" folder if missing:
	if(!file.exists()) {
		message("Creating the 'stox' dirctory in the directory ", paths$stox)
		dir.create(paths$stox, recursive = TRUE, showWarnings = FALSE)
	}
	
	# Create the directories if the "stox" folder exists:
	if(!file.exists()) {
		message("Creation failed, possibly due to missing permission. Try setting the directory in which to put the stox folder, using the parameter 'ProjectDirectory'")
	}
	else{
		# The directory paths$stox already exists:
		paths$stox <- NULL
		temp <- lapply(paths, dir.create, recursive = TRUE)
	}
	
	# Return the paths:
	paths
}




