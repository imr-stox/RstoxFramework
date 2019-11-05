# RstoxFramework functions run by Modify-processes in the GUI will return update flags
# 
# updateProcessTable
# updateProcessProperties
# updateMapData: showInMap & canShowInMap
# updateLog



# 
# 
# 
# In DefineAcousticPSU and other functions requiring specific inputs we were for a while # thinking that these inputs should define which layers to plot in the map. We divert from # this and reserve the resposibility of plotting the appropriate layers to the user!


# We use CamelCase for StoX functions and the parameters shown in StoX, for data types, models, 
# We use camelCase for everything else.

# Should we use "~" for the workspace folder? Tilde gives the Documents folder on Windows, whereas StoX 2.7 uses the user folder.
#


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
    
    ##### Packages: #####
    officialStoxLibraryPackages <- c(
        "RstoxBase", 
        "RstoxData"
        #"RstoxECA", 
        #"RstoxStatistics", 
        #"RstoxReport"
    )
    stoxLibrary <- getStoxLibrary(officialStoxLibraryPackages)
    
    ##### Data: #####
    speciesVariables <- list(
        NMDBiotic1.4 = c(
            "commonname", 
            "catchcategory", 
            "aphia", 
            "scientificname"
        ), 
        NMDBiotic3.0 = c(
            "species", 
            "noname", 
            "aphia"
        ), 
        ICESBiotic1 = c(
            "SpeciesCode"
        )
    )
    
    #### Fundamental settings of StoX: ####
    
    # Define the regular expression listing lower and upper characters, integers, underscore and dot:
    validProcessNameSet <- "[[:alnum:]_.]"
    # The prefix for new unnamed processes:
    process_Prefix <- "Process_"
    # The number of digits in the integer part of the project IDs:
    numDigitsOfProcessIntegerID <- 3
    
    # Define the valid output data classes:
    validOutputDataClasses <- c(
        "data.table", 
        "json", 
        "geojson"
    )
    
    # Define the StoX folders, data sources, model names, model display names, model descriptions, and the latter three grouped as model info:
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
    stoxModelInfo <- data.table::data.table(
        modelName = stoxModelNames, 
        displayName = stoxModelDisplayNames, 
        description = stoxModelDescriptions
    )
    
    # Define the folder structure of StoX:
    stoxFolderStructure <- list(
        stoxDataSources, 
        stoxModelNames, 
        ""
    )
    stoxFolderStructureNames <- unname(unlist(mapply(paste, stoxFolders, stoxFolderStructure, sep = "_")))
    stoxFolderStructure <- unname(unlist(mapply(file.path, stoxFolders, stoxFolderStructure)))
    stoxFolderStructure <- gsub('\\/$', '', stoxFolderStructure)
    names(stoxFolderStructure) <- stoxFolderStructureNames
    stoxFolderStructureList <- as.list(stoxFolderStructure)
    
    # Define data types which can be plotted in the map (includes also changing colour etc, such as assigned stations of an acoustic PSU):
    dataTypesToShowInMap <- c(
        StoxBioticData = "StoxBioticData", 
        StoxAcousticData = "StoxAcousticData", 
        StratumPolygon = "StratumPolygon", 
        Assignment = "Assignment", 
        AcousticPSU = "AcousticPSU", 
        SweptAreaPSU = "SweptAreaPSU"
    )
    
    # Define the data types for the map modes "stratum", "assignment" and "PSU"
    strataDataTypes <- c(
        StratumPolygon = "StratumPolygon"
    )
    assignmentDataTypes <- c(
        AcousticPSU = "AcousticPSU", 
        SweptAreaPSU = "SweptAreaPSU"
    )
    PSUDataTypes <- c(
        Assignment = "Assignment"
    )
    
    # Define the process parameters with default values, display names and descriptions:
    processParameters <- list(
        enabled = TRUE, 
        showInMap = FALSE, 
        fileOutput = TRUE
    )
    processParametersDisplayNames <- list(
        enabled = "Enabled", 
        showInMap = "Show in map", 
        fileOutput = "write to output"
    )
    processParametersDescriptions <- list(
        enabled = "Whether to execute the process or not", 
        showInMap = "Whether to show specific data from the process in the map, such as stations, EDSUs, strata, or assignment shown as colors on stations and EDSUs", 
        fileOutput = "Whether to write tab separated text files of the output data from the process"
    )
    
    # Define process property names:
    processPropertyNames <- data.table::data.table(
        name = c("process", "functionInputs", "functionParameters"), 
        displayName = c("Process", "Function inputs", "Function parameters")
    )
    
    # Define the process arguments, which define a process:
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
    
    
    #### Define the folders and paths used when a project is open: ####
    projectSessionFolder <- file.path(stoxFolders["Process"], "projectSession")
    # Sub folders:
    dataFolder <- file.path(projectSessionFolder, "data")
    GUIFolder <- file.path(projectSessionFolder, "GUI")
    projectMemoryFolder <- file.path(projectSessionFolder, "projectMemory")
    statusFolder <- file.path(projectSessionFolder, "status")
    # Return also a vector of all session folders, to generate the folder structure recursively:
    projectSessionFolderStructure <- c(
            dataFolder, 
            GUIFolder, 
            projectMemoryFolder, 
            statusFolder
    )
    
    
    #### Project description: ####
    projectRDataFile <- file.path(stoxFolders["Process"], "project.RData")
    projectXMLFile <- file.path(stoxFolders["Process"], "project.xml")
    projectSavedStatusFile <- file.path(statusFolder, "projectSavedStatus.txt")
    projectIsRunningFile <- file.path(statusFolder, "projectIsRunning.txt")
    #currentProcessFile = file.path(statusFolder, "currentProcess.txt")
    
    # Memory files:
    originalProjectMemoryFile <- file.path(projectMemoryFolder, "originalProjectMemory.rds")
    currentProjectMemoryFile <- file.path(projectMemoryFolder, "currentProjectMemory.rds")
    projectMemoryIndexFile <- file.path(projectMemoryFolder, "projectMemoryIndex.txt")
    # The file containing a table of modelName, processID and processName, where the rows are ordered by the processIndex:
    processIndexTableFile <- file.path(projectMemoryFolder, "processIndexTable.txt")
    # The file containing a table of one row holding the index of the active process for each model (columns named by the model names):
    activeProcessIDFile <- file.path(projectMemoryFolder, "activeProcessID.txt")
    # The file containing a table of one row holding the maximum process ID (sequential integer starting from 1 at the firstly generated process) for each model (columns named by the model names):
    maxProcessIntegerIDFile <- file.path(projectMemoryFolder, "maxProcessIntegerID.txt")
    
    
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
            stoxFolders = stoxFolders, 
            stoxFolderStructure = stoxFolderStructure, 
            
            # Project session:
            projectSessionFolder = projectSessionFolder, 
            dataFolder = dataFolder, 
            GUIFolder = GUIFolder, 
            projectMemoryFolder = projectMemoryFolder, 
            statusFolder = statusFolder, 
            projectSessionFolderStructure = projectSessionFolderStructure, 
            
            # Project description:
            projectRDataFile = projectRDataFile, 
            projectXMLFile = projectXMLFile, 
            projectSavedStatusFile = projectSavedStatusFile, 
            projectIsRunningFile = projectIsRunningFile, 
            #currentProcessFile = currentProcessFile, 
            originalProjectMemoryFile = originalProjectMemoryFile, 
            currentProjectMemoryFile = currentProjectMemoryFile, 
            projectMemoryIndexFile = projectMemoryIndexFile, 
            processIndexTableFile = processIndexTableFile, 
            activeProcessIDFile = activeProcessIDFile, 
            maxProcessIntegerIDFile = maxProcessIntegerIDFile
        )
    )
    
    #### Assign to RstoxEnv and return the definitions: ####
    definitionsNames <- ls()
    definitionsNames <- setdiff(definitionsNames, names(paths))
    definitions <- lapply(definitionsNames, get, pos = environment())
    names(definitions) <- definitionsNames
    
    # Add the stoxTemplates: 
    definitions$stoxTemplates <- stoxTemplates
    
    #### Create the RstoxFrameworkEnv environment, holding definitions on folder structure and all the projects. This environment cna be accesses using RstoxFramework:::RstoxFrameworkEnv: ####
    utils::globalVariables("RstoxFrameworkEnv")
    assign("RstoxFrameworkEnv", new.env(), parent.env(environment()))
    
    assign("definitions", definitions, envir=get("RstoxFrameworkEnv"))
    assign("projects", list(), envir=get("RstoxFrameworkEnv"))
    
    # Load the required packages to enable searching for formals and documentation:
    lapply(officialStoxLibraryPackages, library, character.only = TRUE)
    
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

#' 
#' @export
#'
getStoxLibrary <- function(packageNames) {
    
    # Validate the pakcages:
    packageNames <- packageNames[sapply(packageNames, validateStoxLibraryPackage)]
    # Get a list of the 'stoxFunctionAttributes' from each package:
    stoxFunctionAttributeLists <- lapply(packageNames, getStoxFunctionAttributes)
    
    # Collapse to one list:
    stoxFunctionAttributes <- unlist(stoxFunctionAttributeLists, recursive = FALSE)
    
    # Check for duplicaetd function names:
    functionNames <- names(stoxFunctionAttributes)
    packageNames <- sapply(stoxFunctionAttributes, "[[", "packageName")
    areDuplicatedFunctionNames <- duplicated(functionNames)
    
    # If there are any duplicated function names, report a warning stating which function names and from which packages:
    if(any(areDuplicatedFunctionNames)) {
        # Get the package strings as concatenations of the packages with common function names:
        packageNamesString <- as.character(
            by(
                functionNames[areDuplicatedFunctionNames], 
                packageNames[areDuplicatedFunctionNames], 
                paste, 
                collapse = ", "
            )
        )
        # Get the unique duplicated function names, and paste the packageNamesString to these:
        uniqueDuplicatedFunctionNames <- unique(functionNames[areDuplicatedFunctionNames])
        functionNamePackageNamesString <- paste0(
            uniqueDuplicatedFunctionNames, 
            "(", 
            packageNamesString, 
            ")"
        )
        
        warning("The following functions are present in several packages (package names in parenthesis): ", paste(functionNamePackageNamesString, collapse = ", "))
    }
    
    # Return the non-duplicated functions: 
    stoxFunctionAttributes[!areDuplicatedFunctionNames]
}

# Function for extracting the stoxFunctionAttributes of the package, and adding the package name and full function name (packageName::functionName) to each elements (function) of the list:
getStoxFunctionAttributes <- function(packageName) {
    
    stoxFunctionAttributes <- tryCatch(
        getExportedValue(packageName, "stoxFunctionAttributes"), 
        error = function(err) NULL
    )
    
    # Add function and package name:
    stoxFunctionAttributes <- lapply(stoxFunctionAttributes, append, list(packageName = packageName))
    stoxFunctionAttributes <- mapply(
        append, 
        stoxFunctionAttributes, 
        lapply(names(stoxFunctionAttributes), function(x) list(functionName = paste(packageName, x, sep = "::"))), 
        SIMPLIFY = FALSE
    )
    
    # Add the argument descriptions:
    argumentDescriptionFile <- file.path(system.file("extdata", package = packageName), "functionArguments.rds")
    if(file.exists(argumentDescriptionFile)) {
        
        # Read the argument descriptions:
        argumentDescriptions <- readRDS(argumentDescriptionFile)
        # Keep only the argument descriptions for functions given in the stoxFunctionAttributes:
        argumentDescriptions <- argumentDescriptions[names(argumentDescriptions) %in% names(stoxFunctionAttributes)]
        
        for(functionName in names(argumentDescriptions)) {
            stoxFunctionAttributes [[functionName]] [["functionArgumentDescription"]] <- argumentDescriptions [[functionName]]
        }
    }
    else {
        warning("The file ", argumentDescriptionFile, " does not exist.")
    }
    
    
    stoxFunctionAttributes
}

### #' 
### #' @export
### #'
### getStoxFunctionTable <- function() {
###     # Get the list of all StoX functions with attributes:
###     stoxFunctionList <- getStoxLibrary()
###     # extract a table of 
###     stoxFunctionTable <- data.table::data.table(
###         functionName = sapply(stoxFunctionList, "[[", "functionName"), 
###         packageFunctionName = sapply(stoxFunctionList, "[[", "packageName")
###     )
###     
###     stoxFunctionTable
### }
### 
### expandFunctionName <- function(functionName) {
###     
###     # Check first whether the function names are already expanded:
###     expanded <- grepl("::", functionName)
###     
###     # Match the non-expanded with the stoxFunctionTable:
###     stoxFunctionTable <- getStoxFunctionTable()
###     matches <- match(functionName[!expanded], stoxFunctionTable$functionName)
###     
###     # report a warning for the functions that were not recognized:
###     if(any(is.na(matches))) {
###         warning(
###             "The following funciton names were not expanded: ", 
###             paste(functionName[!expanded][is.na(matches)], collapse = ", ")
###         )
###     }
###     
###     # Expand the recognized functions:
###     toExpand <- which(!expanded)[!is.na(matches)]
###     cleanMatches <- matches[!is.na(matches)]
###     if(length(toExpand)) {
###         functionName[toExpand] <- paste(
###             stoxFunctionTable$packageFunctionName[cleanMatches], 
###             sep ="::"
###         )
###     }
###     
###     functionName
### }



# Function for validating a StoX function library package:
validateStoxLibraryPackage <- function(packageName) {
    
    # Get the StoX function attributes:
    stoxFunctionAttributes <- getStoxFunctionAttributes(packageName)
    
    # Return FALSE if the stox funciton attributes list does not exist:
    if(length(stoxFunctionAttributes) == 0) {
        warning("The package ", packageName, " does not export the required object 'stoxFunctionAttributes'.")
        return(FALSE)
    }
    
    # Check that all of the StoX functions are exported:
    exports <- getNamespaceExports(getNamespace(packageName))
    stoxFunctionNames <- names(stoxFunctionAttributes)
    stoxFunctionNamesPresent <- stoxFunctionNames %in% exports
    if(!all(stoxFunctionNamesPresent)) {
        warning("The package ", packageName, " specifies functions in the 'stoxFunctionAttributes' object that are not exported:\n", paste(stoxFunctionNames[!stoxFunctionNamesPresent], collapse = ", "))
        return(FALSE)
    }
    
    # This is GUI specific, and should not be used for validating a StoX funciton library package:
    ### # Get the StoX functions:
    ### stoxFunctionNames <- names(stoxFunctionAttributes)
    ### # Get the paths to the required single function PDFs:
    ### pathToSingleFunctionPDF <- getPathToSingleFunctionPDF(packageName, stoxFunctionNames)
    ### existsSingleFunctionPDF <- file.exists(pathToSingleFunctionPDF)
    ### # Return FALSE if not all single function PDFs exists:
    ### if(!all(existsSingleFunctionPDF)) {
    ###     warning("The package ", packageName, " does not contain PDF of the documentation of each of its exported StoX functions.")
    ###     return(FALSE)
    ### }
    ### 
    ### # Look also for the single function argument descriptions:
    ### pathToSingleFunctionArgumentRDS <- getPathToSingleFunctionArgumentRDS(packageName, stoxFunctionNames)
    ### existsSingleFunctionArgumentRDS <- file.exists(pathToSingleFunctionArgumentRDS)
    ### # Return FALSE if not all single function PDFs exists:
    ### if(!all(existsSingleFunctionArgumentRDS)) {
    ###     warning("The package ", packageName, " does not contain RDS of the documentation of each of its exported StoX functions.")
    ###     return(FALSE)
    ### }
    
    TRUE
}

# Functions for getting the package or function name from the full adress to a function:
getPackageNameFromPackageFunctionName <- function(functionName) {
    sub("\\::.*", "", functionName)
}
getFunctionNameFromPackageFunctionName <- function(functionName) {
    substring(functionName, regexpr("::", functionName) + 2)
}


# Function to check that the functionName refers to a valid funciton, i.e., that the function is exported from a valid package (see validateStoxLibraryPackage()), and that it is represented in the associated stoxFunctionAttributes list of that package:
validateFunction <- function(functionName) {
    
    # Expand the funciton name:
    #functionName <- expandFunctionName(functionName)
    
    # 1. Check first that the function name contains a double colon, which is the first requirement for a process:
    if(!grepl("::", functionName, fixed = TRUE)) {
        stop("The function \"", functionName, "\" does not appear to be a string of the form PACKAGENAME::FUNCTIONNAME, where PACKAGENAME is the package exporting the function with name FUNCTIONNAME.")
    }
    
    # Extract the packageName:
    packageName <- getPackageNameFromPackageFunctionName(functionName)
    
    # 2. Validate the package for use in the process:
    if(validateStoxLibraryPackage(packageName)) {
        functionName
    }
    else {
        stop("Invalid function ", functionName)
    }
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
getTemplate <- function(template) {
    # Get the templates:
    templates <- getAvaiableTemplates(list.out = TRUE)
    if(template %in% names(templates)) {
        template <- templates[[template]]
    }
    else {
        warning("Invalid template name ", template, ". Available templates are ", paste0(names(templates), collapse = ", "))
    }
    
    # Define the process IDs and return the template:
    defineProcessIDs(template)
}


defineProcessIDs <- function(projectMemory) {
    # Define the process IDs:
    numProcessesPerModel <- sapply(projectMemory, length)
    integerIDsPerModel <- lapply(numProcessesPerModel, seq_len)
    processIDs <- lapply(integerIDsPerModel, createProcessIDString)
    
    # Set the processIDs as names to the processes of the models:
    for(thisname in names(projectMemory)) {
        names(projectMemory[[thisname]]) <- processIDs[[thisname]]
    }
    
    projectMemory
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
        else {
            unlink(projectPath, recursive = TRUE, force = TRUE)
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
    
    # Get the template:
    thisTemplate <- getTemplate(template)
    
    ## Get the tempaltes:
    #templates <- getAvaiableTemplates(TRUE)
    #thisTemplate <- templates[[template]]
    #if(length(thisTemplate) == 0) {
    #    stop("The requested template does not exist. See getAvaiableTemplates() for a list of the available templates (with list.out = TRUE if y#ou wish to see what the dirrefent templates are.)")
    #}
    
    # Create the project folder structure:
    projectSkeleton <- createProjectSkeleton(projectPath, ow = ow)
    
    # Create the project session folder structure:
    createProjectSessionFolderStructure(projectPath, showWarnings = showWarnings)
    
    # Set the project memory as the selected template:
    temp <- addProcesses(
        projectPath = projectPath, 
        modelName = modelName, 
        projectMemory = thisTemplate
    )
    
    # Save the project, close it, and open:
    saveProject(projectPath)
    if(!open) {
        closeProject(projectPath, save = TRUE)
    }
    else {
        # Set the active process ID:
        initiateActiveProcessID(projectPath)
    }
    
    list(
        projectPath = projectPath, 
        projectName = basename(projectPath)
    )
}
#' 
#' @export
#' 
openProject <- function(projectPath, showWarnings = FALSE) {
    
    if(isOpenProject(projectPath)) {
        message("Project ", projectPath, "is already open.")
        out <- list(
            projectPath = projectPath, 
            projectName = basename(projectPath)
        )
        return(out)
    }
    
    projectPath <- resolveProjectPath(projectPath)
    if(length(projectPath) == 0) {
        warning("The selected projectPath is not a StoX project or a folder/file inside a StoX project.")
        return(NULL)
    }
    
    # Create the project session folder structure:
    createProjectSessionFolderStructure(projectPath, showWarnings = showWarnings)
    
    # Read the project description file:
    projectMemory <- readProjectDescription(projectPath)
    
    # Set the project memory:
    temp <- addProcesses(
        projectPath = projectPath, 
        modelName = names(projectMemory), 
        projectMemory = projectMemory
    )
    
    # Set the status of the projcet as saved:
    setSavedStatus(projectPath, status = TRUE)
    
    # Set the active process ID:
    initiateActiveProcessID(projectPath)
    
    list(
        projectPath = projectPath, 
        projectName = basename(projectPath)
    )
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
    projectSessionFolderStructure <- getProjectPaths(projectPath, "projectSessionFolder")
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
### #' 
### #' @export
### #' 
### resetProject <- function(projectPath) {
###     originalProjectDescription <- getOriginalProjectDescription(projectPath)
###     setCurrentProjectDescription(projectPath, projectDescription = originalProjectDescription)
###     saveProject(projectPath)
### }


setSavedStatus <- function(projectPath, status) {
    # Get the path to the projectSavedStatusFile:
    projectSavedStatusFile <- getProjectPaths(projectPath, "projectSavedStatusFile")
    # Write the status to the file:
    writeLines(as.character(status), projectSavedStatusFile)
}


isSaved <- function(projectPath) {
    # Get the path to the projectSavedStatusFile:
    projectSavedStatusFile <- getProjectPaths(projectPath, "projectSavedStatusFile")
    # Missing file implies not saved:
    if(!file.exists(projectSavedStatusFile)) {
        FALSE
    }
    else {
        as.logical(readLines(projectSavedStatusFile, 1))
    }
}

#' 
#' @export
#' 
isOpenProject <- function(projectPath) {
    existsFolders <- sapply(getProjectPaths(projectPath, "projectSessionFolderStructure"), file.exists)
    length(existsFolders) && all(existsFolders)
}

#' 
#' @export
#' 
isProject <- function(projectPath) {
    existsFolders <- sapply(getProjectPaths(projectPath, "stoxFolders"), file.exists)
    length(existsFolders) && all(existsFolders)
}

resolveProjectPath <- function(filePath) {
    # Move up the folder hierarchy and find the project path:
    projectPath <- filePath
    while(!isProject(projectPath)) {
        up <- dirname(projectPath)
        if(up == projectPath) {
            return(NULL)
        }
        else {
            projectPath <- up
        }
    }
    projectPath
}

#' 
#' @export
#'
getActiveProcessID <- function(projectPath, modelName) {
    # Read the active process ID for the model:
    activeProcessIDFile <- getProjectPaths(projectPath, "activeProcessIDFile")
    activeProcessIDTable <- data.table::fread(activeProcessIDFile, sep = "\t")
    activeProcessIDTable[[modelName]]
}


writeActiveProcessID <- function(projectPath, modelName, activeProcessID) {
    # Read the active process ID for the model:
    activeProcessIDFile <- getProjectPaths(projectPath, "activeProcessIDFile")
    if(!file.exists(initiateActiveProcessID)) {
        warning("The active process ID file has not been initiated.")
    }
    activeProcessIDTable <- data.table::fread(activeProcessIDFile, sep = "\t")
    activeProcessIDTable[[modelName]] <- activeProcessID
    data.table::fwrite(activeProcessIDTable, activeProcessIDFile, sep = "\t")
    activeProcessIDFile
}


initiateActiveProcessID <- function(projectPath) {
    # Read the active process ID for the model:
    activeProcessIDFile <- getProjectPaths(projectPath, "activeProcessIDFile")
    # Initiate with all zeros:
    activeProcessIDTable <- data.table::as.data.table(matrix(0, nrow = 1, ncol = 3))
    colnames(activeProcessIDTable) <-getRstoxFrameworkDefinitions("stoxModelNames")
    data.table::fwrite(activeProcessIDTable, activeProcessIDFile, sep = "\t")
    activeProcessIDFile
}





# 
resetModel <- function(projectPath, modelName, processID = 1) {
    
    # Delete the output from the processes past the activeProcessID:
    deleteProcessOutput(projectPath, modelName, processID)
    
    # Get the process ID to reset the model to:
    processIndexTable <- readProcessIndexTable(projectPath, modelName)
    processIndex <- which(processIndexTable$processID == processID)
    
    # Get the active process ID as the process ID of the process before the specified process in the processIndexTable (or NA if processIndex is 1):
    if(processIndex == 1) {
        activeProcessID <- NA
    }
    else {
        activeProcessID <- processIndexTable$processID[processIndex - 1]
    }
    
    writeActiveProcessID(projectPath, modelName, activeProcessID)
    activeProcessID
}









readProjectDescription <- function(projectPath) {
    # Get the path to the project description file:
    projectRDataFile <- getProjectPaths(projectPath, "projectRDataFile")
    load(projectRDataFile) # Creates the object
    # Define the process IDs and return the project description:
    defineProcessIDs(projectDescription)
}

writeProjectDescription <- function(projectPath) {
    # Get the current project description:
    projectDescription <- getProjectMemoryData(projectPath)
    
    # Get the path to the project description file, and save the current project description:
    projectRDataFile <- getProjectPaths(projectPath, "projectRDataFile")
    save(projectDescription, file = projectRDataFile)
}






# (1a) argumentFile:
# Every process argument (one of processName, functionName, processParameters, processData, functionInputs and functionParameters) is written as an argumentFile.
#
# (1b) argumentValue:
# The data stored in an argumentFile.
#
# (2) projectMemory:
# A vector of the file paths of the argumentFile comprising the projectMemory
#
# (3) projectMemoryFile:
# The file holding the argumentFileList. There is one argumentFileListFile for each change, which can involve multiple individal changes.
#
# (4) projectMemoryFilePath:
# The path to the file holding the argumentFileList. There are two special files named originalArgumentFileList and currentArgumentFileList. 
# 
# (5) originalProjectMemory:
# The file holding the original projectMemory as a list of files.
# 
# (5) currentProjectMemory:
# The file holding the current projectMemory as a list of files.

# (6) projectMemoryData:
# A nested list of the individual argument values with levels modelName, processID, argumentName and argumentValue




# <<<<<<<<<<<<<<<<<<<<<<<<<<<< DONE
# 5. Function to read the current project memory, or parts of it (e.g., argumentName = NULL indicate all arguments of the specified process(es)):
#' 
#' @export
#' 
getProjectMemoryData <- function(projectPath, modelName = NULL, processID = NULL, argumentName = NULL, drop1 = FALSE, type = c("current", "original")) {
    
    # Get a data.table of process argument file paths split into modelName, processID, argumentName and argumentFilePath:
    argumentFileTable <- getArgumentFileTable(projectPath, type = type)
    
    # Apply the selected model, process and argument, where NULL indicates all elements:
    TRUEvector <- !logical(nrow(argumentFileTable))
    requestedModelNames    <- if(length(modelName))    argumentFileTable$modelName    %in% modelName    else TRUEvector
    requestedprocessIDs    <- if(length(processID))    argumentFileTable$processID    %in% processID    else TRUEvector
    requestedArgumentNames <- if(length(argumentName)) argumentFileTable$argumentName %in% argumentName else TRUEvector
    requested              <- requestedModelNames & requestedprocessIDs & requestedArgumentNames
    
    # Get the requested files:
    argumentFileTable <- argumentFileTable[requested, ]
    
    # Create an empty projectDescription and read and insert the files:
    projectMemory <- list()
    for(ind in seq_len(nrow(argumentFileTable))) {
        
        # For convenience get the current modelName, processID, argumentName and argument:
        thisModelName <- argumentFileTable$modelName[ind]
        thisProcessID <- argumentFileTable$processID[ind]
        thisArgumentName <- argumentFileTable$argumentName[ind]
        thisArgumentValue <- readRDS(argumentFileTable$argumentFile[[ind]])
        
        # Append the missing list elements down to the argument:
        if(!thisModelName %in% names(projectMemory)) {
            projectMemory <- append(
                projectMemory, 
                structure(list(NULL), names = thisModelName)
            )
        }
        if(!thisProcessID %in% names(projectMemory [[thisModelName]])) {
            projectMemory [[thisModelName]] <- append(
                projectMemory[[thisModelName]], 
                structure(list(NULL), names = thisProcessID)
            )
        }
        # If missing, append the argument, and if not replace it:
        if(!thisArgumentName %in% names(projectMemory [[thisModelName]] [[thisProcessID]])) {
            projectMemory [[thisModelName]] [[thisProcessID]] <- append(
                projectMemory [[thisModelName]] [[thisProcessID]], 
                structure(list(thisArgumentValue), names = thisArgumentName)
            )
        }
        else {
            projectMemory [[thisModelName]] [[thisProcessID]] [[thisArgumentName]] <- thisArgumentValue
        }
        
        
        
        # Using the "[[" operator does not generate the lists recursively like the "$" operator does, so we need to generate the neste structure when needed:
        #if(length(projectMemory[argumentFileTable$modelName[ind]]) == 0) {
        #    projectMemory[[argumentFileTable$modelName[ind]]] <- list()
        #}
        #if(length(projectMemory[[argumentFileTable$modelName[ind]]] [argumentFileTable$processID[ind]]) == 0) {
        #    projectMemory[[argumentFileTable$modelName[ind]]] [[argumentFileTable$processID[ind]]] <- list()
        #}
        ## Insert the argumentValue:
        #projectMemory[[argumentFileTable$modelName[ind]]] [[argumentFileTable$processID[ind]]] [[argumentFileTable$argumentName[ind]]] <- readRDS(argumentFileTable$argumentFile[[ind]])
    }
    
    # Drop the levels with only one elements if requested:
    if(drop1) {
        if(length(modelName) == 1) {
            projectMemory <- projectMemory[[modelName]]
        }
        if(length(processID) == 1) {
            projectMemory <- projectMemory[[processID]]
        }
        if(length(argumentName) == 1) {
            projectMemory <- projectMemory[[argumentName]]
        }
        #projectMemory <- unlist1(projectMemory)
    }
    
    # Return the list containing the requested project memory objects:
    projectMemory
}
# >>>>>>>>>>>>>>>>>>>>>>>>>>>> DONE


# <<<<<<<<<<<<<<<<<<<<<<<<<<<< DONE
# Read the process argument files to a list of the elements modelName, processID, argumentName, argumentValue:
getArgumentFileTable <- function(projectPath, modelName = NULL, processID = NULL, type = c("current", "original")) {
    
    # Read the current project memory file, which contains the list of files holding the current process arguments:
    projectMemoryFile <- getProjectPaths(projectPath, paste0(type[1], "ProjectMemoryFile"))
    
    # If the projectMemoryFile does not exist, return an empty data.table:
    if(file.exists(projectMemoryFile)) {
        # Read the projectMemoryFile:
        argumentFileTable <- readRDS(projectMemoryFile)
        # Subset out the model if requested:
        if(length(modelName)) {
            argumentFileTable <- subset(argumentFileTable, modelName == modelName)
        }
        if(length(processID)) {
            argumentFileTable <- subset(argumentFileTable, processID == processID)
        }
    }
    else {
        argumentFileTable <- data.table()
    }
    
    argumentFileTable
}
# >>>>>>>>>>>>>>>>>>>>>>>>>>>> DONE





# <<<<<<<<<<<<<<<<<<<<<<<<<<<< DONE
# Function for getting the file path of one specific process argument rds file:
getNewArgumentFile <- function(projectPath, modelName, processID, argumentName) {
    
    # Get the folder holding the project descriptions:
    projectMemoryFolder <- getProjectPaths(projectPath, "projectMemoryFolder")
    argumentFolder <- file.path(projectMemoryFolder, modelName, processID, argumentName)
    
    # Define a string with time in ISO 8601 format:
    timeString <- format(Sys.time(), tz = "UTC", format = "%Y%m%dT%H%M%OS3Z")
    # Define the file name including the time string, and build the path to the file:
    fileName <- paste0(argumentName, "_", timeString, ".rds")
    filePath <- file.path(argumentFolder, fileName)
    filePath
}
# >>>>>>>>>>>>>>>>>>>>>>>>>>>> DONE

# <<<<<<<<<<<<<<<<<<<<<<<<<<<< DONE
# Function for getting the file path of a new project memory file:
getNewProjectMemoryFile <- function(projectPath) {
    # Get the folder holding the project descriptions:
    projectMemoryFolder <- getProjectPaths(projectPath, "projectMemoryFolder")
    
    # Define a string with time in ISO 8601 format:
    timeString <- format(Sys.time(), tz = "UTC", format = "%Y%m%dT%H%M%OS3Z")
    # Define the file name including the time string, and build the path to the file:
    fileName <- paste0("projectMemory", "_", timeString, ".rds")
    filePath <- file.path(projectMemoryFolder, fileName)
    filePath
}
# >>>>>>>>>>>>>>>>>>>>>>>>>>>> DONE

# <<<<<<<<<<<<<<<<<<<<<<<<<<<< DONE
# Function for getting the file path of a current project memory file:
getCurrentProjectMemoryFile <- function(projectPath) {
    getProjectPaths(projectPath, "currentProjectMemoryFile")
}
# >>>>>>>>>>>>>>>>>>>>>>>>>>>> DONE

# <<<<<<<<<<<<<<<<<<<<<<<<<<<< DONE
# Function for getting the file path of a original project memory file:
getOriginalProjectMemoryFile <- function(projectPath) {
    getProjectPaths(projectPath, "originalProjectMemoryFile")
}
# >>>>>>>>>>>>>>>>>>>>>>>>>>>> DONE



# <<<<<<<<<<<<<<<<<<<<<<<<<<<< DONE
# Function for saving an argument value to one process argument rds file:
saveArgumentFile <- function(projectPath, modelName, processID, argumentName, argumentValue) {
    
    # Get the path to the new argument file:
    argumentFile <- getNewArgumentFile(projectPath, modelName, processID, argumentName)
    if(!file.exists(dirname(argumentFile))) {
        dir.create(dirname(argumentFile), showWarnings = FALSE, recursive = TRUE)
    }
    # Save the argument to the file, and return the file path:
    saveRDS(argumentValue, file = argumentFile)
    
    # Return a list with the modelName, processID, argumentName and argumentFile:
    # data.table::data.table(
    #     modelName = modelName, 
    #     processID = processID, 
    #     argumentName = argumentName, 
    #     argumentFile = argumentFile
    # )
    argumentFile
}
# >>>>>>>>>>>>>>>>>>>>>>>>>>>> DONE

# 6. Function to write the current project description:
#' 
#' @export
#' 
setProcessMemory <- function(projectPath, modelName, processID, argumentName, argumentValue, process = NULL) {
    
    # Get the arguments and argument names from the process:
    if(length(process)) {
        argumentName <- names(process)
        argumentValue <- process
    }
    
    # Save all project arguments to files (shorter repeated to the longest):
    newArgumentFiles <- mapply(saveArgumentFile, projectPath, modelName, processID, argumentName, argumentValue)
    
    # Get the current table of process argument files:
    argumentFileTable <- getArgumentFileTable(projectPath)
    
    # Modify the argument file table with the new files:
    argumentFileTable <- insertToArgumentFileTable(
        argumentFileTable = argumentFileTable, 
        modelName = modelName, 
        processID = processID, 
        argumentName = argumentName, 
        argumentFile = newArgumentFiles
    )
    
    # Save the project memory:
    saveProjectMemory(projectPath, argumentFileTable)
}
#' 
#' @export
#' 
removeProcessMemory <- function(projectPath, modelName, processID) {
    
    # Get the current table of process argument files:
    argumentFileTable <- getArgumentFileTable(projectPath)
    
    # Remove the process from the argument file table:
    argumentFileTable <- removeFromArgumentFileTable(argumentFileTable, modelName, processID)
    
    # Save the project memory:
    saveProjectMemory(projectPath, argumentFileTable)
}


insertToArgumentFileTable <- function(argumentFileTable, modelName, processID, argumentName, argumentFile) {
    
    # Function to get the row index of a combination of values of the columns of argumentFileTable:
    getRowIndex <- function(ind, argumentFilesToInsert, argumentFileTable) {
        # Get the indices at which to insert the row of argumentFilesToInsert:
        atModelName    <- argumentFilesToInsert$modelName[ind]    == argumentFileTable$modelName
        atProcessID    <- argumentFilesToInsert$processID[ind]    == argumentFileTable$processID
        atArgumentName <- argumentFilesToInsert$argumentName[ind] == argumentFileTable$argumentName
        index <- which(atModelName & atProcessID & atArgumentName)
        # Return NA for missing indices:
        if(length(index) == 0) {
            index <- NA
        }
        index
    }
    
    # Define a data.table of the same form as the argumentFileTable, with the data to insert:
    argumentFilesToInsert <- data.table::data.table(
        modelName = modelName, 
        processID = processID, 
        argumentName = argumentName, 
        argumentFile = argumentFile
    )
    
    # Identify the row which are not present in the current argumentFileTable:
    index <- sapply(
        seq_len(nrow(argumentFilesToInsert)), 
        getRowIndex, 
        argumentFilesToInsert = argumentFilesToInsert, 
        argumentFileTable = argumentFileTable
    )
    
    # Detect the new files:
    additions <- is.na(index)
    
    # Replace the argument files:
    argumentFileTable[index[!additions], "argumentFile"] <- argumentFilesToInsert[!additions, "argumentFile"]
    
    # Append the new argument files:
    argumentFileTable <- rbind(
        argumentFileTable, 
        argumentFilesToInsert[additions, ], 
        fill = TRUE
    )
    
    # Return the modified table:
    argumentFileTable
}

removeFromArgumentFileTable <- function(argumentFileTable, modelName, processID) {
    
    # Get the rows in the argument file table to remove, which are those with processID as that specified by the user to remove:
    toRemove <- argumentFileTable$processID == processID & argumentFileTable$modelName == modelName
    
    # Remove the argument files of the process:
    if(any(toRemove)) {
        argumentFileTable <- argumentFileTable[!toRemove, ]
    }
    else {
        warning("The process with processID ", processID, " was not found in the current state of the model")
    }
    
    # Return the argument file table:
    argumentFileTable
}

# Funciton for saving an argument file table (defining the process memory files comprising the process memory):
saveProjectMemory <- function(projectPath, argumentFileTable) {
    # Save the list of project argument files to the current project description file and to the new project description file:
    currentProjectMemoryFile <- getCurrentProjectMemoryFile(projectPath)
    newProjectMemoryFile     <- getNewProjectMemoryFile(projectPath)
    saveRDS(argumentFileTable, file = currentProjectMemoryFile)
    saveRDS(argumentFileTable, file = newProjectMemoryFile)
    
    # Update the projectDescriptionIndexFile:
    projectMemoryIndex <- readProjectMemoryIndex(projectPath)
    
    # Delete any files with positive index:
    hasPositiveIndex <- projectMemoryIndex$Index > 0
    if(any(hasPositiveIndex)) {
        unlink(projectMemoryIndex$Path[hasPositiveIndex])
        projectMemoryIndex <- projectMemoryIndex[!hasPositiveIndex, ]
    }
    # Subtract 1 from the indices, and add the new project description file path:
    projectMemoryIndex$Index <- projectMemoryIndex$Index - 1
    projectMemoryIndex <- rbind(
        projectMemoryIndex, 
        data.table::data.table(
            Index = 0, 
            Path = newProjectMemoryFile
        ), 
        fill = TRUE
    )
    # Write the projectDescriptionIndex to file:
    writeProjectMemoryIndex(projectPath, projectMemoryIndex)
    
    # Return the new project description file path:
    newProjectMemoryFile
}









# <<<<<<<<<<<<<<<<<<<<<<<<<<<< DONE
# Split a projectMemoryData object to a list of the elements modelName, processID, argumentName, argumentValue:
splitProjectMemoryList <- function(projectMemoryData) {
    
    # Unlist the projectMemory twice to reach the process argument level, then extract the names splitting by dot (as unlist concatenates the names with dot as separator):
    projectMemoryData <- unlist(unlist(projectMemoryData, recursive = FALSE), recursive = FALSE)
    model_process_argument <- names(projectMemoryData)
    model_process_argument <- strsplit(model_process_argument, ".", fixed = TRUE)
    # Extract the modelName, processID and argumentName
    modelName <- sapply(model_process_argument, "[[", 1)
    processID <- sapply(model_process_argument, "[[", 2)
    argumentName <- sapply(model_process_argument, "[[", 3)
    
    # Create a data.table with the modelName, processID, argumentName and argumentValue (the latter may be a list):
    data.table::data.table(
        modelName = modelName, 
        processID = processID, 
        argumentName = argumentName, 
        argumentValue = unname(projectMemoryData)
    )
}
# >>>>>>>>>>>>>>>>>>>>>>>>>>>> DONE


# 7.1 Function to read the projectDescriptionIndexFile:
#' 
#' @export
#' 
readProjectMemoryIndex <- function(projectPath) {
    # Read the projectMemoryIndexFile:
    projectMemoryIndexFile <- getProjectPaths(projectPath, "projectMemoryIndexFile")
    
    # If missing, create the file as an empty file:
    if(!file.exists(projectMemoryIndexFile)) {
        NULL
    }
    else {
        data.table::fread(projectMemoryIndexFile, sep = "\t")
    }
}

# 7.2 Function to write the projectDescriptionIndexFile:
#' 
#' @export
#' 
writeProjectMemoryIndex <- function(projectPath, projectMemoryIndex) {
    # Read the projectDescriptionIndexFile:
    projectMemoryIndexFile <- getProjectPaths(projectPath, "projectMemoryIndexFile")
    data.table::fwrite(projectMemoryIndex, file =  projectMemoryIndexFile, sep = "\t")
}



# 7. Function to undo or redo, i.e., reset the current project description file and change the indices. There will be separate GUI functions for undo and redo:
#' 
#' @export
#' 
unReDoProject <- function(projectPath, shift = 0) {
    # Read the projectDescriptionIndexFile, and add the shift value to the index:
    projectMemoryIndex <- readProjectMemoryIndex(projectPath)
    projectMemoryIndex$Index <- projectMemoryIndex$Index - shift
    writeProjectMemoryIndex(projectPath, projectMemoryIndex)
    
    # Copy the projectMemory with index = 0 to the currentProjectMemoryFile:
    fileWithCurrentProjectMemory  <- projectMemoryIndex$Path[projectMemoryIndex$Index == 0]
    file.copy(
        from = fileWithCurrentProjectMemory, 
        to = getProjectPaths(projectPath, "currentProjectMemoryFile"), 
        overwrite = TRUE, 
        copy.date = TRUE
    )
}













##################################
##### StoX function library: #####
##################################

#getStoxFunctionAttributes <- function(packageName) {
#    package <- paste0("package", packageName)
#    get("stoxFunctionAttributes", pos = package)
#}

# Function returning the names of the StoX functions available for a model:
#' 
#' @export
#' 
getAvailableStoxFunctionNames <- function(modelName) {
    
    # Get the function meta data:
    stoxLibrary <- getRstoxFrameworkDefinitions("stoxLibrary")
    
    # Get the categories:
    stoxModelNames <- getRstoxFrameworkDefinitions("stoxModelNames")
    # Get the names of the available functions:
    availableFunctions <- names(stoxLibrary)
    # Get the category of each function, and split by category:
    functionCategories <- sapply(stoxLibrary, "[[", "functionCategory")
    availableFunctionsByCategory <-split(availableFunctions, functionCategories)
    
    # Keep only the valid category:
    availableFunctionsByCategory[modelName]
}

# Function for getting specific metadata of a function, or all metadata if metaDataName = NULL:
getStoxFunctionMetaData <- function(functionName, metaDataName = NULL) {
    
    # Get the function name (without package name ::):
    functionName <- getFunctionNameFromPackageFunctionName(functionName)
    
    # Get the function meta data:
    stoxLibrary <- getRstoxFrameworkDefinitions("stoxLibrary")
    # Match the metaDataName with the available meta data and return:
    if(length(metaDataName) == 0) {
        stoxLibrary [[functionName]]
    }
    else if(metaDataName %in% names(stoxLibrary[[functionName]])) {
        stoxLibrary [[functionName]] [[metaDataName]]
    }
    else {
        warning("The requested meta data ", metaDataName, " is not included in the stoxFunctionAttributes.")
        NULL
    }
}


getArgumentsToShow <- function(functionName, functionArguments, functionArgumentHierarchy) {
    # Loop through the arguments given by paret tags in the functionArgumentHierarchy, and set toShow to FALSE if not any of the criterias are fulfilled:
    toShow <- !logical(length(functionArguments))
    names(toShow) <- names(functionArguments)
    for(argumentName in names(functionArgumentHierarchy)) {
        # Check the function arguments against the values in the function argument hierarchy:
        fullfilled <- functionArguments[names(functionArgumentHierarchy[[argumentName]])] == unlist(functionArgumentHierarchy[[argumentName]])
        if(!any(fullfilled)) {
            toShow[[argumentName]] <- FALSE
        }
    }
}



### getStoxFunctionOutputDataType <- function(functionName) {
###     # Get the function name (without package name ::):
###     functionName <- getFunctionNameFromPackageFunctionName(functionName)
###     # Extract the function output data type:
###     stoxLibrary <- getRstoxFrameworkDefinitions("stoxLibrary")
###     stoxLibrary[[functionName]]$functionOutputDataType
### }




isProcessDataFunction <- function(functionName) {
    # Get the function output data type and match against the defined process data types:
    functionOutputDataType <- getStoxFunctionMetaData(functionName, "functionOutputDataType")
    functionOutputDataType %in% getRstoxFrameworkDefinitions("stoxProcessDataTypes")
}



# No longer used. This funciton is replaced by getStoxFunctionParameters() and getStoxFunctionInputs()
### getParametersToUseInStoX <- function(functionName) {
###     # Get the funciton parameter hierarchy:
###     functionParameterHierarchy <- getFunctionParameterHierarchy(functionName)
###     # ... the names of which are the parameters to use in StoX:
###     names(functionParameterHierarchy)
### }

# Function which gets the values defined for the parameters in the definition of a function:
getStoxFunctionParameterPossibleValues <- function(functionName, dropProcessData = TRUE) {
    
    # Split the function name into function name and package name, and get the formals in the package environment:
    packageFunctionName <- strsplit(functionName, "::")[[1]]
    if(length(packageFunctionName) == 1) {
        f <- formals(functionName)
    }
    else {
        packageName <- packageFunctionName[1]
        functionName <- packageFunctionName[2]
        f <- formals(functionName, envir = as.environment(paste("package", packageName, sep = ":")))
    }
    
    # Convert missing inputs to NULL, to preserve the name-value-pair convention, and to allow evaluating the calls returned by formals():
    areMissing <- sapply(f, class) == "name" & sapply(f, function(x) length(x) > 0 & sum(nchar(x)) == 0)
    f[areMissing] <- vector("list", sum(areMissing))
    
    if(dropProcessData) {
        f <- f[names(f) != "processData"]
    }
    
    # Evaluate and return:
    f <- lapply(f, eval)
    f
}

# Function which gets the default values of a function:
getStoxFunctionParameterDefaults <- function(functionName) {
    # Get the possible values of the parameters of a function:
    functionParameterPossibleValues <- getStoxFunctionParameterPossibleValues(functionName)
    # The default is the first value:
    defaults <- lapply(functionParameterPossibleValues, utils::head, 1)
    defaults
}

# Function which gets the primitive types of the parameters of a function:
getStoxFunctionParameterPrimitiveTypes <- function(functionName) {
    # Get the possible values of the parameters of a function:
    functionParameterPossibleValues <- getStoxFunctionParameterPossibleValues(functionName)
    # The default is the first value:
    primitiveType <- lapply(functionParameterPossibleValues, class)
    primitiveType
}
# Function which gets the primitive types of the parameters of a function:
getFunctionParameterPropertyItemTypes <- function(functionName) {
    # Get the primitive types of the parameters of a function:
    stoxFunctionParameterPrimitiveTypes <- getStoxFunctionParameterPrimitiveTypes(functionName)
    
    # If not integer, double or logical, set to character (as all other types than these are wrapped in JSON strings):
    setAsCharacter <- stoxFunctionParameterPrimitiveTypes %in% c("integer", "numeric", "logical")
    stoxFunctionParameterPrimitiveTypes[setAsCharacter] <- "character"
    stoxFunctionParameterPrimitiveTypes
}










#############################################################
##### Functions for extracting properties of processes: #####
#############################################################
getFunctionName <- function(projectPath, modelName, processID) {
    getProjectMemoryData(
        projectPath, 
        modelName = modelName, 
        processID = processID, 
        argumentName = "functionName", 
        drop1 = TRUE
    )
}

getFunctionInputs <- function(projectPath, modelName, processID) {
    getProjectMemoryData(
        projectPath, 
        modelName = modelName, 
        processID = processID, 
        argumentName = "functionInputs", 
        drop1 = TRUE
    )
}

getFunctionParameters <- function(projectPath, modelName, processID) {
    getProjectMemoryData(
        projectPath, 
        modelName = modelName, 
        processID = processID, 
        argumentName = "functionParameters", 
        drop1 = TRUE
    )
}

getProcessName <- function(projectPath, modelName, processID) {
    getProjectMemoryData(
        projectPath, 
        modelName = modelName, 
        processID = processID, 
        argumentName = "processName", 
        drop1 = TRUE
    )
}

getProcessParameters <- function(projectPath, modelName, processID) {
    getProjectMemoryData(
        projectPath, 
        modelName = modelName, 
        processID = processID, 
        argumentName = "processParameters", 
        drop1 = TRUE
    )
}

getProcessData <- function(projectPath, modelName, processID) {
    getProjectMemoryData(
        projectPath, 
        modelName = modelName, 
        processID = processID, 
        argumentName = "processData", 
        drop1 = TRUE
    )
}

getProcess <- function(projectPath, modelName, processID) {
    getProjectMemoryData(
        projectPath, 
        modelName = modelName, 
        processID = processID, 
        drop1 = TRUE
    )
}

getDataType <- function(projectPath, modelName, processID) {
    # Get the function name:
    functionName <- getFunctionName(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID
    )
    # Get the data type from the function name:
    getStoxFunctionMetaData(functionName, "functionOutputDataType")
}
    

##### Functions for manipulating the process index table, which defines the order of the processes. These functions are used by the frontend to delete, add, and reorder processes: #####
readProcessIndexTable <- function(projectPath, modelName) {
    # Get the path to the process index file:
    processIndexTableFile <- getProjectPaths(projectPath, "processIndexTableFile")
    
    # If missing, create the file as an empty file:
    if(!file.exists(processIndexTableFile)) {
        data.table::data.table()
    }
    # Otherwise read the table from the file:
    else {
        processIndexTable <- data.table::fread(processIndexTableFile, sep = "\t")
        validRows <- processIndexTable$modelName %in% modelName
        subset(processIndexTable, validRows)
    }
}

writeProcessIndexTable <- function(projectPath, modelName, processIndexTable) {
    # Get the path to the process index file:
    processIndexTableFile <- getProjectPaths(projectPath, "processIndexTableFile")
    # write the file:
    processIndexTable <- data.table::fwrite(processIndexTable, processIndexTableFile, sep = "\t")
}


addToProcessIndexTable <- function(projectPath, modelName, processID, processName, afterIndex = NULL) {
    
    # Get the process index file:
    processIndexTable <- readProcessIndexTable(projectPath = projectPath, modelName = modelName)
    
    # Get the default 'afterIndex':
    nrowProcessIndexTable <- nrow(processIndexTable)
    if(length(afterIndex) == 0) {
        afterIndex <- nrowProcessIndexTable
    }
    
    # Add the process ID and as the process after 'afterIndex':
    before <- processIndexTable[seq_len(afterIndex), ]
    new <- data.table::data.table(
        processID = processID, 
        processName = processName, 
        modelName = modelName
    )
    if(afterIndex < nrowProcessIndexTable) {
        after <- processIndexTable[seq(afterIndex + 1, nrowProcessIndexTable), ]
    }
    else {
        after <- NULL
    }
    
    # Build the new processIndexTable:
    processIndexTable <- rbind(
        before, 
        new, 
        after
    )
    
    # Write the file:
    writeProcessIndexTable(projectPath = projectPath, modelName = modelName, processIndexTable = processIndexTable)
}


removeFromProcessIndexTable <- function(projectPath, modelName, processID) {
    # Get the process index file:
    processIndexTable <- readProcessIndexTable(projectPath = projectPath, modelName = modelName)
    
    # Remove the process:
    processIndexTable <- processIndexTable[processID != processID, ]
    
    # Write the file:
    writeProcessIndexTable(projectPath = projectPath, modelName = modelName, processIndexTable = processIndexTable)
}

rearrangeProcessIndexTable <- function(projectPath, modelName, processIDs, afterProcessID) {
    # Get the process index file:
    processIndexTable <- readProcessIndexTable(projectPath = projectPath, modelName = modelName)
    
    # Add the process ID and as the process after 'after':
    rearranged <- processIndexTable[processIDs %in% processIDs, ]
    rest <- processIndexTable[!processIDs %in% processIDs, ]
    afterProcessIndexInRest <- which(rest$processIDs == afterProcessID)
    before <- rest[seq_len(afterProcessIndexInRest), ]
    if(after < nrowProcessIndexTable) {
        after <- rest[seq(afterProcessIndexInRest + 1, nrow(rest)), ]
    }
    else {
        after <- NULL
    }
    
    # Build the new processIndexTable:
    processIndexTable <- rbind(
        before, 
        rearranged, 
        after
    )
    
    # Write the file:
    writeProcessIndexTable(projectPath = projectPath, modelName = modelName, processIndexTable = processIndexTable)
}



getProcessIDFromProcessName <- function(projectPath, modelName, processName) {
    # Get the table linking process names and IDs:
    processIndexTable <- readProcessIndexTable(
        projectPath = projectPath, 
        modelName = modelName
    )
    # Extract the requested process ID:
    validRow <- processIndexTable$processName == processName
    processIndexTable[validRow]$processID
}


#' 
#' @export
#' 
getProcessTable <- function(projectPath, modelName) {
    
    # Get a table of process name and ID:
    processIndexTable <- readProcessIndexTable(projectPath, modelName)
    
    # Return an empty data.table if the processIndexTable is empty:
    if(nrow(processIndexTable) == 0) {
        return(data.table::data.table())
    }
        
    # Get the function names for use when determining the 'canShowInMap' and 'hasProcessData'
    functionNames <- mapply(
        getFunctionName, 
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processIndexTable$processID
    )
    
    # Check whether the data type can be shown in the map:
    canShowInMap <- getCanShowInMap(functionNames)
    
    # Check whether the user has defined that the data from the process should be shown in the map:
    processParameters <- mapply(
        getProcessParameters, 
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processIndexTable$processID, 
        SIMPLIFY = FALSE
    )
    processParameters <- data.table::rbindlist(processParameters)
    
    # Check whether the process returns process data:
    hasProcessData <- sapply(functionNames, isProcessDataFunction)
    
    # Group the info to a table for now:
    processTable <- cbind(
        processIndexTable, # Contains processName and processID
        data.table::data.table(
            functionName = functionNames, 
            canShowInMap = canShowInMap
        ), 
        processParameters, 
        data.table::data.table(
            hasProcessData = hasProcessData
        )
    )
        
    # Get the funciton inputs (as a list):
    functionInputs <- mapply(
        getFunctionInputs, 
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processIndexTable$processID
    )
    # Get the processes that has errors:
    hasModelError <- sapply(
        seq_along(processIndexTable$processID), 
        getFunctionInputErrors, 
        processTable = processTable, 
        functionInputs = functionInputs
    )
    processTable$hasModelError <- hasModelError
    
    # Add the data type:
    processTable$dataType <- sapply(functionNames, getStoxFunctionMetaData, "functionOutputDataType")
    
    ### processTable$functionExists <- functionExists(functionNames)
    
    # Reads a table of the following columns:
    # 1. processName
    # 4. canShowInMap
    # 5. hasProcessData
    # 6. doShowInMap
    
    # 2. hasBeenRun
    # 3. hasError
    
    # 
    # There are two different types of actions, changing processes and changing parameters. Changing processes iduces reset of current process, whereas changing parameters do not. This will be added to the projectDescriptionIndex.txt. Errors given by HasError only occur when there are missing inputs, that is that the processes requersted in funciton inputs do not exist BEFORE the actual function. This will be a check to run in the route-funcitons Add-, Delete- and MoreProcess, which call the corresponding add-, delete- and moreProcess in Framework.R, and then calls getProjectList.
    
    processTable
}




isFunctionInput <- function(parameter) {
    # Get the valid data types (model data and process data), and check whether the inputs are in these:
    stoxDataTypes <- getRstoxFrameworkDefinitions("stoxDataTypes")
    parameter %in% stoxDataTypes
}


createEmptyProcess <- function(modelName = "Baseline", processName = NULL) {
    # Get the default process with empty fields for project and function name, process data, and function parameters and inputs:
    process <- getRstoxFrameworkDefinitions("processDefault")[[modelName]]
    # Possibly add the given process name (this is done here since creating a default process name is not always needed or wanted):
    if(length(processName) || is.na(processName) || nchar(processName) == 0) {
        process$processName <- processName
    }
    process
}


# Function to detect which of the process parameters to include/exclude:
getPossibleProcessParameterNames <- function() {
    # Before this funciton was functionName specific, but all process parameters are included for all processes, and then irrelevant ones are hidden in process properies in StoX.
    # getPossibleProcessParameterNames <- function(functionName) {
        
    # Get the possible process parameters:
    processParameters <- getRstoxFrameworkDefinitions("processParameters")
    possibleProcessParameters <- names(processParameters)
    
    ## Remove "showInMap" if relevant:
    #if(!getCanShowInMap(functionName)) {
    #    possibleProcessParameters <- setdiff(possibleProcessParameters, "showInMap")
    #}
    
    # Return the vector of process parameters names:
    possibleProcessParameters
}





# This funciton is quite central, as it is resposible of setting the default values of funcitons. Only the function inputs and parameters introduced to a process using setFunctionName() can be modified:
setFunctionName <- function(process, newFunctionName) {
    
    # Validate functionName:
    newFunctionName <- validateFunction(newFunctionName)
    
    # Insert the function name:
    process$functionName <- newFunctionName
    
    # Remove any invalid process parameters:
    #possibleProcessParameters <- getPossibleProcessParameterNames(process$functionName)
    #process$processParameters <- process$processParameters[possibleProcessParameters]
    
    # Get the parameters to display, and their defaults:
    defaults <- getStoxFunctionParameterDefaults(process$functionName)
    
    # Detect which parameters are data types, which identifies them as function inputs (outputs from other processes):
    areInputs <- isFunctionInput(names(defaults))
    
    # Split the defaults into function parameters and function inputs:
    process$functionParameters <- defaults[!areInputs]
    process$functionInputs <- defaults[areInputs]
    
    # Delete the processData, since these are no longer valid for the new function:
    process$processData <- list()
        
    # Return the process:
    process
}

onlyValidCharactersInProcessnName <- function(newProcessName) {
    # Check for invalid characters:
    indValidCharacters <- gregexpr(getRstoxFrameworkDefinitions("validProcessNameSet"), newProcessName)[[1]]
    indInvalidCharacters <- setdiff(seq_len(nchar(newProcessName)), indValidCharacters)
    if(length(indInvalidCharacters)) {
        warning("Process names can only contain lower and upper letters, numbers, dot and underscore. Contained ", paste(strsplit(newProcessName, "")[indInvalidCharacters], collapse = ", "))
        FALSE
    }
    else {
        TRUE
    }
}

checkProcessNameAgainstExisting <- function(projectPath, modelName, newProcessName) {
    # Check the process names of the model:
    processIndexTable <- readProcessIndexTable(projectPath = projectPath, modelName = modelName)
    if(newProcessName %in% processIndexTable$processName) {
        warning("The new process name (", newProcessName, ") cannot be identical to the name of an existing process within the same model (", paste(processIndexTable$processName, collapse = ", "), ")")
        FALSE
    }
    else {
        TRUE
    }
}

validateProcessnName <- function(projectPath, modelName, newProcessName) {
    onlyValidCharactersInProcessnName(newProcessName) & checkProcessNameAgainstExisting(projectPath = projectPath, modelName = modelName, newProcessName = newProcessName)
}


setListElements <- function(list, insertList, projectPath, modelName, processID) {
    
    # Report a warning for function parameters not present in the process:
    insertNames <- names(insertList)
    presentNames <- names(list)
    valid <- insertNames %in% presentNames
    
    # This warning made more sense when the contents of setListElements() was included in every function using it, since the process and function name was available. 
    if(any(!valid)) {
        # Warn the user that there are invalid function parameters:
        warning(
            "Removed the following unrecognized parameters for the function ", 
            getFunctionName(projectPath, modelName, processID), 
            " of process ", 
            getProcessName(projectPath, modelName, processID), 
            ": ", 
            paste(insertNames[!valid], collapse = ", "),
            if(length(presentNames)) paste0(" (Valid parameters: ", paste(presentNames, sep = ", "), ")")
        )
        # Keep only the new function parameters that present in the existing function parameters:
        insertNames <- insertNames[valid]
    }
    
    # Insert the function parameters (one by one for safety):
    if(length(insertNames)) {
        for(ind in seq_along(insertList)) {
            list[[names(insertList[ind])]] <- insertList[[ind]]
        }
    }
    
    list
}


##### Functions for modifying individual process arguments. These are called in the exported function modifyProcess(): #####
modifyFunctionName <- function(projectPath, modelName, processID, newFunctionName) {
    
    # Get the project description:
    process <- getProcess(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID
    )

    # Convert from possible JSON input:
    newFunctionName <- parseParameter(newFunctionName)
    
    # Change the function name only if different from the existing:
    if(!identical(process$functionName, newFunctionName)) {
        # Set the function name, and the corresponding default function inputs and parameters, as well as removing any process parameters that should not be included (showImMap):
        process <- setFunctionName(process, newFunctionName)
        
        # Store the changes:
        setProcessMemory(
            projectPath = projectPath, 
            modelName = modelName, 
            processID = processID, 
            process = process
        )
    }
    
    process
}
modifyProcessName <- function(projectPath, modelName, processID, newProcessName) {
    
    # Get the current process name:
    processName <- getProcessName(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID
    )
    
    # Convert from possible JSON input:
    newProcessName <- parseParameter(newProcessName)
    
    if(!identical(processName, newProcessName)) {
        # Validate the new process name (for invalid characters):
        if(validateProcessnName(projectPath = projectPath, modelName = modelName, newProcessName = newProcessName)) {
            setProcessMemory(
                projectPath = projectPath, 
                modelName = modelName, 
                processID = processID, 
                argumentName = "processName", 
                argumentValue = newProcessName
            )
        }
    }
    
    processName
}
modifyFunctionParameters <- function(projectPath, modelName, processID, newFunctionParameters) {
    
    # Get the function parameters:
    functionParameters <- getFunctionParameters(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID
    )
    
    # Convert from possible JSON input:
    newFunctionParameters <- parseParameter(newFunctionParameters)
    
    # Modify the funciton parameters:
    modifiedFunctionParameters <- setListElements(
        list = functionParameters, 
        insertList = newFunctionParameters, 
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID
    )
    
    # Store the changes:
    if(!identical(functionParameters, modifiedFunctionParameters)) {
        setProcessMemory(
            projectPath = projectPath, 
            modelName = modelName, 
            processID = processID, 
            argumentName = "functionParameters", 
            argumentValue = list(modifiedFunctionParameters) # We need to list this to make it correspond to the single value of the argumentName parameter.
        )
    }
    
    modifiedFunctionParameters
}
modifyFunctionInputs <- function(projectPath, modelName, processID, newFunctionInputs) {
    
    # Get the function inputs:
    functionInputs <- getFunctionInputs(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID
    )
    
    # Convert from possible JSON input:
    newFunctionInputs <- parseParameter(newFunctionInputs)
    
    # Modify the funciton inputs:
    modifiedFunctionInputs <- setListElements(
        list = functionInputs, 
        insertList = newFunctionInputs, 
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID
    )
    
    # Store the changes:
    if(!identical(functionInputs, modifiedFunctionInputs)) {
        setProcessMemory(
            projectPath = projectPath, 
            modelName = modelName, 
            processID = processID, 
            argumentName = "functionInputs", 
            argumentValue = list(modifiedFunctionInputs) # We need to list this to make it correspond to the single value of the argumentName parameter.
        )
    }
    
    modifiedFunctionInputs
}
modifyProcessParameters <- function(projectPath, modelName, processID, newProcessParameters) {
    
    # Get the function inputs:
    processParameters <- getProcessParameters(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID
    )
    
    # Convert from possible JSON input:
    newProcessParameters <- parseParameter(newProcessParameters)
    
    # Modify the funciton parameters:
    modifiedProcessParameters <- setListElements(
        list = processParameters, 
        insertList = newProcessParameters, 
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID
    )
    
    # Store the changes:
    if(!identical(processParameters, modifiedProcessParameters)) {
        setProcessMemory(
            projectPath = projectPath, 
            modelName = modelName, 
            processID = processID, 
            argumentName = "processParameters", 
            argumentValue = list(modifiedProcessParameters) # We need to list this to make it correspond to the single value of the argumentName parameter.
        )
    }
    
    modifiedProcessParameters
}
modifyProcessData <- function(projectPath, modelName, processID, newProcessData) {
    
    # Get the function inputs:
    processData<- getProcessData(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID
    )
    
    # Convert from possible JSON input:
    newProcessData <- parseParameter(newProcessData)
    
    # Modify the funciton parameters:
    modifiedProcessData <- setListElements(
        list = processData, 
        insertList = newProcessData, 
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID
    )
    
    # Store the changes:
    if(!identical(processData, modifiedProcessData)) {
        setProcessMemory(
            projectPath = projectPath, 
            modelName = modelName, 
            processID = processID, 
            argumentName = "processData", 
            argumentValue = list(modifiedProcessData) # We need to list this to make it correspond to the single value of the argumentName parameter.
        )
    }
    
    modifiedProcessData
}

#' 
#' @export
#' 
modifyProcess <- function(projectPath, modelName, processName, newValues) {
    
    # The values of the process must be changed in the following order:
    # 1. Function name
    # 2. Function parameters
    # 2. Function inputs
    # 1. Process name
    # 1. Process parameters
    # 1. Process data
    
    # Convert from possible JSON input:
    newValues <- parseParameter(newValues)
    
    # Get process ID from process name:
    processID <- getProcessIDFromProcessName(
        projectPath = projectPath, 
        modelName = modelName, 
        processName = processName
    )
    
    # Function name:
    if(length(newValues$functionName)) {
        modifyFunctionName(
            projectPath = projectPath, 
            modelName = modelName, 
            processID = processID, 
            newFunctionName = newValues$functionName
        )
    }
    
    # Function parameters:
    if(length(newValues$functionParameters)) {
        modifyFunctionParameters(
            projectPath = projectPath, 
            modelName = modelName, 
            processID = processID, 
            newFunctionParameters = newValues$functionParameters
        )
    }
    
    # Function inputs:
    if(length(newValues$functionInputs)) {
        modifyFunctionInputs(
            projectPath = projectPath, 
            modelName = modelName, 
            processID = processID, 
            newFunctionInputs = newValues$functionInputs
        )
    }
    
    # Process name:
    if(length(newValues$processName)) {
        modifyProcessName(
            projectPath = projectPath, 
            modelName = modelName, 
            processID = processID, 
            newProcessName = newValues$processName
        )
    }
    
    # Process parameters:
    if(length(newValues$processParameters)) {
        modifyProcessParameters(
            projectPath = projectPath, 
            modelName = modelName, 
            processID = processID, 
            newProcessParameters = newValues$processParameters
        )
    }
    
    # Process data:
    if(length(newValues$processData)) {
        modifyProcessData(
            projectPath = projectPath, 
            modelName = modelName, 
            processID = processID, 
            newProcessData = newValues$processData
        )
    }
    
    # Set the status as not saved (saving is done when running a process):
    setSavedStatus(projectPath, status = FALSE)
    
    # Return the process:
    getProcess(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID
    )
}
# Convert JSON input to list:
parseParameter <- function(parameter) {
    # If the parameter is JSON, convert to list:
    if("json" %in% class(parameter)) {
        parameter <- jsonlite::fromJSON(parameter)
    }
    parameter
}



getNewDefaultProcessName <- function(projectPath, modelName) {
    
    # Get all process names of the specified model:
    processIndexTable <- readProcessIndexTable(projectPath, modelName)
    processNames <- processIndexTable$processNames
    
    # Identify all process names starting with the process_Prefix:
    process_Prefix <- getRstoxFrameworkDefinitions("process_Prefix")
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
    
    # Create and return the name of the new project:
    processName <- paste0(process_Prefix, process_Index)
    processName
}

createNewProcessID <- function(projectPath, modelName, n = 1) {
    # Get the file containing the maximum process integer ID:
    maxProcessIntegerIDFile <- getProjectPaths(projectPath, "maxProcessIntegerIDFile")
    
    # If missing, create the file as an empty file:
    if(!file.exists(maxProcessIntegerIDFile)) {
        stoxModelNames <- getRstoxFrameworkDefinitions("stoxModelNames")
        maxProcessIntegerIDTable <- data.table::data.table(array(0, dim = c(1, length(stoxModelNames))))
        names(maxProcessIntegerIDTable) <- stoxModelNames
    }
    else {
        maxProcessIntegerIDTable <- data.table::fread(maxProcessIntegerIDFile, sep = "\t")
    }
    
    # Add 1 to the current process integer ID of the model
    processIntegerID <- maxProcessIntegerIDTable[[modelName]] + seq_len(n)
    maxProcessIntegerIDTable[[modelName]] <- max(processIntegerID)
    
    # Write the new maximum process integer ID:
    data.table::fwrite(maxProcessIntegerIDTable, maxProcessIntegerIDFile, sep = "\t")
    
    # Create the processID and return this:
    createProcessIDString(processIntegerID)
}


createProcessIDString <- function(integerID) {
    # Create the processID and return this:
    numDigitsOfProcessIntegerID <- getRstoxFrameworkDefinitions("numDigitsOfProcessIntegerID")
    # Paste P to the process integer ID:
    processID <- paste0("P", formatC(integerID, width = numDigitsOfProcessIntegerID, format = "d", flag = "0"))
    processID
}



#' 
#' @export
#' 
addEmptyProcess <- function(projectPath, modelName, processName = NULL) {
    
    # Get a default new process name, or check the validity of the given process name:
    if(length(processName)) {
        validProcessnName <- validateProcessnName(
            projectPath = projectPath, 
            modelName = modelName, 
            newProcessName = processName
        )
        if(!validProcessnName) {
            stop("Process not added")
        }
    }
    else {
        processName <- getNewDefaultProcessName(
            projectPath = projectPath, 
            modelName = modelName
        )
    }
    
    # Create an empty process:
    process <- createEmptyProcess(
        modelName = modelName, 
        processName = processName
    )
    
    # Get the process ID:
    processID <- createNewProcessID(
        projectPath = projectPath, 
        modelName = modelName
    )
    
    # Store the changes:
    setProcessMemory(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID, 
        process = process
    )
    
    # Return a data frame with process ID and name suited for appending to the process index table using addToProcessIndexTable():
    data.table::data.table(
        processID = processID, 
        processName = processName
    )
}

# Function to add all processes of template or project description:
addProcesses <- function(projectPath, modelName, projectMemory) {
    # Get the possible models:
    stoxModelNames <- getRstoxFrameworkDefinitions("stoxModelNames")
    
    # Loop through the possible models and add the processes:
    processes <- vector("list", length(stoxModelNames))
    names(processes) <- stoxModelNames
    
    for(modelName in stoxModelNames){
        for(ind in seq_along(projectMemory[[modelName]])){
            processes [[modelName]] [[ind]] <- addProcess(
                projectPath = projectPath, 
                modelName = modelName, 
                values = projectMemory[[modelName]][[ind]]
            )
        }
    }
    
    # Return the processes:
    processes
}

#' 
#' @export
#' 
addProcess <- function(projectPath, modelName, values) {
    
    # Create an empty process:
    process <- addEmptyProcess(
        projectPath = projectPath, 
        modelName = modelName, 
        processName = values$processName
    )
    
    # Update the process index table:
    addToProcessIndexTable(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = process$processID, 
        processName = process$processName
    )
    
    # Apply the arguments:
    process <- modifyProcess(
        projectPath = projectPath, 
        modelName = modelName, 
        processName = process$processName, 
        newValues = values
    )
    
    # Return the process:
    process
}
#' 
#' @export
#' 
removeProcess <- function(projectPath, modelName, processID) {
    # Update the project memory:
    removeProcessMemory(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID
    )
    
    # Update the process index table:
    removeFromProcessIndexTable(projectPath, modelName, processID)
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



### #' 
### #' @export
### #' 
### getProcess <- function(projectPath, modelName, processID) {
###     #projectDescription <- getCurrentProjectDescription(projectPath)
###     #process <- projectDescription[[modelName]][[processID]]
###     process <- getProjectMemoryData(projectPath, modelName, processID)
###     process$processIndex <- match(processName, names(projectDescription[[modelName]]))
###     process
### }
#' 
#' @export
#' 
runProcess <- function(projectPath, modelName, processID) {
    
    # Get the process:
    process <- getProcess(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID
    )
    process$processID <- processID
    
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
    functionInputProcessNames <- unlist(process$functionInputs)
    functionInputsProcessIDs <- getProcessIDFromProcessName(
        projectPath = projectPath, 
        modelName = modelName, 
        processName = functionInputProcessNames
    )
    if(length(functionInputsProcessIDs)) {
        functionInputs <- mapply(
            getProcessOutput, 
            projectPath = projectPath, 
            modelName = modelName, 
            processID = functionInputsProcessIDs
        )
        names(functionInputs) <- names(functionInputProcessNames)
    }
    else {
        functionInputs <- NULL
    }
    
    
    # Add functionInputs and functionParameters:
    functionArguments <- c(
        functionArguments, 
        functionInputs, 
        process$functionParameters
    )
    
    # Run the function:
    processOutput <- do.call(
        getFunctionNameFromPackageFunctionName(process$functionName), 
        functionArguments, 
        envir = as.environment(paste("package", getPackageNameFromPackageFunctionName(process$functionName), sep = ":"))
    )
    # Wrap the function output to a list named with the data type:
    processOutput <- list(processOutput)
    names(processOutput) <- getStoxFunctionMetaData(process$functionName, "functionOutputDataType")
    
    # Store the processData (this must be a named list of only one data table):
    if(isProcessDataFunction(process$functionName)) {
        process$processData <- processOutput
    }
    
    # Write to memory files:
    writeProcessOutputMemoryFile(processOutput = processOutput, process = process, projectPath = projectPath, modelName = modelName)
    
    # Write to text files:
    if(process$processParameters$fileOutput) {
        writeProcessOutputTextFile(processOutput = processOutput, process = process, projectPath = projectPath, modelName = modelName)
    }
    
    invisible(processOutput)
}


#wrapProcessOutputToList <- function(processOutput) {
#    if(is.data.table::data.table(processOutput) || "SpatialPolygons" %in% class(processOutput)) {
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
getProcessOutput <- function(projectPath, modelName, processID, tableName = NULL) {
    
    # Get the directory holding the output files:
    folderPath <- getProcessOutputFolder(projectPath = projectPath, modelName = modelName, processID = processID)
    
    # The tables are saved by individual files named by the table name:
    if(length(tableName)) {
        filePahts <- file.path(folderPath, paste(tableName, "rds$", sep = "."))
        if(!all(file.exists(filePahts))) {
            warning("The requested output (", paste(tableName, collapse = ", "), ") does not exist for the process ", getProcessName(projectPath = projectPath, modelName = modelName, processID = processID))
        }
    }
    else {
        filePahts <- list.files(folderPath, full.names = TRUE, pattern = "\\.rds$")
    }
    
    # Read the files to a list:
    processOutput <- lapply(filePahts, readRDS)
    unlist(processOutput, recursive = FALSE)
}

deleteProcessOutput <- function(projectPath, modelName, processID) {
    # Get the directory holding the output files:
    folderPath <- getProcessOutputFolder(projectPath = projectPath, modelName = modelName, processID = processID)
    unlink(folderPath, recursive = FALSE, force = TRUE)
}

getProcessOutputFolder <- function(projectPath, modelName, processID) {
    file.path(getProjectPaths(projectPath, "dataFolder"), modelName, processID)
}



getProcessIndexFromProcessID <- function(projectPath, modelName, processID) {
    processIndexTable <- readProcessIndexTable(projectPath, modelName)
    processIndex <- which(processIndexTable$processID == processID)
    processIndex
}


#' 
#' @export
#' 
writeProcessOutputTextFile <- function(processOutput, process, projectPath, modelName) {
    
    # Function for writing one element of the function output list:
    reportFunctionOutputOne <- function(processOutputOne, filePathSansExt) {
        if(length(processOutputOne)){
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
    
    folderPath <- getProjectPaths(projectPath = projectPath, paste("Output", modelName, sep = "_"))
    processIndex <- getProcessIndexFromProcessID(projectPath = projectPath, modelName = modelName, processID = process$processID)
    fileNamesSansExt <- paste(processIndex, names(processOutput), sep = "_")
    filePathsSansExt <- file.path(folderPath, paste(fileNamesSansExt))
    
    # Set the file name:
    mapply(reportFunctionOutputOne, processOutput, filePathsSansExt)
    # lapply(processOutput, reportFunctionOutputOne)
}

#' 
#' @export
#' 
writeProcessOutputMemoryFile <- function(processOutput, process, projectPath, modelName) {
    
    # Set the file name:
    folderPath <- getProcessOutputFolder(projectPath = projectPath, modelName = modelName, processID = process$processID)
    processIndex <- getProcessIndexFromProcessID(projectPath = projectPath, modelName = modelName, processID = process$processID)
    fileNameSansExt <- paste(processIndex, names(processOutput), sep = "_")
    fileName <- paste(fileNameSansExt, "rds", sep = ".")
    filePath <- file.path(folderPath, fileName)
    
    # Create the folder and save the process output as one file containing a list of DataType, ans possible sublists specified by the function producing the output:
    dir.create(dirname(filePath), recursive = TRUE, showWarnings = FALSE)
    # Drop the top level, since this is the data type, and this is known in runProcess:
    saveRDS(processOutput[[1]], filePath)
}


#' 
#' @export
#' 
runModel <- function(projectPath, modelName, startProcess = 1, endProcess = 3, save = TRUE) {
    
    # Chech that none of the models of the project are running:
    if(isRunning(projectPath)) {
        warning("The project is running (", projectPath, ")")
        return(FALSE)
    }
    else {
        setRunning(projectPath)
    }
    
    # Get the processIDs:
    processIndexTable <- readProcessIndexTable(projectPath, modelName)
    processIDs <- processIndexTable[seq(startProcess, endProcess)]$processID
    
    # Loop through the processes:
    for(processID in processIDs) {
        temp <- runProcess(projectPath = projectPath, modelName = modelName, processID = processID)
    }
    
    # Save the project after each run:
    if(save) {
        saveProject(projectPath)
    }
    
    # Set the state as not running (deleting the isRunning file):
    setNotRunning(projectPath)
    
    TRUE
}


isRunning <- function(projectPath) {
    projectIsRunningFile <- getProjectPaths(projectPath, "projectIsRunningFile")
    file.exists(projectIsRunningFile)
}


setRunning <- function(projectPath) {
    projectIsRunningFile <- getProjectPaths(projectPath, "projectIsRunningFile")
    write("", projectIsRunningFile)
}



setNotRunning <- function(projectPath) {
    projectIsRunningFile <- getProjectPaths(projectPath, "projectIsRunningFile")
    unlink(projectIsRunningFile, force = TRUE)
}


