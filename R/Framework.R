# TODO:
# getModelNameFromProcessID

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
        #"RstoxFDA", 
        #"RstoxAnalysis", 
        #"RstoxReport"
    )
    
    # Define the requested (all) function attributes:
    requestedFunctionAttributeNames <- c(
        "packageName", 
        "functionName", 
        "functionType", 
        "functionCategory", 
        "functionOutputDataType", 
        "functionParameterType", 
        "functionParameterFormat", 
        "functionArgumentHierarchy"
    )
    
    # Get the stoxLibrary as the list of function attributes from all official packages:
    stoxLibrary <- getStoxLibrary(officialStoxLibraryPackages, requestedFunctionAttributeNames = requestedFunctionAttributeNames)
    
    #### Data types: ####
    oldStoxModelDataTypes <- c(
        "AcousticData",
        "BioticData",
        "LandingData",
        "NASC",
        "LengthDist",
        "Density",
        "Abundance",
        "IndividualDataStations",
        "IndividualData",
        "SuperIndividuals",
        "PolygonArea",
        "StationSpecCatDensity",
        "BioticCovData",
        "LandingCovData",
        "LandingWeightCovData",
        "ProcessData"
    )
    
    stoxDataTypes <- data.table::data.table(
        functionOutputDataType = sapply(stoxLibrary, "[[", "functionOutputDataType"), 
        functionType = sapply(stoxLibrary, "[[", "functionType")
    )
    
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
    # Define the number of digits to use in JSON files:
    digits <- list(
        JSON = 6
    )
    
    # Define the permitted classes for individual outputs from StoX functions:
    validOutputDataClasses <- c(
        "data.table", 
        #"SpatialPolygons"
        "SpatialPolygonsDataFrame"
    )
    ## Define the valid output data classes:
    #validOutputDataClasses <- c(
    #    "data.table", 
    #    "json", 
    #    "geojson"
    #)
    
    # Define the regular expression listing lower and upper characters, integers, underscore and dot:
    validProcessNameSet <- "[[:alnum:]_.]"
    # The prefix for new unnamed processes:
    process_Prefix <- "Process_"
    # The number of digits in the integer part of the project IDs:
    numDigitsOfProcessIntegerID <- 3
    
    
    # Define the process property types:
    processPropertyTypes <- list(
        default = "character", 
        optional = list(
            "logical", 
            "integer", 
            "double", 
            "numeric"
        )
    )
    # Define the process property formats (should this be used to check for valid format????????????):
    processPropertyFormats <- list(
        default = "none", 
        single = list(
            "filePath", 
            "directoryPath"
        ), 
        multiple = list(
            "filePaths", 
            "parameterTable", 
            "filterExpressionTable"
        )
    )
    
    # Define filter operators for the different data types:
    filterOperators <- list(
        character = c("==", "!=", "%in%", "%notin%"), 
        logical   = c("==", "!="), # This may never be used
        integer   = c("<", "<=", "==", "!=", ">=", ">", "%in%", "%notin%"),
        double    = c("<", "<=", "==", "!=", ">=", ">", "%in%", "%notin%"),
        numeric   = c("<", "<=", "==", "!=", ">=", ">", "%in%", "%notin%"),
        POSIXct   = c("<", "<=", "==", "!=", ">=", ">", "%in%", "%notin%")
    )
    
    # Define the StoX folders, data sources, model names, model display names, model descriptions, and the latter three grouped as model info:
    stoxFolders <- c(
        Input = "input", 
        Output = "output", 
        Process = "process"
    )
    stoxDataSourceFolders <- c(
        Acoustic = "acoustic", 
        Biotic = "biotic", 
        Landing = "landing"
    )
    stoxModelFolders <- c(
        baseline = "baseline", 
        analysis = "analysis", 
        report = "report"
    )
    stoxModelNames <- c(
        baseline = "baseline", 
        analysis = "analysis", 
        report = "report"
    )
    stoxModelDisplayNames <- c(
        baseline = "Baseline", 
        analysis = "Analysis", 
        report = "Report"
    )
    stoxModelDescriptions <- c(
        baseline = "Baseline: The estimation model", 
        analysis = "Analysis: Processes that run Baseline for analysis, such as estimation of variation", 
        report = "Report: Processes that run Baseline or Analysis processes to generate reports"
    )
    stoxModelInfo <- data.table::data.table(
        modelName = stoxModelNames, 
        displayName = stoxModelDisplayNames, 
        description = stoxModelDescriptions
    )
    
    # Define the folder structure of StoX:
    stoxFolderStructure <- list(
        stoxDataSourceFolders, 
        stoxModelFolders, 
        c(Process = "")
    )
    stoxFolderStructureNames <- unlist(lapply(stoxFolderStructure, names))
    stoxFolderStructure <- unname(unlist(mapply(file.path, stoxFolders, stoxFolderStructure)))
    stoxFolderStructure <- gsub('\\/$', '', stoxFolderStructure)
    names(stoxFolderStructure) <- stoxFolderStructureNames
    stoxFolderStructureList <- as.list(stoxFolderStructure)
    
    # Define data types which can be plotted in the map (includes also changing colour etc, such as assigned stations of an acoustic PSU):
    dataTypesToShowInMap <- c(
        StoxBioticData = "StoxBioticData", 
        StoxAcousticData = "StoxAcousticData", 
        StratumPolygon = "StratumPolygon"#, 
        #Assignment = "Assignment", 
        #AcousticPSU = "AcousticPSU", 
        #SweptAreaPSU = "SweptAreaPSU"
    )
    
    # Define the data types for the interactive modes:
    stratumDataType <- "StratumPolygon"
    acousticPSUDataType <- "AcousticPSU"
    sweptAreaPSUDataType <- "SweptAreaPSU"
    assignmentDataType <- "Assignment"
    stationDataType <- "StoxBioticData"
    EDSUDataType <- "StoxAcousticData"
    
    # Define empty StratumPolygon data type:
    #emptyStratumPolygon <- sp::SpatialPolygons(list())
    emptyStratumPolygon <- sp::SpatialPolygonsDataFrame(
        sp::SpatialPolygons(list()), 
        data = data.frame()
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
        fileOutput = "Write output to file"
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
        baseline = processDefaultFull, 
        analysis = processDefaultSansProcessData, 
        report = processDefaultSansProcessData
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
    
    # Load the required packages to enable searching for formals and documentation, e.g. for getStoxFunctionParameterPossibleValues():
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
getStoxLibrary <- function(packageNames, requestedFunctionAttributeNames) {
    
    # Validate the pakcages:
    packageNames <- packageNames[sapply(packageNames, validateStoxLibraryPackage)]
    # Get a list of the 'stoxFunctionAttributes' from each package:
    stoxFunctionAttributeLists <- lapply(packageNames, getStoxFunctionAttributes, requestedFunctionAttributeNames = requestedFunctionAttributeNames)
    
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
    
    # Keep only the non-duplicated functions: 
    stoxFunctionAttributes <- stoxFunctionAttributes[!areDuplicatedFunctionNames]
    stoxFunctionAttributes
}

# Function for extracting the stoxFunctionAttributes of the package, and adding the package name and full function name (packageName::functionName) to each element (function) of the list:
getStoxFunctionAttributes <- function(packageName, requestedFunctionAttributeNames = NULL) {
    
    # Get the exported object 'stoxFunctionAttributes' from the package:
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
    
    # Add the requested attributes if missing:
    if(length(requestedFunctionAttributeNames)) {
        stoxFunctionAttributes <- addMissingAttributes(stoxFunctionAttributes, requestedFunctionAttributeNames = requestedFunctionAttributeNames)
    }
    
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

# Function to add the missing attributes of all functions:
addMissingAttributes <- function(stoxFunctionAttributes, requestedFunctionAttributeNames) {
    # Function to add the missing attributes on one function:
    addMissingAttributes_one <- function(stoxFunctionAttribute, requestedFunctionAttributeNames) {
        # Declare a list of empty elements named with the requested attributes:
        out <- vector("list", length(requestedFunctionAttributeNames))
        names(out) <- requestedFunctionAttributeNames
        # Get the names of the present attributes:
        presentNames <- intersect(names(stoxFunctionAttribute), requestedFunctionAttributeNames)
        # Insert the present attributes:
        out[presentNames] <- stoxFunctionAttribute[presentNames]
        out
    }
    
    # Add the missing attributes from all functions:
    stoxFunctionAttributes <- lapply(stoxFunctionAttributes, addMissingAttributes_one, requestedFunctionAttributeNames = requestedFunctionAttributeNames)
    stoxFunctionAttributes
}



# Function for reading the backwardCompatibility object of a package:
getBackwardCompatibility <- function(packageName) {
    
    backwardCompatibility <- tryCatch(
        getExportedValue(packageName, "backwardCompatibility"), 
        error = function(err) NULL
    )
    
    backwardCompatibility
}

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
    
    TRUE
}

# Functions for getting the package or function name from the full adress to a function:
getPackageNameFromPackageFunctionName <- function(functionName) {
    sub("\\::.*", "", functionName)
}
getFunctionNameFromPackageFunctionName <- function(functionName) {
    substring(functionName, regexpr("::", functionName) + 2)
}
getPackageFunctionName <- function(functionName) {
    if(grepl("::", functionName, fixed = TRUE)) {
        return(functionName)
    }
    stoxLibrary <- getRstoxFrameworkDefinitions("stoxLibrary")
    if(functionName %in% names(stoxLibrary)) {
        stoxLibrary[[functionName]]$functionName
    }
    else {
        NULL
    }
    
}
getPackageNameFromFunctionName <- function(functionName) {
    getPackageNameFromPackageFunctionName(getPackageFunctionName(functionName))
}


# Function to check that the functionName refers to a valid funciton, i.e., that the function is exported from a valid package (see validateStoxLibraryPackage()), and that it is represented in the associated stoxFunctionAttributes list of that package:
validateFunction <- function(functionName) {
    
    # Expand the funciton name:
    functionName <- getPackageFunctionName(functionName)
    
    # 1. Check first that the function name contains a double colon, which is the first requirement for a process:
    #if(!grepl("::", functionName, fixed = TRUE)) {
    #    stop("The function \"", functionName, "\" does not appear to be a string of the form PACKAGENAME::FUNCTIONNAME, where PACK#AGENAME is the package exporting the function with name FUNCTIONNAME.")
    #}
    if(length(functionName) == 0) {
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


##################################################
##################################################
#' Create, open, close, save, saveAs and copy StoX projects.
#' 
#' Create a StoX project using \code{createProject}, possibly specifying a template; open an existing (un-opened) project using \code{openProject}, which involves creating files holding the memory of the project; close a project using  \code{closeProject}, which removes the memory files; save the project using \code{saveProject}, which saves the memory file to the project description file or make a copy using \code{saveProject} or \code{saveProject}, where the former closes the given project unsaved and opens the copy.
#' 
#' @param projectPath       The path to the StoX project, i.e., the folder of the project with the sub folders "input", "output" and "process". Can possibly be the path to a file inside the project folder.
#' @param template          A string naming the template to use when generating the project. See \code{getAvaiableTemplates} for a list of available templates.
#' @param ow                Logical: If TRUE overwrite the project.
#' @param showWarnings      Logical: If TRUE display warninigs when creting the project folders.
#' @param open              Logical: If TRUE open the project after creating it.
#' @param force             Logical: If TRUE reopen (close and then open) the project if already open.
#' @param save              Logical: If TRUE save the project before closing. Default (NULL) is to ask the user whether to save the project before closing.
#' @param newProjectPath    The path to the copied StoX project.
#' 
#' @name Projects
#' 
NULL
#' 
#' @export
#' @rdname Projects
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
    # Set the active process ID to 0 for all models:
    initiateActiveProcessID(projectPath)
    
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
    
    list(
        projectPath = projectPath, 
        projectName = basename(projectPath)
    )
}
#' 
#' @export
#' @rdname Projects
#' 
openProject <- function(projectPath, showWarnings = FALSE, force = FALSE) {
    
    if(!force && isOpenProject(projectPath)) {
        warning("Project ", projectPath, "is already open.")
        out <- list(
            projectPath = projectPath, 
            projectName = basename(projectPath)
        )
        return(out)
    }
    #else if(force) {
        closeProject(projectPath, save = FALSE)
    #}
    
    
    projectPath <- resolveProjectPath(projectPath)
    if(length(projectPath) == 0) {
        warning("The selected projectPath is not a StoX project or a folder/file inside a StoX project.")
        return(NULL)
    }
    
    # Create the project session folder structure:
    createProjectSessionFolderStructure(projectPath, showWarnings = showWarnings)
    
    # Read the project description file:
    projectMemory <- readProjectDescription(projectPath)
    
    # Set the active process ID to 0 for all models:
    initiateActiveProcessID(projectPath)
    
    # Set the project memory:
    temp <- addProcesses(
        projectPath = projectPath, 
        modelName = names(projectMemory), 
        projectMemory = projectMemory
    )
    
    # Set the status of the projcet as saved:
    setSavedStatus(projectPath, status = TRUE)
    
    list(
        projectPath = projectPath, 
        projectName = basename(projectPath)
    )
}
#' 
#' @export
#' @rdname Projects
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
#' @rdname Projects
#' 
saveProject <- function(projectPath) {
    # Get the current project description and save it to the project.RData file:
    writeProjectDescription(projectPath)
    # Set the status of the projcet as saved:
    setSavedStatus(projectPath, status = TRUE)
}
#' 
#' @export
#' @rdname Projects
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
#' @rdname Projects
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



##################################################
##################################################
#' Utilities for projects.
#' 
#' @inheritParams Projects
#' @name ProjectUtils
#' 
NULL
#' 
#' @export
#' @rdname ProjectUtils
#' 
isProject <- function(projectPath) {
    existsFolders <- sapply(getProjectPaths(projectPath, "stoxFolders"), file.exists)
    length(existsFolders) && all(existsFolders)
}
#' 
#' @export
#' @rdname ProjectUtils
#' 
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
#' @rdname ProjectUtils
#' 
isOpenProject <- function(projectPath) {
    if(isProject(projectPath)) {
        activeProcessIDFile <- getProjectPaths(projectPath, "activeProcessIDFile")
        hasActiveProcessData <- file.exists(activeProcessIDFile)
        existsFolders <- sapply(getProjectPaths(projectPath, "projectSessionFolderStructure"), file.exists)
        hasActiveProcessData && length(existsFolders) && all(existsFolders)
    }
    else {
        warning("Project ", projectPath, " does not exist.")
        NA
    }
}

#' 
#' @export
#' @rdname ProjectUtils
#' 
readProjectDescription <- function(projectPath, type = c("RData", "XML")) {
    # Read the project.RData or project.xml file depending on the 'type':
    type <- match.arg(type)
    switch(
        type,
        RData = readProjectDescriptionRdata(projectPath),
        XML = readProjectDescriptionXML(projectPath)
    )
}
readProjectDescriptionRdata <- function(projectPath) {
    # Get the path to the project description file:
    projectRDataFile <- getProjectPaths(projectPath, "projectRDataFile")
    load(projectRDataFile) # Creates the object
    # Define the process IDs and return the project description:
    defineProcessIDs(projectDescription)
}
readProjectDescriptionXML <- function(projectPath) {
    # Get the path to the project description file:
    projectXMLFile <- getProjectPaths(projectPath, "projectXMLFile")
    projectDescription <- readProjectXML(projectXMLFile) # Creates the object
    # Define the process IDs and return the project description:
    defineProcessIDs(projectDescription)
}
readProjectXML <- function(projectXMLFile) {
    # This is Edvins work, which will be completed later. Process data needs to be converted from JSON using JSON2processData():
    readProject(projectXMLFile)
}






##### UNFINISHED!!!!!!!!!!!!! #####
processData2JSON <- function(processData, digits = getRstoxFrameworkDefinitions("digits")) {
    if("Stratum" %in% names(processData)) {
        #as.character(geojsonio::geojson_json(processData))
        geojsonio::geojson_json(processData)
    }
    else {
        jsonlite::toJSON(processData, digits = digits)
    }
    
}

JSON2processData <- function(JSON) {
    data <- jsonlite::fromJSON(JSON)
    if(is.list(data)) {
        if(!data.table::is.data.table(data)) {
            data <- lapply(data, data.table::as.data.table)
        }
        else if(all(c("type", "features") %in% names(data))) {
            # Do not convert as the object is already geojson???
        }
    }
    else {
        data <- data.table::as.data.table(data)
    }
   
    data
}



#' 
#' @export
#' @rdname ProjectUtils
#' 
writeProjectDescription <- function(projectPath, type = c("RData", "XML")) {
    # Read the project.RData or project.xml file depending on the 'type':
    type <- match.arg(type)
    switch(
        type,
        RData = writeProjectDescriptionRdata(projectPath),
        XML = writeProjectDescriptionXML(projectPath)
    )
}
writeProjectDescriptionRdata <- function(projectPath) {
    # Get the current project description:
    projectDescription <- getProjectMemoryData(projectPath)
    
    # Get the path to the project description file, and save the current project description:
    projectRDataFile <- getProjectPaths(projectPath, "projectRDataFile")
    save(projectDescription, file = projectRDataFile)
}
writeProjectDescriptionXML <- function(projectPath) {
    # Get the current project description:
    projectDescription <- getProjectMemoryData(projectPath)
    
    # Get the path to the project description file, and save the current project description:
    projectXMLFile <- getProjectPaths(projectPath, "projectXMLFile")
    writeProjectXML(projectDescription, projectXMLFile)
}


writeProjectXML <- function(projectDescription, projectXMLFile) {
    # This is Edvins work, which will be completed later. This function will have to call processData2JSON to convert all process data except Stratum to JSON. Maybe Stratum should be recocnized using processData2JSON(), and converted to JSON. I see that writeProcessOutputTextFile() does convert geojson to json and then writes. Maybe this is the simnples way. We also need a function readProjectXML():
    #saveProject(projectDescription, projectXMLFile)
}










#' 
#' @export
#' @rdname ProjectUtils
#' 
initiateActiveProcessID <- function(projectPath) {
    # Read the active process ID for the model:
    activeProcessIDFile <- getProjectPaths(projectPath, "activeProcessIDFile")
    # Initiate with all zeros:
    activeProcessIDTable <- data.table::as.data.table(matrix(NA, nrow = 1, ncol = 3))
    colnames(activeProcessIDTable) <- getRstoxFrameworkDefinitions("stoxModelNames")
    data.table::fwrite(activeProcessIDTable, activeProcessIDFile, sep = "\t", na = "NA")
    activeProcessIDFile
}

#' 
#' @export
#' @rdname ProjectUtils
#' 
isRunning <- function(projectPath) {
    projectIsRunningFile <- getProjectPaths(projectPath, "projectIsRunningFile")
    file.exists(projectIsRunningFile)
}
#' 
#' @export
#' @rdname ProjectUtils
#' 
setRunning <- function(projectPath) {
    projectIsRunningFile <- getProjectPaths(projectPath, "projectIsRunningFile")
    write("", projectIsRunningFile)
}
#' 
#' @export
#' @rdname ProjectUtils
#' 
setNotRunning <- function(projectPath) {
    projectIsRunningFile <- getProjectPaths(projectPath, "projectIsRunningFile")
    unlink(projectIsRunningFile, force = TRUE)
}



#' 
#' 
#' 
setSavedStatus <- function(projectPath, status) {
    # Get the path to the projectSavedStatusFile:
    projectSavedStatusFile <- getProjectPaths(projectPath, "projectSavedStatusFile")
    # Write the status to the file:
    writeLines(as.character(status), projectSavedStatusFile)
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
getActiveProcessID <- function(projectPath, modelName = NULL) {
    # Read the active process ID for the model:
    activeProcessIDFile <- getProjectPaths(projectPath, "activeProcessIDFile")
    activeProcessIDTable <- data.table::fread(activeProcessIDFile, sep = "\t")
    if(length(modelName)) {
        return(activeProcessIDTable[[modelName]])
    }
    else {
        return(activeProcessIDTable)
    }
    
}


writeActiveProcessID <- function(projectPath, modelName, activeProcessID) {
    # Read the active process ID for the model:
    activeProcessIDFile <- getProjectPaths(projectPath, "activeProcessIDFile")
    if(!file.exists(activeProcessIDFile)) {
        warning("The active process ID file has not been initiated.")
    }
    activeProcessIDTable <- data.table::fread(activeProcessIDFile, sep = "\t")
    activeProcessIDTable[[modelName]] <- activeProcessID
    data.table::fwrite(activeProcessIDTable, activeProcessIDFile, sep = "\t", na = "NA")
    activeProcessIDFile
}

writeActiveProcessIDFromTable <- function(projectPath, activeProcessIDTable) {
    # Read the active process ID for the model:
    activeProcessIDFile <- getProjectPaths(projectPath, "activeProcessIDFile")
    if(!file.exists(activeProcessIDFile)) {
        warning("The active process ID file has not been initiated.")
    }
    data.table::fwrite(activeProcessIDTable, activeProcessIDFile, sep = "\t", na = "NA")
}

#' 
#' @export
#'
#revertActiveProcessID <- function(projectPath, modelName, step = 1) {
#    # Read the active process ID for the model:
#    activeProcessID <- getActiveProcessID(
#        projectPath = projectPath, 
#        modelName = modelName
#    )
#    
#    
#    # Get the process ID to reset the model to:
#    processIndexTable <- readProcessIndexTable(projectPath, modelName)
#    processIndex <- which(processIndexTable$processID == processID)
#    
#    # Get the active process ID as the process ID of the process before the specified process in the processIndexTable (or NA if #processIndex is 1):
#    if(processIndex == 1) {
#        activeProcessID <- NA
#    }
#    else {
#        activeProcessID <- processIndexTable$processID[processIndex - 1]
#    }
#    
#    
#    # Subtract 'step' from the active process ID:
#    activeProcessID <- activeProcessID - step
#    
#    # Write the reverte active process ID:
#    writeActiveProcessID(
#        projectPath = projectPath, 
#        modelName = modelName, 
#        activeProcessID = activeProcessID
#    )
#    
#    activeProcessID
#}


#' 
#' @export
#'
resetModel <- function(projectPath, modelName, processID = 1) {
    
    # ??
    ## Delete the output from the processes past the activeProcessID:
    #deleteProcessOutput(projectPath, modelName, processID)
    
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
    
    return(activeProcessID)
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
        fullPath <- file.path(projectPath, argumentFileTable$argumentFile[[ind]])
        thisArgumentValue <- readRDS(fullPath)
        
        projectMemory <- appendProjectDescription(
            projectMemory, 
            modelName = thisModelName, 
            processID = thisProcessID, 
            argumentName = thisArgumentName, 
            argumentValue = thisArgumentValue
        )
        
        # # Append the missing list elements down to the argument:
        # if(!thisModelName %in% names(projectMemory)) {
        #     projectMemory <- append(
        #         projectMemory, 
        #         structure(list(NULL), names = thisModelName)
        #     )
        # }
        # if(!thisProcessID %in% names(projectMemory [[thisModelName]])) {
        #     projectMemory [[thisModelName]] <- append(
        #         projectMemory[[thisModelName]], 
        #         structure(list(NULL), names = thisProcessID)
        #     )
        # }
        # # If missing, append the argument, and if not replace it:
        # if(!thisArgumentName %in% names(projectMemory [[thisModelName]] [[thisProcessID]])) {
        #     projectMemory [[thisModelName]] [[thisProcessID]] <- append(
        #         projectMemory [[thisModelName]] [[thisProcessID]], 
        #         structure(list(thisArgumentValue), names = thisArgumentName)
        #     )
        # }
        # else {
        #     projectMemory [[thisModelName]] [[thisProcessID]] [[thisArgumentName]] <- thisArgumentValue
        # }
        
        
        
        
        
        
        
        
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


appendProjectDescription <- function(projectDescription, modelName, processID, argumentName, argumentValue) {
    # Append the missing list elements down to the argument:
    if(!modelName %in% names(projectDescription)) {
        projectDescription <- append(
            projectDescription, 
            structure(list(NULL), names = modelName)
        )
    }
    if(!processID %in% names(projectDescription [[modelName]])) {
        projectDescription [[modelName]] <- append(
            projectDescription[[modelName]], 
            structure(list(NULL), names = processID)
        )
    }
    # If missing, append the argument, and if not replace it:
    if(!argumentName %in% names(projectDescription [[modelName]] [[processID]])) {
        projectDescription [[modelName]] [[processID]] <- append(
            projectDescription [[modelName]] [[processID]], 
            structure(list(argumentValue), names = argumentName)
        )
    }
    else {
        projectDescription [[modelName]] [[processID]] [[argumentName]] <- argumentValue
    }
    
    return(projectDescription)
}


# Read the process argument files to a list of the elements modelName, processID, argumentName, argumentValue:
getArgumentFileTable <- function(projectPath, modelName = NULL, processID = NULL, type = c("current", "original")) {
    
    # Read the current project memory file, which contains the list of files holding the current process arguments:
    projectMemoryFile <- getProjectPaths(projectPath, paste0(type[1], "ProjectMemoryFile"))
    
    # If the projectMemoryFile does not exist, return an empty data.table:
    if(file.exists(projectMemoryFile)) {
        # Read the projectMemoryFile:
        argumentFileTable <- readRDS(projectMemoryFile)$argumentFileTable
        # Subset out the model if requested:
        if(length(modelName)) {
            argumentFileTable <- subset(argumentFileTable, modelName == modelName)
        }
        if(length(processID)) {
            argumentFileTable <- subset(argumentFileTable, processID == processID)
        }
    }
    else {
        argumentFileTable <- data.table::data.table()
    }
    
    argumentFileTable
}


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


# Function for getting the file path of a current project memory file:
getCurrentProjectMemoryFile <- function(projectPath) {
    getProjectPaths(projectPath, "currentProjectMemoryFile")
}


# Function for getting the file path of a original project memory file:
getOriginalProjectMemoryFile <- function(projectPath) {
    getProjectPaths(projectPath, "originalProjectMemoryFile")
}




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
    
    # Return the file path relative to the project path:
    relativePath <- sub(projectPath, "", argumentFile)
    #argumentFile
    return(relativePath)
}



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

# Function for saving an argument file table (defining the process memory files comprising the process memory):
saveProjectMemory <- function(projectPath, argumentFileTable) {
    # Save the list of project argument files to the current project description file and to the new project description file:
    currentProjectMemoryFile <- getCurrentProjectMemoryFile(projectPath)
    newProjectMemoryFile     <- getNewProjectMemoryFile(projectPath)
    # Add the processIndexTable, the activeProcessID and the maxProcessIntegerID to the data to write:
    toWrite <- list(
        argumentFileTable = argumentFileTable, 
        processIndexTable = readProcessIndexTable(projectPath),  
        activeProcessIDTable = getActiveProcessID(projectPath), 
        maxProcessIntegerIDTable = getMaxProcessIntegerID(projectPath)
    )
    # Write the project memory to the current and new file:
    saveRDS(toWrite, file = currentProjectMemoryFile)
    saveRDS(toWrite, file = newProjectMemoryFile)
    
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



deleteProjectMemoryFile <- function(projectPath, projectMemoryFileRelativePath) {
    unlink(file.path(projectPath, projectMemoryFileRelativePath))
}








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
    fileWithNewCurrentProjectMemory  <- file.path(
        projectPath, 
        projectMemoryIndex$Path[projectMemoryIndex$Index == 0]
    )
    file.copy(
        from = fileWithNewCurrentProjectMemory, 
        to = getProjectPaths(projectPath, "currentProjectMemoryFile"), 
        overwrite = TRUE, 
        copy.date = TRUE
    )
    
    # Rewrite the text file holding processIndexTable, activeProcessIDTable and maxProcessIntegerIDTable:
    unwrapProjectMemoryFile(fileWithNewCurrentProjectMemory)
}


unwrapProjectMemoryFile <- function(projectMemoryFile) {
    # Read the project memory to get the data to write to the text files:
    projectMemory <- readRDS(projectMemoryFile)
    
    # Unwrap and overwrite the process index table file:
    writeProcessIndexTable(projectPath, projectMemory$processIndexTable)
    
    # Unwrap and overwrite the active process ID file:
    writeActiveProcessIDFromTable(projectPath, projectMemory$activeProcessIDTable)
    
    # Unwrap and overwrite the maximum process integer ID file:
    writeMaxProcessIntegerIDTable(projectPath, projectMemory$maxProcessIntegerIDTable)
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
    
    # Get the names of the available functions:
    availableFunctions <- names(stoxLibrary)
    # Get the category of each function, and split by category:
    functionCategories <- sapply(stoxLibrary, "[[", "functionCategory")
    availableFunctionsByCategory <-split(availableFunctions, functionCategories)
    # Sort each category:
    availableFunctionsByCategory <- lapply(availableFunctionsByCategory, sort)
    
    # Keep only the valid category:
    availableFunctionsByCategory[modelName]
}

# Function for getting specific metadata of a function, or all metadata if metaDataName = NULL:
getStoxFunctionMetaData <- function(functionName, metaDataName = NULL, showWarnings = TRUE) {
    
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
        if(showWarnings) {
            warning("The requested meta data ", metaDataName, " is not included in the stoxFunctionAttributes for function ", functionName, ".")
        }
        NULL
    }
}


getArgumentsToShow <- function(functionName, functionArguments) {
    
    # Get the function argument hierarchy:
    functionArgumentHierarchy <- getStoxFunctionMetaData(functionName, "functionArgumentHierarchy", showWarnings = FALSE)
    
    # Loop through the arguments given by parent tags in the functionArgumentHierarchy, and set toShow to FALSE if not any of the criterias are fulfilled:
    toShow <- logical(length(functionArguments))
    names(toShow) <- names(functionArguments)
    
    for(argumentName in names(toShow)) {
        # Check whether the argument is given in the functionArgumentHierarchy. If not, it will be shown:
        if(argumentName %in% names(functionArgumentHierarchy)) {
            # Loop through the functionArgumentHierarchy of the current argumentName and set to show if at least one condition is fullfilled:
            for(conditionArgument in names(functionArgumentHierarchy[[argumentName]])) {
                if(functionArguments[[conditionArgument]] == functionArgumentHierarchy[[argumentName]][[conditionArgument]]) {
                    toShow[[argumentName]] <- TRUE
                }
            }
        }
        else {
            toShow[[argumentName]] <- TRUE
        }
    }
    
    
    
    #toShow <- !logical(length(functionArguments))
    #names(toShow) <- names(functionArguments)
    #
    #for(argumentName in names(functionArgumentHierarchy)) {
    #    # Check the function arguments against the values in the function argument hierarchy:
    #    fullfilled <- functionArguments[names(functionArgumentHierarchy[[argumentName]])] == unlist(functionArgumentHierarchy[[argumen#tName]])
    #    if(!any(fullfilled)) {
    #        toShow[[argumentName]] <- FALSE
    #    }
    #}
    
    # Return only the names of the arguments to show:
    return(names(toShow)[toShow])
}


isProcessDataFunction <- function(functionName) {
    # Get the function output data type and match against the defined process data types:
    #functionOutputDataType <- getStoxFunctionMetaData(functionName, "functionOutputDataType")
    #functionOutputDataType %in% getRstoxFrameworkDefinitions("stoxProcessDataTypes")
    getStoxFunctionMetaData(functionName, "functionType") == "processData"
}



# Function which gets the values defined for the parameters in the definition of a function:
getStoxFunctionParameterPossibleValues <- function(functionName, dropProcessData = TRUE, fill.logical = TRUE) {
    
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
    
    # Insert c(FALSE, TRUE) for logicals:
    if(fill.logical) {
        areLogicals <- sapply(f, is.logical)
        if(sum(areLogicals)) {
            f[areLogicals] <- lapply(f[areLogicals], expandLogical)
            # f[areLogicals] <- rep(list(c(FALSE, TRUE)), sum(areLogicals))
        }
    }
    
    f
}

expandLogical <- function(x) {
    c(x, !x)
}

# Function which gets the default values of a function:
getStoxFunctionParameterDefaults <- function(functionName) {
    # Get the possible values of the parameters of a function:
    functionParameterPossibleValues <- getStoxFunctionParameterPossibleValues(functionName, fill.logical = FALSE)
    # The default is the first value:
    defaults <- lapply(functionParameterPossibleValues, utils::head, 1)
    defaults
}

# Function which gets the primitive types of the parameters of a function:
getStoxFunctionParameterPrimitiveTypes <- function(functionName) {
    # Get the possible values of the parameters of a function:
    functionParameterDefaults <- getStoxFunctionParameterDefaults(functionName)
    # The default is the first value:
    primitiveType <- lapply(functionParameterDefaults, firstClass)
    primitiveType
}
# Function which gets the primitive types of the parameters of a function:
getStoxFunctionParameterPropertyTypes <- function(functionName) {
    
    # Get the primitive types of the parameters of a function (as specified in the function definition):
    typeFromDefinition <- getStoxFunctionParameterPrimitiveTypes(functionName)
    
    # Get the meta data functionParameterType (as specified in the 'stoxFunctionAttributes' of each package):
    functionParameterType <- getStoxFunctionMetaData(functionName, "functionParameterType")
    
    # Replace the types by those from the meta data:
    valid <- intersect(names(typeFromDefinition), names(functionParameterType))
    if(length(valid)) {
        typeFromDefinition[valid] <- functionParameterType[valid]
    }
    
    # If not integer, double or logical, set to character (as all other types than these are wrapped to JSON strings by the GUI):
    processPropertyTypes <- getRstoxFrameworkDefinitions("processPropertyTypes")
    setAsCharacter <- !typeFromDefinition %in% processPropertyTypes$optional
    typeFromDefinition[setAsCharacter] <- processPropertyTypes$default
    
    # Return the types:
    typeFromDefinition
}

# Function which applies the default format on formats not recognized :
getFunctionParameterPropertyFormats <- function(functionName) {
    
    # Get the types, and interpret all types as format "none":
    formats <- getStoxFunctionParameterPropertyTypes(functionName)
    formats[] <- "none"
    
    # Get the meta data functionParameterFormat (as specified in the 'stoxFunctionAttributes' of each package):
    functionParameterFormat = getStoxFunctionMetaData(functionName, "functionParameterFormat")
    
    # Replace the formats by those from the meta data:
    valid <- intersect(names(formats), names(functionParameterFormat))
    if(length(valid)) {
        formats[valid] <- functionParameterFormat[valid]
    }
    
    # Return the formats:
    formats
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

# This function gets the process data as stored in the process memory files. These process data may diffre from the process data output from the process, stored in the output data files, particularly if interactive functions have been used. In this case, the process must be run again with UseProcessData = TRUE (automatically set by RstoxFramework) to update the process output, which is used in runProcess() using getProcessOutput(). 
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

checkDataType <- function(dataType, projectPath, modelName, processID) {
    #dataType %in% getDataType(projectPath, modelName, processID)
    if(!dataType %in% getDataType(projectPath, modelName, processID)) {
        stop("The process ", getProcessName(projectPath, modelName, processID), " does not return ", dataType, " data.")
    }
}
    

##### Functions for manipulating the process index table, which defines the order of the processes. These functions are used by the frontend to delete, add, and reorder processes: #####
readProcessIndexTable <- function(projectPath, modelName = NULL, startProcess = 1, endProcess = Inf) {
    # Get the path to the process index file:
    processIndexTableFile <- getProjectPaths(projectPath, "processIndexTableFile")
    
    # If missing, create the file as an empty file:
    if(!file.exists(processIndexTableFile)) {
        return(data.table::data.table())
    }
    # Otherwise read the table from the file:
    else {
        # Read and extract the specified model:
        processIndexTable <- data.table::fread(processIndexTableFile, sep = "\t")
        # Return immediately if modelName is empty (returning the entire table):
        if(length(modelName) == 0) {
            return(processIndexTable)
        }
        
        validRows <- processIndexTable$modelName %in% modelName
        processIndexTable <- subset(processIndexTable, validRows)
        
        # If the model in empty, return an empty data.table:
        if(nrow(processIndexTable) == 0) {
            return(data.table::data.table())
        }
        
        # Restrict the startProcess and endProcess to the range of process indices:
        startProcess <- max(1, startProcess)
        endProcess <- min(nrow(processIndexTable), endProcess)
        # Extract the requested process IDs:
        processIndexTable <- processIndexTable[seq(startProcess, endProcess), ]
    }
    
    return(processIndexTable)
}

writeProcessIndexTable <- function(projectPath, processIndexTable) {
    # Get the path to the process index file:
    processIndexTableFile <- getProjectPaths(projectPath, "processIndexTableFile")
    # write the file:
    processIndexTable <- data.table::fwrite(processIndexTable, processIndexTableFile, sep = "\t")
}


addToProcessIndexTable <- function(projectPath, modelName, processID, processName, afterIndex = NULL) {
    
    # Get the process index file:
    #processIndexTable <- readProcessIndexTable(projectPath = projectPath, modelName = modelName)
    processIndexTable <- readProcessIndexTable(projectPath = projectPath)
    
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
    writeProcessIndexTable(projectPath = projectPath, processIndexTable = processIndexTable)
}


removeFromProcessIndexTable <- function(projectPath, modelName, processID) {
    
    # Get the process index file:
    processIndexTable <- readProcessIndexTable(projectPath = projectPath)
    
    # Remove the process:
    toRemove <- processIndexTable$modelName == modelName & processIndexTable$processID == processID
    toKeep <- !toRemove
    processIndexTable <- subset(processIndexTable, toKeep)
    
    # Write the file:
    writeProcessIndexTable(projectPath = projectPath, processIndexTable = processIndexTable)
}

rearrangeProcessIndexTable <- function(projectPath, modelName, processID, afterProcessID) {
    
    # Get the process index file:
    processIndexTable <- readProcessIndexTable(projectPath = projectPath, modelName = modelName)
    
    # Add the process ID and as the process after 'afterProcessID':
    toRearrange <- processIndexTable$modelName %in% modelName & processIndexTable$processID %in% processID
    notToRearrange <- !toRearrange
    rearranged <- subset(processIndexTable, toRearrange)
    rest <- subset(processIndexTable, notToRearrange)
    
    afterProcessIndexInRest <- which(rest$modelName %in% modelName & rest$processID == afterProcessID)
    before <- rest[seq_len(afterProcessIndexInRest), ]
    if(afterProcessIndexInRest < nrowProcessIndexTable) {
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
    writeProcessIndexTable(projectPath = projectPath, processIndexTable = processIndexTable)
}


modifyProcessNameInProcessIndexTable <- function(projectPath, modelName, processName, newProcessName) {
    # Get the process index file:
    processIndexTable <- readProcessIndexTable(projectPath = projectPath, modelName = modelName)
    
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


#getProcessIDFromStartEnd <- function(projectPath, modelName, startProcess = 1, endProcess = Inf) {
#    # Get the processIDs:
#    processIndexTable <- readProcessIndexTable(projectPath, modelName)
#    # Rstrict the startProcess and endProcess to the range of process indices:
#    startProcess <- max(1, startProcess)
#    endProcess <- min(nrow(processIndexTable), endProcess)
#    # Extract the requested process IDs:
#    processIDs <- processIndexTable[seq(startProcess, endProcess)]$processID
#    return(processIDs)
#}



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
    canShowInMap <- sapply(functionNames, getCanShowInMap)
    
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
    
    # Add the function inputs:
    processTable$functionInputs <- functionInputs
    
    # Add the function parameters:
    functionParameters <- mapply(
        getFunctionParameters, 
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processIndexTable$processID
    )
    processTable$functionParameters <- functionParameters
    
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
    parameter %in% stoxDataTypes$functionOutputDataType
}


createEmptyProcess <- function(modelName = "baseline", processName = NULL) {
    # Get the default process with empty fields for project and function name, process data, and function parameters and inputs:
    process <- getRstoxFrameworkDefinitions("processDefault")[[modelName]]
    # Possibly add the given process name (this is done here since creating a default process name is not always needed or wanted):
    if(length(processName) && !is.na(processName) && nchar(processName) > 0) {
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
    
    # Report a warning for elements not present in the list:
    insertNames <- names(insertList)
    #presentNames <- names(list)
    #valid <- insertNames %in% presentNames
    
    # This warning made more sense when the contents of setListElements() was included in every function using it, since the process and function name was available. 
    
    #if(any(!valid)) {
    #    # Warn the user that there are invalid list elements:
    #    warning(
    #        "Removed the following unrecognized parameters for the function ", 
    #        getFunctionName(projectPath, modelName, processID), 
    #        " of process ", 
    #        getProcessName(projectPath, modelName, processID), 
    #        ": ", 
    #        paste(insertNames[!valid], collapse = ", "),
    #        if(length(presentNames)) paste0(" (Valid parameters: ", paste(presentNames, sep = ", "), ")")
    #    )
    #    # Keep only the new list elements that are present in the list:
    #    insertNames <- insertNames[valid]
    #}
    
    # Insert the list elements (one by one for safety):
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
    
    # Change the function name only if different from the existing:
    if(!identical(process$functionName, newFunctionName)) {
        # Error if the function name is not character:
        if(!is.character(newFunctionName)) {
            stop("The function name must be a character string of the type packageName::functionName")
        }
        # Set the function name, and the corresponding default function inputs and parameters, as well as removing any process parameters that should not be included (showImMap):
        process <- setFunctionName(process, newFunctionName)
        
        # Store the changes:
        setProcessMemory(
            projectPath = projectPath, 
            modelName = modelName, 
            processID = processID, 
            process = process
        )
        
        # Return a flag TRUE if the function name was changed: 
        return(TRUE)
    }
    else {
        return(FALSE)
    }
    #process
}
modifyProcessName <- function(projectPath, modelName, processID, newProcessName) {
    
    # Get the current process name:
    processName <- getProcessName(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID
    )
    
    # Change the process name only if different from the existing:
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
        
        
        
        # Return a flag TRUE if the process name was changed: 
        return(TRUE)
    }
    else {
        return(FALSE)
    }
    
    #processName
}
modifyFunctionParameters <- function(projectPath, modelName, processID, newFunctionParameters) {
    
    # Get the function parameters:
    functionParameters <- getFunctionParameters(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID
    )
    
    # Modify any file or directory paths to relative paths if possible, and issue a warning if the projectPath is not in the path:
    newFunctionParameters <- getRelativePaths(
        functionParameters = newFunctionParameters, 
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID
    )
    
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
        
        # Return a flag TRUE if the function parameters were changed: 
        return(TRUE)
    }
    else {
        return(FALSE)
    }
    
    #modifiedFunctionParameters
}
modifyFunctionInputs <- function(projectPath, modelName, processID, newFunctionInputs) {
    
    # Get the function inputs:
    functionInputs <- getFunctionInputs(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID
    )
    
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
    
        # Return a flag TRUE if the function inputs were changed: 
        return(TRUE)
    }
    else {
        return(FALSE)
    }
    
    #modifiedFunctionInputs
}
modifyProcessParameters <- function(projectPath, modelName, processID, newProcessParameters) {
    
    # Get the function inputs:
    processParameters <- getProcessParameters(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID
    )
    
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
    
        # Return a flag TRUE if the process parameters were changed: 
        return(TRUE)
    }
    else {
        return(FALSE)
    }
    
    #modifiedProcessParameters
}
modifyProcessData <- function(projectPath, modelName, processID, newProcessData) {
    
    # Get the process data:
    processData<- getProcessData(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID
    )
    
    # Modify the process data:
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
    
        # Return a flag TRUE if the process data were changed: 
        return(TRUE)
    }
    else {
        return(FALSE)
    }
    
    #modifiedProcessData
}


# Function returning a logical vector with TRUE for function parameters which are file paths as per the format attribute:
detectFilePaths <- function(functionParameters, projectPath, modelName, processID) {
    # Get the function name and the function parameter formats:
    functionName <- getFunctionName(projectPath, modelName, processID)
    functionParameterFormat <- getStoxFunctionMetaData(functionName, "functionParameterFormat")
    
    # Detect file path formats:
    areFilePathsAndNonEmpty <- functionParameterFormat[names(functionParameters)] %in% c("filePath", "filePaths", "directoryPath") & lengths(functionParameters) > 0
    areFilePathsAndNonEmpty
}

# Function to detect function parameter format filePath, filePaths or directoryPath, and convert to relative paths if the projectPath is present in the paths:
getRelativePaths <- function(functionParameters, projectPath, modelName, processID) {
    
    # Function to attempt to convert to relative path:
    getRelativePath <- function(filePath, projectPath) {
        # Expand the paths:
        projectPath <- path.expand(projectPath)
        filePath <- path.expand(filePath)
        
        # Check whether the filePath is a relative path already:
        fullFilePath <- file.path(projectPath, filePath)
        if(file.exists(fullFilePath) && isFALSE(file.info(fullFilePath)$isdir)) {
            return(filePath)
        }
        
        # If the projectPath is in the filePath, convert to a relative file path:
        if(grepl(projectPath, filePath)) {
            filePath <- sub(projectPath, "", filePath, fixed = TRUE)
            # Remove also the trailing file separator:
            filePath <- substring(filePath, 2)
        }
        else {
            warning("The specified file ", filePath, " is not present in the project folder (", projectPath, ")")
        }
        filePath
    }
    
    getRelativePaths <- function(filePaths, projectPath) {
        sapply(filePaths, getRelativePath, projectPath)
    }
    
    # Detect the file paths:
    areFilePathsAndNonEmpty <- detectFilePaths(
        functionParameters = functionParameters, 
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID
    )
    
    # Get relative paths:
    if(any(areFilePathsAndNonEmpty)) {
        functionParameters[areFilePathsAndNonEmpty] <- lapply(
            functionParameters[areFilePathsAndNonEmpty], 
            getRelativePaths, 
            projectPath = projectPath
        )
    }
    
    functionParameters
}

# Function to detect function parameter format filePath, filePaths or directoryPath, and convert to abolute paths for use in functions:
getAbsolutePaths <- function(functionParameters, projectPath, modelName, processID) {
    
    # Function to attempt to convert to relative path:
    getAbsolutePath <- function(filePath, projectPath) {
        # Check first whether the file exists as a relative path:
        absolutePath <- file.path(projectPath, filePath)
        if(all(file.exists(absolutePath))) {
            absolutePath
        }
        else if(all(file.exists(filePath))) {
            filePath
        }
        else {
            warning("The file ", filePath, " does not exist.")
            filePath
        }
    }
    
    # Detect the file paths:
    areFilePathsAndNonEmpty <- detectFilePaths(
        functionParameters = functionParameters, 
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID
    )
    
    # Get absolute paths:
    if(any(areFilePathsAndNonEmpty)) {
        functionParameters[areFilePathsAndNonEmpty] <- lapply(
            functionParameters[areFilePathsAndNonEmpty], 
            getAbsolutePath, 
            projectPath = projectPath
        )
    }
    
    functionParameters
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
    if(!isOpenProject(projectPath)) {
        warning("The project ", projectPath, " is not open.")
        return(NULL)
    }
    
    # Get process ID from process name:
    processID <- getProcessIDFromProcessName(
        projectPath = projectPath, 
        modelName = modelName, 
        processName = processName
    )
    
    # Output a flag of TRUE if a modification occurred:
    modified <- FALSE
    
    # Function name:
    if(length(newValues$functionName)) {
        modified <- modified | modifyFunctionName(
            projectPath = projectPath, 
            modelName = modelName, 
            processID = processID, 
            newFunctionName = newValues$functionName
        )
    }
    
    # Function parameters:
    if(length(newValues$functionParameters)) {
        modified <- modified | modifyFunctionParameters(
            projectPath = projectPath, 
            modelName = modelName, 
            processID = processID, 
            newFunctionParameters = newValues$functionParameters
        )
    }
    
    # Function inputs:
    if(length(newValues$functionInputs)) {
        modified <- modified | modifyFunctionInputs(
            projectPath = projectPath, 
            modelName = modelName, 
            processID = processID, 
            newFunctionInputs = newValues$functionInputs
        )
    }
    
    # Process name:
    if(length(newValues$processName)) {
        modified <- modified | modifyProcessName(
            projectPath = projectPath, 
            modelName = modelName, 
            processID = processID, 
            newProcessName = newValues$processName
        )
    }
    
    # Process parameters:
    if(length(newValues$processParameters)) {
        modified <- modified | modifyProcessParameters(
            projectPath = projectPath, 
            modelName = modelName, 
            processID = processID, 
            newProcessParameters = newValues$processParameters
        )
    }
    
    # Process data:
    if(length(newValues$processData)) {
        modified <- modified | modifyProcessData(
            projectPath = projectPath, 
            modelName = modelName, 
            processID = processID, 
            newProcessData = newValues$processData
        )
    }
    
    # Set the status as not saved (saving is done when running a process):
    setSavedStatus(projectPath, status = FALSE)
    
    ### # Return the process:
    ### getProcess(
    ###     projectPath = projectPath, 
    ###     modelName = modelName, 
    ###     processID = processID
    ### )
    
    return(modified)
}
#' 
#' @export
#' 
# Convert JSON input to list:
parseParameter <- function(parameter, simplifyVector = TRUE) {
    # If the parameter is JSON, convert to list:
    if("json" %in% class(parameter)) {
        parameter <- jsonlite::fromJSON(parameter, simplifyVector = simplifyVector)
    }
    else if(is.character(parameter) && jsonlite::validate(parameter)) {
        parameter <- jsonlite::parse_json(parameter, simplifyVector = simplifyVector)
    }
    parameter
}



getNewDefaultProcessName <- function(projectPath, modelName) {
    
    # Get all process names of the specified model:
    processIndexTable <- readProcessIndexTable(projectPath, modelName)
    processNames <- processIndexTable$processNames
    
    ## Identify all process names starting with the process_Prefix:
    #process_Prefix <- getRstoxFrameworkDefinitions("process_Prefix")
    #startsWithProcess_Prefix <- startsWith(processNames, process_Prefix)
    #
    ## Get the lowest index which is not occupied:
    #if(any(startsWithProcess_Prefix)) {
    #    # Extract the integers after the underscore:
    #    process_Index <- as.numeric(substring(processNames[startsWithProcess_Prefix], nchar(process_Prefix) + 1))
    #    process_Index <- min(seq_len(max(process_Index)))
    #}
    #else {
    #    process_Index <- 1
    #}
    #
    ## Create and return the name of the new project:
    #processName <- paste0(process_Prefix, process_Index)
    #processName
    
    getNewDefaultName(processNames, getRstoxFrameworkDefinitions("process_Prefix"))
}

getMaxProcessIntegerID <- function(projectPath) {
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
    
    return(maxProcessIntegerIDTable)
}

writeMaxProcessIntegerIDTable <- function(projectPath, maxProcessIntegerIDTable) {
    # Get the file containing the maximum process integer ID:
    maxProcessIntegerIDFile <- getProjectPaths(projectPath, "maxProcessIntegerIDFile")
    # Write the new maximum process integer ID:
    data.table::fwrite(maxProcessIntegerIDTable, maxProcessIntegerIDFile, sep = "\t")
}




createNewProcessID <- function(projectPath, modelName, n = 1) {
    ## Get the file containing the maximum process integer ID:
    #maxProcessIntegerIDFile <- getProjectPaths(projectPath, "maxProcessIntegerIDFile")
    #
    ## If missing, create the file as an empty file:
    #if(!file.exists(maxProcessIntegerIDFile)) {
    #    stoxModelNames <- getRstoxFrameworkDefinitions("stoxModelNames")
    #    maxProcessIntegerIDTable <- data.table::data.table(array(0, dim = c(1, length(stoxModelNames))))
    #    names(maxProcessIntegerIDTable) <- stoxModelNames
    #}
    #else {
    #    maxProcessIntegerIDTable <- data.table::fread(maxProcessIntegerIDFile, sep = "\t")
    #}
    
    
    
    maxProcessIntegerIDTable <- getMaxProcessIntegerID(projectPath)
    
    # Add 1 to the current process integer ID of the model
    processIntegerID <- maxProcessIntegerIDTable[[modelName]] + seq_len(n)
    maxProcessIntegerIDTable[[modelName]] <- max(processIntegerID)
    
    # Write the new maximum process integer ID:
    #data.table::fwrite(maxProcessIntegerIDTable, maxProcessIntegerIDFile, sep = "\t")
    writeMaxProcessIntegerIDTable(
        projectPath = projectPath, 
        maxProcessIntegerIDTable = maxProcessIntegerIDTable
    )
    
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
    modifyProcess(
        projectPath = projectPath, 
        modelName = modelName, 
        processName = process$processName, 
        newValues = values
    )
    
    # Return the process:
    process <- getProcess(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = process$processID
    )
    return(process)
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



rearrangeProcesses <- function(projectPath, modelName, processNames, MoveTo = NULL) {
    
    # Get the project description:
    projectDescription <- getCurrentProjectDescription(projectPath)
    
    # Get the names of all processes, and the indices of the selected processes:
    originalxProcessNames <- names(projectDescription[[modelName]])
    indexOfProcessesToMove <- match(processNames, originalxProcessNames)
    
    
}




#### Functions to run models: ####
#' 
#' @export
#' 
runProcess <- function(projectPath, modelName, processID, msg = TRUE) {
    
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
    functionParameters <- list()
    # Add the processData if a processData function:
    if(isProcessDataFunction(process$functionName)) {
        functionParameters$processData <- process$processData
    }
    
    # Get the function input as output from the previously run processes:
    functionInputProcessNames <- unlist(process$functionInputs)
    if(length(functionInputProcessNames)) {
        # Get the function input process IDs:
        functionInputsProcessIDs <- mapply(
            getProcessIDFromProcessName, 
            projectPath = projectPath, 
            modelName = modelName, 
            processName = functionInputProcessNames
        )
        # Get the actual function inputs from the functionInputsProcessIDs:
        functionInputs <- mapply(
            getProcessOutput, 
            projectPath = projectPath, 
            modelName = modelName, 
            processID = functionInputsProcessIDs, 
            SIMPLIFY = FALSE
        )
        names(functionInputs) <- names(functionInputProcessNames)
    }
    else {
        functionInputs <- NULL
    }
    
    # Add functionInputs and functionParameters:
    functionParameters <- c(
        functionParameters, 
        functionInputs, 
        process$functionParameters
    )
    
    # Get absolute paths:
    functionParameters <- getAbsolutePaths(
        functionParameters = functionParameters, 
        projectPath = projectPath, 
        modelName = modelName,
        processID = processID
    )
    
    ### # Run the function:
    ### processOutput <- do.call(
    ###     getFunctionNameFromPackageFunctionName(process$functionName), 
    ###     functionParameters, 
    ###     envir = as.environment(paste("package", getPackageNameFromPackageFunctionName(process$functionName), sep = ":"))### 
    ### )
    
    # Try running the function, and return FALSE if failing:
    failed <- FALSE
    if(msg) {
        message(
            "StoX: Running process ", 
            getProcessIndexFromProcessID(projectPath, modelName, processID), 
            ": ", 
            getProcessName(projectPath, modelName, processID), 
            "..."
            )
    }
    
    processOutput <- tryCatch(
        do.call(
            getFunctionNameFromPackageFunctionName(process$functionName), 
            functionParameters, 
            envir = as.environment(paste("package", getPackageNameFromPackageFunctionName(process$functionName), sep = ":"))
        ), 
        error = function(err) {
            failed <<- TRUE
            stop(err)
        }
    )
    
    if(failed){
        return(FALSE)
    }
    else{
        # Update the active process ID:
        writeActiveProcessID(projectPath, modelName, processID)
        
        # If a valid output class wrap the function output to a list named with the data type:
        if(firstClass(processOutput) %in% getRstoxFrameworkDefinitions("validOutputDataClasses")) {
            processOutput <- list(processOutput)
            names(processOutput) <- getStoxFunctionMetaData(process$functionName, "functionOutputDataType")
        }
        
        # Store the processData (this must be a named list of only one data table):
        if(isProcessDataFunction(process$functionName)) {
            modifyProcessData(projectPath, modelName, processID, processOutput)
            
            # Set the function parameters UseProcessData to TRUE:
            setUseProcessDataToTRUE(projectPath, modelName, processID)
            #process$processData <- processOutput
        }
        
        # Write to memory files:
        writeProcessOutputMemoryFile(processOutput = processOutput, process = process, projectPath = projectPath, modelName = modelName)
        
        # Write to text files:
        if(process$processParameters$fileOutput) {
            writeProcessOutputTextFile(processOutput = processOutput, process = process, projectPath = projectPath, modelName = modelName)
        }
        
        #invisible(processOutput)
        TRUE
    }
}


setUseProcessDataToTRUE <- function(projectPath, modelName, processID) {
    modifyFunctionParameters(projectPath, modelName, processID, list(UseProcessData = TRUE))
}


##################################################
##################################################
#' Get output of a StoX process.
#' 
#' Gets the output of a process that has been run.
#' 
#' @param modelName The name of the model (one of "baseline", "analysis" and "report").
#' @param processID The ID of the process.
#' @param tableName The name of the table to extract from the process.
#' 
#' @export
#' 
#' @inheritParams Projects
#' @export
#' 
getProcessOutput <- function(projectPath, modelName, processID, tableName = NULL, subFolder = NULL, flatten = FALSE, pretty = FALSE, linesPerPage = 1000L, pageindex = integer(0), columnSeparator = " ", lineSeparator = NULL, na = "-", list.pretty = FALSE, drop = FALSE, drop.datatype = TRUE) {
    
    # If the 'tableName' contains "/", extract the 'subFolder' and 'tableName':
    if(any(grepl("/", tableName))) {
        subFolder_tableName <- strsplit(tableName, "/")
        subFolder <- sapply(subFolder_tableName, "[", 1)
        tableName <- sapply(subFolder_tableName, "[", 2)
    }
   
    
    # Get the directory holding the output files:
    folderPath <- getProcessOutputFolder(projectPath = projectPath, modelName = modelName, processID = processID)
    
    # Detect whether the output is a list of tables (depth 1) or a list of lists of tables (depth 2):
    folderDepth <- getFolderDepth(folderPath)
    
    # Get the files 
    processOutputFiles <- getProcessOutputFiles(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID
    )
    
    # Get the file paths of the requested memory files:
    if(folderDepth == 1) {
        # Get the selected tables:
        if(length(tableName)) {
            processOutputFiles <- selectValidElements(processOutputFiles, tableName)
        }
    }
    else {
        # Apply the subFolder if given:
        if(length(subFolder)) {
            processOutputFiles <- selectValidElements(processOutputFiles, subFolder)
        }
        
        # Also select the tables of each sub folder:
        if(length(tableName)) {
            # Warning: This selection ignores the file extension by the partial matching of R:
            processOutputFiles <- lapply(processOutputFiles, selectValidElements, tableName)
        }
    }
    
    if(length(processOutputFiles) == 0) {
        warning("Invalid specification of projectPath, modelName, processID or tableName (most likely tableName).")
    }
    
    
    
    # Read the files recursively:
    processOutput <- rapply(
        processOutputFiles, 
        readProcessOutputFile, 
        flatten = flatten, 
        pretty = pretty, 
        linesPerPage = linesPerPage, 
        pageindex = pageindex, 
        columnSeparator = columnSeparator, 
        lineSeparator = lineSeparator, 
        na = na, 
        list.pretty = list.pretty, 
        how = "replace"
    )

    # Unlist the top level if a single tabled data type is wrapped in a list:
    if(drop.datatype && is.list(processOutput) && length(processOutput) == 1 && names(processOutput) %in% getRstoxFrameworkDefinitions("stoxDataTypes")$functionOutputDataType) {
        processOutput <- processOutput[[1]]
    }
    
    # Unlist if only one element:
    if(drop) {
        while(is.list(processOutput) && !data.table::is.data.table(processOutput) && length(processOutput) == 1) {
            processOutput <- processOutput[[1]]
        }
    }
    
    processOutput
}


# Function to read a single process output file, possibly by pages and in flattened and pretty view:
readProcessOutputFile <- function(filePath, flatten = FALSE, pretty = FALSE, linesPerPage = 1000L, pageindex = integer(0), columnSeparator = " ", lineSeparator = NULL, na = "-", list.pretty = FALSE) {
    
    # Read the process output file:
    data <- readRDS(filePath)
    
    # Flatten the output so that cells which are vectors are transposed and the non-vector cells of the same line repeated:
    if(flatten) {
        data <- flattenProcessOutput(data)
    }
    
    # Extract the requested lines:
    numberOfLines <- nrow(data)
    numberOfPages <- ceiling(numberOfLines / linesPerPage)
    if(length(pageindex)) {
        linesToExtract <- seq_len(linesPerPage) + rep((pageindex - 1) * linesPerPage, each = linesPerPage)
        linesToExtract <- linesToExtract[linesToExtract <= numberOfLines]
        data <- data[linesToExtract, ]
    }
    
    # Convert to pretty view, which inserts spaces to obtain 
    if(pretty) {
        data <- fixedWidthDataTable(
            data, 
            columnSeparator = columnSeparator, 
            lineSeparator = lineSeparator, 
            na = na, 
            list.pretty = list.pretty
        )
        # In the pretty model, output a list containing the number of lines and the number of pages:
        data <- list(
            data = data, 
            numberOfLines = numberOfLines, 
            numberOfPages = numberOfPages
        )
    }
    data
}

flattenProcessOutput <- function(processOutput) {
    #if(firstClass(processOutput) == "SpatialPolygons") {
    if(firstClass(processOutput) == "SpatialPolygonsDataFrame") {
        geojsonio::geojson_json(processOutput)
    }
    else if(firstClass(processOutput) == "data.table") {
        # Check whether the table is rugged:
        if(isDataTableRugged(processOutput)) {
            flattenDataTable(processOutput)
        }
    }
    else {
        stop("Invalid process output.")
    }
}

# Function to get all process output memory files of a process:
#' 
#' @inheritParams Projects
#' @export
#' 
getProcessOutputFiles <- function(projectPath, modelName, processID, onlyTableNames = FALSE) {
    
    # Function to list RDS file in a folder:
    listRDSFiles <- function(folderPath) {
        # Create a list of the files, and name it with the file names sans ext:
        out <- as.list(list.files(folderPath, full.names = TRUE, pattern = "\\.rds$"))
        names(out) <- basename(tools::file_path_sans_ext(unlist(out)))
        
        # Read the order file if present:
        orderFile <- file.path(folderPath, "tableOrder.txt")
        if(file.exists(orderFile)) {
            tableOrder <- readLines(orderFile)
            tableOrder <- basename(tools::file_path_sans_ext(unlist(tableOrder)))
            out <- out[tableOrder]
        }
        
        out
    }
    
    # Get the directory holding the output files:
    folderPath <- getProcessOutputFolder(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID
    )
    
    # If the folder does not exist, it is a sign that the process does not exist:
    if(length(folderPath) == 0 || !file.exists(folderPath)) {
        #processName <- getProcessName(projectPath, modelName, processID)
        stop("The folder ", folderPath, " does not exist. This is likely due to non-existing process")
    }
    
    # Detect whether the output is a list of tables (depth 1) or a list of lists of tables (depth 2):
    folderDepth <- getFolderDepth(folderPath)
    
    # Get the file paths of the memory files and prepare the processOutput for writing to these files:
    if(folderDepth == 1) {
        processOutputFiles <- listRDSFiles(folderPath)
    }
    else {
        # Get the sub folder paths and create the folders:
        folderPaths <- list.dirs(folderPath, recursive = FALSE)
        processOutputFiles <- lapply(folderPaths, listRDSFiles)
        names(processOutputFiles) <- basename(folderPaths)
    }
    
    if(onlyTableNames) {
        # Strip the table names of the folderPath:
        processOutputFiles <- gsub(path.expand(folderPath), "", unname(unlist(processOutputFiles)))
        # Remove the resulting trailing "/" and the file extension:
        processOutputFiles <- substring(processOutputFiles, 2)
        processOutputFiles <- tools::file_path_sans_ext(processOutputFiles)
    }
    
    processOutputFiles
}

#' 
#' @inheritParams Projects
#' @export
#' 
getProcessOutputTableNames <- function(projectPath, modelName, processID) {
    # Get the output file names, and add the process name:
    tableNames <- getProcessOutputFiles(projectPath, modelName, processID, onlyTableNames = TRUE)
    processName <- getProcessName(projectPath, modelName, processID)
    tableNames <- paste(processName, tableNames, sep ="_")
    
    # Ensure that this is a vector in JSON after auto_unbox = TRUE, by using as.list():
    tableNames <- as.list(tableNames)
    print(tableNames)
    return(tableNames)
}


deleteProcessOutput <- function(projectPath, modelName, processID) {
    # Get the directory holding the output files:
    folderPath <- getProcessOutputFolder(projectPath = projectPath, modelName = modelName, processID = processID)
    unlink(folderPath, recursive = FALSE, force = TRUE)
}

# Function for reading all RDS files in a folder non-recursively:
readFolderWithRSDFiles <- function(folderPath) {
    # Get the paths to the output files:
    filePaths <- list.files(folderPath, full.names = FALSE, pattern = "\\.rds$")
    # Read the files to a list:
    processOutput <- lapply(filePahts, readRDS)
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
    # Return NULL for empty process output:
    if(length(processOutput)) {
        # Unlist introduces dots, and we replace by underscore:
        processOutput <- unlistToDataType(processOutput)
        #names(processOutput) <- gsub(".", "_", names(processOutput), fixed = TRUE)
        
        folderPath <- getProjectPaths(
            projectPath = projectPath, 
            #name = paste(getRstoxFrameworkDefinitions("paths")$stoxFolders["Output"], modelName, sep = "_")
            name = modelName
        )
        
        processIndex <- getProcessIndexFromProcessID(projectPath = projectPath, modelName = modelName, processID = process$processID)
        fileNamesSansExt <- paste(processIndex, process$processName, names(processOutput), sep = "_")
        filePathsSansExt <- file.path(folderPath, paste(fileNamesSansExt))
        
        # Set the file name:
        mapply(reportFunctionOutputOne, processOutput, filePathsSansExt)
        # lapply(processOutput, reportFunctionOutputOne)
    }
    else {
        NULL
    }
}

# Function for writing one element of the function output list:
reportFunctionOutputOne <- function(processOutputOne, filePathSansExt) {
    if(length(processOutputOne)){
        #if("SpatialPolygons" %in% class(processOutputOne)) {
        if("SpatialPolygonsDataFrame" %in% class(processOutputOne)) {
            # Add file extension:
            filePath <- paste(filePathSansExt, "geojson", sep = ".")
            # Write the file:
            jsonObject <- geojsonio::geojson_json(processOutputOne)
            
            # Hack to rermove all IDs from the geojson:
            jsonObject <- removeIDsFromGeojson(jsonObject)
            
            jsonlite::write_json(jsonObject, path = filePath)
        }
        else if("data.table" %in% class(processOutputOne)) {
            # Add file extension:
            filePath <- paste(filePathSansExt, "txt", sep = ".")
            # Write the file:
            data.table::fwrite(processOutputOne, filePath, sep = "\t")
        }
        else {
            stop("Unknown function output: ", class(processOutputOne))
        }
    }
}

removeIDsFromGeojson <- function(json) {
    json[[1]] <- gsub(",\\s*\\\"id\\\":[\\\"a-zA-Z1-9_]*", "", json[[1]])
    json
}



# Function to flatten the list and add names from the levels of the list:
unlistToDataType <- function(processOutput) {
    
    # Function to check that all the output elements are of the valid classes:
    areAllValidOutputDataClasses <- function(processOutput) {
        validOutputDataClasses <- getRstoxFrameworkDefinitions("validOutputDataClasses")
        classes <- sapply(processOutput, firstClass)
        #classes <- unlist(lapply(classes, "[[", 1))
        all(classes %in% validOutputDataClasses)
    }
    
    
    unlistOne <- function(processOutput) {
        # Unlist and add the names:
        if(!areAllValidOutputDataClasses(processOutput)){
            # Define the names of the files first, by pasting the level and the sub-level names separated by underscore:
            processOutputNames <- unlist(lapply(names(processOutput), function(x) paste(x, names(processOutput[[x]]), sep = "_")))
            # Unlist down one level:
            processOutput <- unlist(processOutput, recursive = FALSE)
            # Add the names again:
            names(processOutput) <- processOutputNames
        }
        
        processOutput
    }
    
    # Unlist through 2 levels:
    for(i in seq_len(2)) {
        processOutput <- unlistOne(processOutput)
    }
    
    return(processOutput)
}


#' 
#' @export
#' 
writeProcessOutputMemoryFile <- function(processOutput, process, projectPath, modelName) {
    
    if(length(processOutput)) {
        
        saveRDSs <- function(objects, files) {
            orderFileName <- file.path(dirname(files[1]), "tableOrder.txt")
            write(files, orderFileName)
            mapply(saveRDS, objects, files)
        }
        
        # Get the path to the folder to place the memory file in:
        folderPath <- getProcessOutputFolder(projectPath = projectPath, modelName = modelName, processID = process$processID)
        # Create the folder:
        dir.create(folderPath, recursive = TRUE, showWarnings = FALSE)
        
        # Detect whether the output is a list of tables (depth 1) or a list of lists of tables (depth 2):
        outputDepth <- getOutputDepth(processOutput)
        
        # Get the file paths of the memory files and prepare the processOutput for writing to these files:
        if(outputDepth == 1) {
            fileNames <- getProcessOutputMemoryFileNames(processOutput)
            #filePaths <- file.path(folderPath, fileNames)
            filePaths <- file.path(folderPath, fileNames)
            
            # Wrap in a list to coordinate using the saveRDSs():
            filePaths <- list(filePaths)
            processOutput <- list(processOutput)
        }
        else {
            # Get the sub folder paths and create the folders:
            folderPaths <- file.path(folderPath, names(processOutput))
            lapply(folderPaths, dir.create, recursive = TRUE, showWarnings = FALSE)
            
            # Create the file names and add the folder paths to the file names (flattening the output):
            fileNames <- lapply(processOutput, getProcessOutputMemoryFileNames)
            #filePaths <- unlist(mapply(file.path, folderPaths, fileNames, SIMPLIFY = FALSE))
            filePaths <- mapply(file.path, folderPaths, fileNames, SIMPLIFY = FALSE)
            
            # Flatten the processOutput:
            #processOutput <- unlist(processOutput, recursive = FALSE)
        }
        
        # Write the individual tables:
        mapply(saveRDSs, processOutput, filePaths)
    }
    else {
        NULL
    }
    
}
# Function to get the depth of the data, 1 for a list of valid output data objects, and 2 for a list of such lists:
getOutputDepth <- function(x) {
    outputDepth <- 1
    validOutputDataClasses <- getRstoxFrameworkDefinitions("validOutputDataClasses")
    if(is.list(x[[1]]) && length(x[[1]]) && firstClass(x[[1]][[1]]) %in% validOutputDataClasses) {
        outputDepth <- 2
    }
    outputDepth
}
# Function to get the folder of the memory files, 1 for all files in one folder, and 2 for a subfolders:
getFolderDepth <- function(folderPath) {
    # List the files in the folder:
    filePaths <- list.dirs(folderPath, recursive = FALSE)
    folderDepth <- 1
    if(length(filePaths)) {
        folderDepth <- 2
    }
    folderDepth
}

# Small function to get the file name of a memory file:
getProcessOutputMemoryFileNames <- function(processOutput) {
    paste(names(processOutput), "rds", sep = ".")
}


#' 
#' @export
#' 
#runModel <- function(projectPath, modelName, startProcess = 1, endProcess = Inf, save = TRUE, force = FALSE) {
runProcesses <- function(projectPath, modelName, startProcess = 1, endProcess = Inf, save = TRUE, force.restart = FALSE) {
        
    ## Get the processIDs:
    #processIndexTable <- readProcessIndexTable(projectPath, modelName)
    ## Rstrict the startProcess and endProcess to the range of process indices:
    #startProcess <- max(1, startProcess)
    #endProcess <- min(nrow(processIndexTable), endProcess)
    ## Extract the requested process IDs:
    #processIDs <- processIndexTable[seq(startProcess, endProcess)]$processID
    processIDs <- readProcessIndexTable(projectPath, modelName, startProcess = startProcess, endProcess = endProcess)$processID
    
    # Check that the project exists:
    failedVector <- logical(length(processIDs))
    if(!isProject(projectPath)) {
        warning("The StoX project ", projectPath, " does not exist")
        return(failedVector)
    }
    
    # Check that the project is open:
    if(!isOpenProject(projectPath)) {
        warning("The StoX project ", projectPath, " is not open")
        return(failedVector)
    }
    
    # Chech that none of the models of the project are running:
    if(isRunning(projectPath) && !force.restart) {
        warning("The project is running (", projectPath, "). Use force.restart = TRUE to force restart the project.")
        return(failedVector)
    }
    else {
        setRunning(projectPath)
    }
    
    on.exit({
        setNotRunning(projectPath)
    })

    
    # Loop through the processes:
    #status <- logical(length(processIDs))
    #names(status) <- processIDs
    
    #err <- NULL
    
    # Try running the processes, and retun the failedVector if craching:
    #tryCatch(
    #    {
            for(processID in processIDs) {
                #status[processID] <- runProcess(projectPath = projectPath, modelName = modelName, processID = processID)
                runProcess(projectPath = projectPath, modelName = modelName, processID = processID)
            }
   #     }#, 
        #error = function(e) {
        #    err <<- e
        #    stop(err)
        #}, 
        #finally = {
        #    
            
            #if(length(err)) {
            #    stop(err)
            #}
            
            #return(status)
        #}
    #)
    
    #status
    
    # Save the project after each run:
    if(save) {
        saveProject(projectPath)
    }
    
    #status
    list(
        activeProcessID = utils::tail(processID, 1), 
        interactiveMode = getInteractiveMode(projectPath, modelName, processID)
    )
    
}

#' 
#' @export
#' 
runModel <- function(projectPath, modelName, startProcess = 1, endProcess = Inf, run = TRUE, save = TRUE, force.restart = FALSE) {
    # Un the model if required:
    if(run) {
        runProcesses(
            projectPath = projectPath, 
            modelName = modelName, 
            startProcess = startProcess, 
            endProcess = endProcess, 
            save = save, 
            force.restart = force.restart
        )
    }
    # Get the model data:
    modelData <- getModelData(
        projectPath = projectPath, 
        modelName = modelName, 
        startProcess = startProcess, 
        endProcess = endProcess
    )
    
    return(modelData)
}



#' 
#' @export
#' 
runFunction <- function(what, args, removeCall = TRUE, onlyStoxMessages = TRUE) {
    
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
                do.call(what, args), 
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


#' 
#' @export
#' 
getModelData <- function(projectPath, modelName, startProcess = 1, endProcess = Inf) {
    
    processTable <- readProcessIndexTable(projectPath, modelName, startProcess = startProcess, endProcess = endProcess)
    
    processOutput <- mapply(
        getProcessOutput, 
        projectPath = projectPath, 
        modelName = modelName, 
        processTable$processID
    )
    names(processOutput) <- processTable$processName
    
    return(processOutput)
}



##################################################
##################################################
#' Convert a project from StoX 2.7 to the current StoX
#' 
#' This function applies the conversion functions defined in the \code{stoxFunctionAttributes} list defined in each StoX function package.
#' 
#' @noRd
#' @export
#' 
convertProjectDescription1.92 <- function(projectDescription) {
    # Get the current project description:
    #projectDescription <- getProjectMemoryData(projectPath)
    
    ## Get the StoX version:
    #StoxVersion <- attr(projectDescription, "StoxVersion")
    #if(resourceversion < "1.92") {
    #    stop("Backward compatibility not supported for versions of StoX prior to 2.7")
    #}
    
    # Checkc the version, issuing an error if resourceversion is set and lower than "1.92":
    checkVersion(projectDescription)
    
    newProjectDescription <- list()
    
    
    #for(modelName in )
    
    
    
    convertProcess <- function(projectDescription, modelName, processID) {
        
        process <- list()
        
        # Extract proecess name:
        projectDescription <- appendProjectDescription(
            projectDescription = projectDescription, 
            modelName = modelName, 
            processID = processID, 
            argumentName = "processName", 
            argumentValue = 
        )
        
        # Extract function name:
        
        # Extract proecess parameters:
        
        # Extract proecess data:
        
        # Extract function input:
        
        # Extract function parameters:
    }
    
    
    
    
    
}


##################################################
##################################################
#' Check the version for backwards compatibility
#' 
#' @noRd
#' @export
#'
checkVersion <- function(projectDescription, resourceVersion = NULL, RstoxFrameworkVersion = NULL) {
    # Get the StoxVersion from the attributes:
    savedResourceVersion <- attr(projectDescription, "resourceversion")
    savedRstoxFrameworkVersion <- attr(projectDescription, "RstoxFrameworkVersion")
    
    # Issue an error if the project.xml is before the backwards compatibility time limit:
    if(savedResourceVersion < "1.92") {
        stop("Backward compatibility not supported for versions of StoX prior to 2.7 (resourceversion 1.92)")
    }
    
    if(length(resourceVersion) && savedResourceVersion == resourceversion) {
        return(TRUE)
    }
    else if(length(RstoxFrameworkVersion) && savedRstoxFrameworkVersion == RstoxFrameworkVersion) {
        return(TRUE)
    }
    else {
        return(FALSE)
    }
}


