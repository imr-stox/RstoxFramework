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
    # Define the process property formats:
    processPropertyFormats <- list(
        default = "none", 
        single = list(
            "filePath", 
            "directoryPath"
        ), 
        vector = list(
            "filePaths"
        ), 
        list = list(
            "filterExpressionList"
        ), 
        table = list(
            "speciesCategoryTable", 
            "acousticCategoryTable", 
            "catchCompensationTable", 
            "selectivityTable", 
            "speciesLinktable", 
            "acousticTargetStrengthTable"
        )
    )
    
    # Define the column names of the different parameter tables:
    parameterTableColumnNames <- list(
        speciesCategoryTable = c(
            "SpeciesCategory", 
            "NewSpeciesCategory"
        ),
        acousticCategoryTable = c(
            "AcousticCategory", 
            "NewAcousticCategory"
        ),
        catchCompensationTable = c(
            "SpeciesCategory", 
            "Alpha", 
            "Beta", 
            "LMin", 
            "LMax"
        ),
        selectivityTable = c(
            "SpeciesCategory", 
            "Alpha", 
            "Beta", 
            "LMax"
        ),
        speciesLinktable = c(
            "AcousticCategory",
            "SpeciesCategory"
        ),
        acousticTargetStrengthTable = c(
            "AcousticCategory", 
            "m", 
            "a", 
            "d"
        ),
        variableConversionTable = c(
            "TableName", 
            "VariableName", 
            "Value", 
            "NewValue"
        )
    )
    
    # Define the titles of the different parameter tables:
    parameterTableTitle <- list(
        speciesCategoryTable = "Define new species categories",
        acousticCategoryTable = "Define new acoustic categories",
        catchCompensationTable = "Define parameters for length dependent catch compensation",
        selectivityTable = "Define parameters for length dependent selectivity",
        speciesLinktable = "Link acoustic categories and species categories",
        acousticTargetStrengthTable = "Define parameters of acoustic target strength by length"
    )
    
    # Define filter operators for the different data types:
    filterOperators <- list(
        character = c("==", "!=", "%in%", "%notin%"), 
        logical   = c("==", "!="), # This may never be used
        integer   = c("==", "!=", "<", "<=", ">=", ">", "%in%", "%notin%"),
        double    = c("==", "!=", "<", "<=", ">=", ">", "%in%", "%notin%"),
        numeric   = c("==", "!=", "<", "<=", ">=", ">", "%in%", "%notin%"),
        POSIXct   = c("==", "!=", "<", "<=", ">=", ">", "%in%", "%notin%")
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
    assignmentDataType <- "BioticAssignment"
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
        functionName = "", 
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
    
    currentMemoryFolder <- file.path(projectMemoryFolder, "current")
    historyMemoryFolder <- file.path(projectMemoryFolder, "history")
    
    statusFolder <- file.path(projectSessionFolder, "status")
    # Return also a vector of all session folders, to generate the folder structure recursively:
    projectSessionFolderStructure <- c(
        dataFolder, 
        GUIFolder, 
        projectMemoryFolder, 
        statusFolder, 
        currentMemoryFolder, 
        historyMemoryFolder
    )
    
    
    #### Project description: ####
    projectRDataFile <- file.path(stoxFolders["Process"], "project.RData")
    projectXMLFile <- file.path(stoxFolders["Process"], "project.xml")
    projectJSONFile <- file.path(stoxFolders["Process"], "project.json")
    projectSavedStatusFile <- file.path(statusFolder, "projectSavedStatus.txt")
    projectIsRunningFile <- file.path(statusFolder, "projectIsRunning.txt")
    #currentProcessFile = file.path(statusFolder, "currentProcess.txt")
    
    # Memory files:
    #originalProjectMemoryFile <- file.path(projectMemoryFolder, "originalProjectMemory.rds")
    currentProjectMemoryFile <- file.path(currentMemoryFolder, "currentProjectMemory.rds")
    projectMemoryIndexFile <- file.path(historyMemoryFolder, "projectMemoryIndex.txt")
    # The file containing a table of modelName, processID and processName, where the rows are ordered by the processIndex:
    processIndexTableFile <- file.path(currentMemoryFolder, "processIndexTable.txt")
    # The file containing a table of one row holding the index of the active process for each model (columns named by the model names):
    activeProcessIDFile <- file.path(currentMemoryFolder, "activeProcessID.txt")
    # The file containing a table of one row holding the maximum process ID (sequential integer starting from 1 at the firstly generated process) for each model (columns named by the model names):
    maxProcessIntegerIDFile <- file.path(currentMemoryFolder, "maxProcessIntegerID.txt")
    
    
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
            currentMemoryFolder = currentMemoryFolder, 
            historyMemoryFolder = historyMemoryFolder, 
            statusFolder = statusFolder, 
            projectSessionFolderStructure = projectSessionFolderStructure, 
            
            # Project description:
            projectRDataFile = projectRDataFile, 
            projectXMLFile = projectXMLFile, 
            projectJSONFile = projectJSONFile, 
            projectSavedStatusFile = projectSavedStatusFile, 
            projectIsRunningFile = projectIsRunningFile, 
            #currentProcessFile = currentProcessFile, 
            #originalProjectMemoryFile = originalProjectMemoryFile, 
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
    #utils::globalVariables("RstoxFrameworkEnv")
    utils::globalVariables(c(
        "RstoxFrameworkEnv", 
        ":=", ".", 
        "..PSU", 
        "..activeProcessID", 
        "..clickPointNames", 
        "..coordinateNames", 
        "..functionInputs", 
        "..functionName", 
        "..functionParameters", 
        "..infoToKeep", 
        "..modified", 
        "..newProcessName", 
        "CruiseKey", 
        "Latitude", 
        "Latitude2", 
        "LogOrigin", 
        "LogOrigin2", 
        "Longitude", 
        "Longitude2", 
        "PSU", 
        "atRemove", 
        "canShowInMap", 
        "filePahts", 
        "functionName", 
        "functionOutputDataType", 
        "hasBeenRun", 
        "hasProcessData", 
        "modelName", 
        "modified", 
        "name", 
        "possibleValues", 
        "processID", 
        "projectPath", 
        "value"
    ))
        
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
