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
    
    memoryFileFormat <- "rds"
    
    ##### Packages: #####
    officialStoxLibraryPackages <- c(
        "RstoxBase", 
        "RstoxData"
        #"RstoxFDA", 
        #"RstoxAnalysis", 
        #"RstoxReport"
    )
    
    # Define formats for files saved by Rstox:
    memoryFileFormat_Empty <- "rds"
    # 2020-06-08: The fst::write_fst() does not retain the encoding, and has been discarded until these problems are fixed:s
    #memoryFileFormat_Table <- "fst"
    memoryFileFormat_Table <- "rds"
    memoryFileFormat_Spatial <- "rds"
    memoryFileFormat_List <- "rds"
    memoryFileFormat_Other <- "rds"
    allMemoryFileFormats <- unique(
        c(
            memoryFileFormat_Empty, 
            memoryFileFormat_Table, 
            memoryFileFormat_Spatial, 
            memoryFileFormat_List, 
            memoryFileFormat_Other
        )
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
    
    # Load the required packages to enable searching for formals and documentation, e.g. for getStoxFunctionParameterPossibleValues():
    lapply(officialStoxLibraryPackages, library, character.only = TRUE)
    
    # Get the stoxLibrary as the list of function attributes from all official packages:
    stoxLibrary <- getStoxLibrary(officialStoxLibraryPackages, requestedFunctionAttributeNames = requestedFunctionAttributeNames)
    availableFunctions <- names(stoxLibrary)
    
    # Get the possible values of the functions:
    availableFunctionPossibleValues <- lapply(availableFunctions, extractStoxFunctionParameterPossibleValues)
    names(availableFunctionPossibleValues) <- availableFunctions
    
    # Get the json schema for RstoxFramework:
    schema <- jsonlite::read_json(system.file("formats", "projectSchema.json", package = "RstoxFramework"))
    
    # Get the schemas of the Rstox packages:
    processDataSchemas <- lapply(officialStoxLibraryPackages, readProcessDataSchema)
    processDataSchemas <- unlist(processDataSchemas, recursive = FALSE)
    
    # Get the names of the processData schemas:
    processDataSchemaNames <- names(processDataSchemas)
    processDataSchema <- list(
        processData = list(
            oneOf = lapply(
                processDataSchemaNames, 
                function(x) list(
                    "$ref" = paste0("\"#/", x, "\"")
                )
            ) 
        )
    )
    
    # Paste the subSchemas to the RstoxFramework schema:
    schema <- jsonlite::toJSON(
        c(
            schema, 
            processDataSchema, 
            processDataSchemas
        ), 
        pretty = TRUE, 
        auto_unbox = TRUE
    )
    # Create a project.json validator:
    projectValidator <- jsonvalidate::json_validator(schema)
    
   
    
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
        functionType = sapply(stoxLibrary, "[[", "functionType"), 
        functionName = availableFunctions, 
        packageName = sapply(stoxLibrary, "[[", "packageName")
    )
    
    # Check that there are no functions with the same name as a datatype:
    commonFunctionAndDataTypeName <- intersect(stoxDataTypes$functionOutputDataType, stoxDataTypes$functionName)
    if(length(commonFunctionAndDataTypeName)) {
        warning("The function name ", paste0("\"", commonFunctionAndDataTypeName, "\"", collapse = ", "), " of the package ", paste0("\"", stoxDataTypes[functionName == commonFunctionAndDataTypeName, "packageName"], "\"", collapse = ", "),  " is identical to the name of a data type. This may lead to unexpected errors when overriding a model using 'replaceArgs' and '...' in RstoxBase::runProcesses() and RstoxAPI::runModel(). Please notify the packcage maintainer.")
    }
    
    
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
        "SpatialPolygonsDataFrame"
    )
    
    # Define code words for the start and end of files to write geojson data to, which are read into the project.json after being written for a project:
    spatialFileReferenceCodeStart <- "<stratumpolygontempfile:"
    spatialFileReferenceCodeEnd <- ":stratumpolygontempfile>"
    
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
    
    # Function getting formats of a package:
    getProcessPropertyFormats <- function(packageName) {
        processPropertyFormats <- tryCatch(
            getExportedValue(packageName, "processPropertyFormats"), 
            error = function(err) NULL
        )
        return(processPropertyFormats)
    }
    
    # Get the processPropertyFormats of all packages, and merge the lists and add the default ("none"):
    processPropertyFormats <- unlist(
        c(
            lapply(officialStoxLibraryPackages, getProcessPropertyFormats), 
            # Add the default format "none"
            list(defaultProcessPropertyFormat)
        ), 
        recursive = FALSE
    )
    
    #
    #allFormatClasses <- unique(unlist(lapply(processPropertyFormats, names)))
    #processPropertyFormats <- lapply(allFormatClasses, function(x) unlist(lapply(processPropertyFormats, "[[", x)))
    #names(processPropertyFormats) <- allFormatClasses
    
    # Get the parameterTableInfo from all packages, and combine into a list:
    parameterTableInfo <- processPropertyFormats[sapply(processPropertyFormats, "[[", "type") == "table"]

    
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
    )
    
    # Define the data types for the interactive modes:
    stratumDataType <- "StratumPolygon"
    acousticPSUDataType <- "AcousticPSU"
    sweptAreaPSUDataType <- "SweptAreaPSU"
    acousticLayerDataType <- "AcousticLayer"
    sweptAreaLayerDataType <- "SweptAreaLayer"
    bioticAssignmentDataType <- "BioticAssignment"
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
    
    # Sub folders 1:
    dataFolder <- file.path(projectSessionFolder, "data")
    memoryFolder <- file.path(projectSessionFolder, "memory")
    statusFolder <- file.path(projectSessionFolder, "status")
    
    # Sub folders of the data folder:
    dataModelsFolder <- file.path(dataFolder, "models")
    dataModelsFolders <- file.path(dataModelsFolder, stoxModelFolders)
    
    # Sub folders of the memory folder:
    memoryCurrentFolder <- file.path(memoryFolder, "current")
    memoryHistoryFolder <- file.path(memoryFolder, "history")
    memoryModelsFolder <- file.path(memoryFolder, "models")
    memoryModelsFolders <- file.path(memoryModelsFolder, stoxModelFolders)
    
    memoryCurrentModelsFolder <- file.path(memoryCurrentFolder, "models")
    memoryCurrentModelsFolders <- file.path(memoryCurrentModelsFolder, stoxModelFolders)
    
    # Return also a vector of all session folders, to generate the folder structure recursively:
    projectSessionFolderStructure <- c(
        dataFolder, 
        memoryFolder, 
        statusFolder, 
        dataModelsFolder, 
        dataModelsFolders, 
        memoryCurrentFolder, 
        memoryHistoryFolder, 
        memoryModelsFolder, 
        memoryModelsFolders, 
        memoryCurrentModelsFolder, 
        memoryCurrentModelsFolders
    )
    
    
    #### Project description: ####
    projectRDataFile <- file.path(stoxFolders["Process"], "project.RData")
    projectXMLFile <- file.path(stoxFolders["Process"], "project.xml")
    projectJSONFile <- file.path(stoxFolders["Process"], "project.json")
    projectSavedStatusFile <- file.path(statusFolder, "projectSavedStatus.txt")
    projectIsRunningFile <- file.path(statusFolder, "projectIsRunning.txt")
    
    # Memory files:
    projectMemoryIndexFile <- file.path(memoryHistoryFolder, "projectMemoryIndex.txt")
    # The file containing a table of modelName, processID and processName, where the rows are ordered by the processIndex:
    processIndexTableFile <- file.path(memoryCurrentFolder, "processIndexTable.txt")
    # The file containing a table of one row holding the index of the active process for each model (columns named by the model names):
    activeProcessIDFile <- file.path(memoryCurrentFolder, "activeProcessID.txt")
    # The file containing a table of one row holding the maximum process ID (sequential integer starting from 1 at the firstly generated process) for each model (columns named by the model names):
    maxProcessIntegerIDFile <- file.path(memoryCurrentFolder, "maxProcessIntegerID.txt")
    
    
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
            memoryFolder = memoryFolder, 
            statusFolder = statusFolder, 
            dataModelsFolder = dataModelsFolder, 
            dataModelsFolders = dataModelsFolders, 
            memoryCurrentFolder = memoryCurrentFolder, 
            memoryHistoryFolder = memoryHistoryFolder, 
            memoryModelsFolder = memoryModelsFolder, 
            memoryModelsFolders = memoryModelsFolders, 
            memoryCurrentModelsFolder = memoryCurrentModelsFolder, 
            memoryCurrentModelsFolders = memoryCurrentModelsFolders, 
            
            projectSessionFolderStructure = projectSessionFolderStructure, 
            
            # Project description:
            projectRDataFile = projectRDataFile, 
            projectXMLFile = projectXMLFile, 
            projectJSONFile = projectJSONFile, 
            projectSavedStatusFile = projectSavedStatusFile, 
            projectIsRunningFile = projectIsRunningFile, 
            
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
        "..processDirty", 
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
        "processDirty", 
        "name", 
        "possibleValues", 
        "processID", 
        "projectPath", 
        "value"
    ))
        
    assign("RstoxFrameworkEnv", new.env(), parent.env(environment()))
    
    assign("definitions", definitions, envir=get("RstoxFrameworkEnv"))
    assign("projects", list(), envir=get("RstoxFrameworkEnv"))
    
    #### Return the definitions: ####
    definitions
}


# Define the default process property format:
defaultProcessPropertyFormat <- list(
    none = list(
        title = "Default format", 
        type = "single"
    )
)




readProcessDataSchema <- function(packageName) {
    # Get the file to the schema:
    schemaFile <- system.file("formats", "processDataSchema.json", package = packageName)
    if(nchar(schemaFile) > 0) {
        schema <- jsonlite::read_json(schemaFile)
    }
    else {
        schema <- NULL
    }
    
    return(schema)
}


extractStoxFunctionParameterPossibleValues <- function(functionName, dropProcessData = TRUE) {
    
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
    output <- f
    for(i in seq_along(f)) {
        assign(names(f[i]), if(!is.null(f[[i]])) output[[i]] <- eval(f[[i]]) else eval(f[[i]]))
    }
    
    return(output)
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
#' getRstoxFrameworkDefinitions("officialStoxLibraryPackages")
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


