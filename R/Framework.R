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
    
    #### The folders, data sources, model types and data types in a StoX project: ####
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
    
    # Define the folder structure of StoX:
    stoxFolderStructure <- list(
        stoxDataSources, 
        stoxModelTypes, 
        ""
    )
    names(stoxFolderStructure) <- stoxFolders
    stoxFolderStructure <- unname(unlist(mapply(file.path, names(stoxFolderStructure), stoxFolderStructure)))
    
    # Define the folders and paths used when a project is open:
    projectSessionFolder <- file.path(stoxFolders["Process"], "projectSession")
    dataFolder <- file.path(projectSessionFolder, "data")
    GUIFolder <- file.path(projectSessionFolder, "GUI")
    projectDescriptionFolder <- file.path(projectSessionFolder, "projectDescription")
    settingsFolder <- file.path(projectSessionFolder, "settings")
    originalProjectDescriptionFile <- file.path(projectDescriptionFolder, "originalProjectDescription.rds")
    currentProjectDescriptionFile <- file.path(projectDescriptionFolder, "currentProjectDescription.rds")
    projectDescriptionIndexFile <- file.path(projectDescriptionFolder, "projectDescriptionIndex.txt")
    
    
    
    # Define the process parameters with default values:
    processParameters <- list(
        Enabled = TRUE, 
        BreakInGUI = FALSE, 
        FileOutput = TRUE
    )
    
    # Do we need this???????:
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
    
    
    
    #### Assign to RstoxEnv and return the definitions: ####
    definitions <- list(
        stoxFolders = stoxFolders, 
        stoxDataSources = stoxDataSources, 
        stoxModelTypes = stoxModelTypes, 
        stoxFolderStructure = stoxFolderStructure, 
        # 
        projectSessionFolder = projectSessionFolder, 
        dataFolder = dataFolder, 
        GUIFolder = GUIFolder, 
        projectDescriptionFolder = projectDescriptionFolder, 
        settingsFolder = settingsFolder, 
        originalProjectDescriptionFile = originalProjectDescriptionFile, 
        currentProjectDescriptionFile = currentProjectDescriptionFile, 
        projectDescriptionIndexFile = projectDescriptionIndexFile, 
        # 
        processParameters = processParameters, 
        stoxModelDataTypes = stoxModelDataTypes, 
        stoxProcessDataTypes = stoxProcessDataTypes, 
        stoxDataTypes = stoxDataTypes, 
        # This is defined in the file Templates.R:
        stoxTemplates = stoxTemplates, 
        projectRData = "project.RData", 
        projectXML = "project.xml"
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


createProjectSkeleton <- function(ProjectPath) {
    
    # Get the paths of the root directory and StoX skeleton:
    stoxFolderStructure <- getRstoxFrameworkDefinitions("stoxFolderStructure")
    
    # Check whether the project exists:
    if(dir.exists(ProjectPath)) {
        warning("The project '", ProjectPath, "' exists. Choose a different project path.")
        return(NULL)
    }
    else {
        ProjectSkeleton <- file.path(ProjectPath, stoxFolderStructure)
        lapply(ProjectSkeleton, dir.create, showWarnings = FALSE, recursive = TRUE)
    }
    
    # Return the paths:
    ProjectSkeleton
}



createProject <- function(ProjectPath, Template = "EmptyTemplate") {
    
    # Create the project folder structure:
    projectSkeleton <- createProjectSkeleton(ProjectPath)
    
    # Get the tempaltes:
    templates <- getAvaiableTemplates()
    thisTemplate <- templates[[Template]]
    if(length(thisTemplate) == 0) {
        stop("The requested template does not exist. See getAvaiableTemplates() for a list of the available templates (with list.out = TRUE if you wish to see what the dirrefent templates are.)")
    }
    
    # Create an empty ProjectDescription:
    projectDescription <- createEmptyProjectDescription()
    
    # Fill inn the processes::::::::::::::::::::::
}

createEmptyProjectDescription <- function() {
    # Get the model types, and populate a list with these:
    modelTypes <- getRstoxFrameworkDefinitions("stoxModelTypes")
    projectDescription <- vector("list", length(modelTypes))
    names(projectDescription) <- modelTypes
    projectDescription
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










##### Manage (write, read, undo, redo) the project description: #####

# 1. Funciton to get the path to a new project description file:
#' 
#' @export
#' 
getNewProjectDescriptionFilePath <- function(ProjectPath) {
    # Get the folder holding the project descriptions:
    projectDescriptionFolder <- getRstoxFrameworkDefinitions("projectDescriptionFolder")
    # Define a string with time in ISO 8601 format:
    timeString <- format(Sys.time(), tz = "UTC", format = "%y-%m-%dT%H:%M:%OS3Z")
    # Define the file name including the time string, and build the path to the file:
    fileName <- paste0("projectDescription", "_", timeString, ".rds")
    filePath <- file.path(ProjectPath, projectDescriptionFolder, fileName)
    filePath
}


# 5. Function to read the current project description:
#' 
#' @export
#' 
getCurrentProjectDescription <- function(ProjectPath) {
    currentProjectDescriptionFile <- getRstoxFrameworkDefinitions("currentProjectDescriptionFile")
    readRDS(currentProjectDescriptionFile)
}

# 6. Function to write the current project description:
#' 
#' @export
#' 
setCurrentProjectDescription <- function(ProjectPath, projectDescription) {
    # Get the new project description file path and write the project description to this file:
    newProjectDescriptionFilePath <- getNewProjectDescriptionFilePath(ProjectPath)
    saveRDS(projectDescription, file = newProjectDescriptionFilePath)
    
    # Save also to the currentProjectDescriptionFile:
    currentProjectDescriptionFile <- getRstoxFrameworkDefinitions("currentProjectDescriptionFile")
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
        c(0, newProjectDescriptionFilePath)
    )
    
    # Write the projectDescriptionIndex to file:
    writeProjectDescriptionIndexFile(ProjectPath, projectDescriptionIndex)
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
    file.copy(fileWithCurrentProjectDescription, getRstoxFrameworkDefinitions("currentProjectDescriptionFile"))
}


# 7.1 Function to read the projectDescriptionIndexFile:
#' 
#' @export
#' 
readProjectDescriptionIndexFile <- function(ProjectPath) {
    # Read the projectDescriptionIndexFile:
    projectDescriptionIndexFile <- file.path(ProjectPath, getRstoxFrameworkDefinitions("projectDescriptionIndexFile"))
    projectDescriptionIndex <- data.table::fread(projectDescriptionIndexFile)
    projectDescriptionIndex
}

# 7.2 Function to write the projectDescriptionIndexFile:
#' 
#' @export
#' 
writeProjectDescriptionIndexFile <- function(ProjectPath, projectDescriptionIndex) {
    # Read the projectDescriptionIndexFile:
    projectDescriptionIndexFile <- file.path(ProjectPath, getRstoxFrameworkDefinitions("projectDescriptionIndexFile"))
    data.table::fwrite(projectDescriptionIndex, file =  projectDescriptionIndexFile)
}

















ModifyFunctionName <- function(ProcessName, ModelName, ProjectName, NewFunctionName) {
    
    # Get the project description:
    projectDescription <- getCurrentProjectDescription(ProjectName)
    
    
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
    
    Process
}

ModifyProcessName <- function(ProcessName, Model, NewProcessName) {
    # Set the process name:
    Process$ProcessName <- ProcessName
    
    Process
}

ModifyFunctionParameters <- function(Process, FunctionParameters) {
    
    # Report a warning for function parameters not present in the process:
    valid <- names(FunctionParameters) %in% names(Process$FunctionParameters)
    if(any(!valid)) {
        warning("The following function parameters are not present for the function ", Process$FunctionName, " of the process ", Process$ProcessName, ": ", paste(names(FunctionParameters)[!valid], collapse = ", "))
        FunctionParameters <- FunctionParameters[valid]
    }
    
    # Insert the function parameters:
    for(ind in seq_along(FunctionParameters)) {
        Process$FunctionParameters[[names(FunctionParameters[ind])]] <- FunctionParameters[[ind]]
    }
    
    Process
}

ModifyFunctionInputs <- function(Process, FunctionInputs) {
    
    # Report a warning for function inputs not present in the process:
    valid <- names(FunctionInputs) %in% names(Process$FunctionInputs)
    if(any(!valid)) {
        warning("The following function inputs are not present for the function ", Process$FunctionName, " of the process ", Process$ProcessName, ": ", paste(names(FunctionInputs)[!valid], collapse = ", "))
        FunctionInputs <- FunctionInputs[valid]
    }
    
    # Insert the function inputs:
    for(ind in seq_along(FunctionInputs)) {
        Process$FunctionInputs[[names(FunctionInputs[ind])]] <- FunctionInputs[[ind]]
    }
    
    Process
}

ModifyProcessParameters <- function(Process, ProcessParameters) {
    
    # Get names of the process parameters:
    validProcessParameterNames = names(getRstoxFrameworkDefinitions("processParameters"))
    
    # Report a warning for non-existing process parameters:
    valid <- names(ProcessParameters) %in% validProcessParameterNames
    if(any(!valid)) {
        warning("The following process parameters are not valid: ", paste(names(ProcessParameters)[!valid], collapse = ", "))
        ProcessParameters <- ProcessParameters[valid]
    }
    
    # Insert the process parameters:
    for(ind in seq_along(ProcessParameters)) {
        Process$ProcessParameters[[names(ProcessParameters[ind])]] <- ProcessParameters[[ind]]
    }
    
    Process
}



# This function checks that 
checkModel <- function(ProjectName, ModelName) {
    
    # Function to check that the function inputs of one process are all process names existing prior to that function:
    checkFunctionInputs <- function(ind, functionInputs, processNames) {
        all(functionInputs %in% processNames[seq_len(ind - 1)])
    }
    
    # Get the processes of the model:
    processes <- getProcesses(ProjectName, ModelName)
    
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






GetProjectDescription <- function(ProjectName, position = 0) {
    
}


#GetProjectDescriptionName <- 



SaveProjectDescription <- function(ProjectName, ProjectDescription, mode = c("Memory", "File")) {
    
}
    
    

#AppendProcess <- function(ProjectName, ModelName, Process) {
#    
#    # Get the project name:
#    ProcessName <- Process$ProcessName
#    
#    # Get project description:
#    ProjectDescription <- GetProjectDescription(ProjectName)
#    
#    # Check whether the process name is already present in the model:
#    if(Process$ProcessName %in% names(ProjectDescription[[ModelName]])){
#        stop(paste0("The process ", Process$ProcessName, " is already present in the model ", ModelName, #" of project ", ProjectName))
#    }
#    
#    # Append the process:
#    ProjectDescription[[ModelName]][[ProcessName]] <- Process
#    
#    # Save the project description:
#    SaveProjectDescription <- 
#}


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
getOutputFileNames <- function(processName, ProjectName, fileExt="txt") {
    
    # Get the status of the project:
    status <- getProjectStatus(ProjectName)
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


### ##################################################
### ##################################################
### #' Get paths to the project directories and files
### #' 
### #' This function gets the paths to the top level, input and output folders, and the project recipe ### file.
### #' 
### #' @param ProjectName   The directory in which to put the "stox" folder, defaulted to the "workspace### " folder in the home directory.
### #' @param ProjectDirectory   The directory in which to put the "stox" folder, defaulted to the ### "workspace" folder in the home directory.
### #' 
### #' @return
### #' A list of paths to the "stox" folder and sub folders.
### #' 
### #' @examples
### #' getProjectPaths()
### #' 
### #' @export
### #' @seealso Use \code{\link{createStoxSkeleton}} to create the folders.
### #' 
### getProjectPaths <- function(ProjectName, ProjectDirectory = NULL) {
###     
###     # Small function to convert a vector to a list and use the basenames as names:
###     addBasenameAsListNames <- function(x) {
###         out <- as.list(x)
###         names(out) <- basename(x)
###         out
###     }
###     
###     # Get the paths to StoX:
###     stoxPaths <- getStoxSkeletonPaths(ProjectDirectory = ProjectDirectory)
###     # Get the project path:
###     projectPath <- file.path(stoxPaths$project, ProjectName)
###     
###     # Get project folder names:
###     projectFolders <- getRstoxFrameworkDefinitions()
###     
###     # Define top level of the project:
###     projectDirs <- file.path(
###         projectPath, 
###         projectFolders$stoxFolders
###     )
###     
###     # Define input folders of the project:
###     projectInputDirs <- file.path(
###         projectPath, 
###         projectFolders$stoxFolders["input"], 
###         projectFolders$stoxDataSources
###     )
###     
###     # Define output folders of the project:
###     projectOutputDirs <- file.path(
###         projectPath, 
###         projectFolders$stoxFolders["output"], 
###         projectFolders$stoxModelTypes
###     )
###     
###     # Define project file:
###     projectXML <- file.path(projectPath, projectFolders$projectXML)
###     
###     # Define list of all folder and file names of the project:
###     projectDirsList <- c(
###         addBasenameAsListNames(projectDirs), 
###         addBasenameAsListNames(projectInputDirs), 
###         addBasenameAsListNames(projectOutputDirs), 
###         list(projectXML = projectXML)
###     )
###     
###     # Return a vector of paths to the individual folders:
###     list(
###         projectDirs = projectDirs, 
###         projectInputDirs = projectInputDirs, 
###         projectOutputDirs = projectOutputDirs, 
###         projectXML = projectXML, 
###         projectDirsList = projectDirsList
###     )
### }




