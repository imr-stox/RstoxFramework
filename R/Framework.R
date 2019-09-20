- Relative path to files "${STOX}"

- Create the StoxRootDir and sub folders in OpenProject(), if missing

- We will use FunctionInputs and FunctionParameters.






Things to do in RstoxFramework:
    
1. Create project
2. Open project
3. Save project
4. SaveAs project: Creates a new project as a copy of the current status of the old project, and keeps the old project open, but shifts focus to the new.
5. Close project (1. Ask the user)
6. Reset project



# Functions for an isolated project object:
createEmptyBaselineProcess

modifyBaselineProcessName
modifyBaselineFunctionName
    modifyBaselineFunctionInputs
    modifyBaselineFunctionParameters
modifyBaselineProcessParameters
modifyBaselineProcessData



get_ftp_info <- function(ftp) {
    # Download the list of contents of the ftp server:
    destfile <- tempfile()
    download.file(ftp, destfile)
    # Pick out the last column of the table returned from the frp server:
    dirs <- readLines(destfile)
    permission <- substr(dirs, 1, 10)
    path <- substring(dirs, 57)
    
    data.frame(
        permission = permission, 
        path = path, 
        stringsAsFactors = FALSE
    )
}




download_files_ftp_recursive <- function(ftp, dir) {
    # Get the file names of the current ftp path:
    dirs <- get_ftp_info(ftp)
    
    # Recursive downloading:
    for(ind in seq_len(nrow(dirs))) {
        # Is the path a directory, then recurse into that directory:
        isdir <- startsWith(dirs$permission[ind], "d")
        # Define the current path in the for loop:
        #thisftp <- substr(ftp, 1, nchar(ftp) - 1)
        
        folderPath <- file.path(ftp, dirs$path[ind], "")
        folderPathSansSlash <- substr(folderPath, 1, nchar(folderPath) - 1)
        
         if(isdir) {
             download_files_ftp_recursive(folderPath, dir)
        }
        else {
            localFileName <- file.path(dir, basename(folderPathSansSlash))
            download.file(folderPathSansSlash, localFileName)
        } 
    }
    
}


l <- download_files_ftp_recursive("ftp://ftp.imr.no/StoX/Download/reference/", "~/workspace/Test")

downloadReferenceData <- function(ftp = "ftp://ftp.imr.no/StoX/Download/reference/", ) {
    
    referenceFiles <- list.files.ftp.recursive(ftp)
    
    localFiles <- 
    
    lapply(referenceFiles, download.file, )
    
}


createStoxRoot <- function(StoxRootDir = NULL) {
    # Get the default paths to the StoX root:
    if(length(StoxRootDir) == 0) {
        StoxRootDir <- getRstoxFrameworkDefinitions(StoxRootDir)
    }
    StoxProjectDir <- getRstoxFrameworkDefinitions(StoxProjectDir)
    StoxReferenceDir <- getRstoxFrameworkDefinitions(StoxReferenceDir)
    
    # Create the directories:
    dir.create(StoxRootDir)
    dir.create(StoxProjectDir)
    dir.create(StoxReferenceDir)
    
    # Copy the reference data to the StoxReferenceDir:
    downloadReferenceData()
    
}


##################################################
##################################################
#' Intitate RstoxFramework
#' 
#' This function writes vital definitions to the RstoxFramework environment.
#' 
#' @return
#' A list of paths to the "stox" folder and sub folders.
#' 
#' @noRd
#' @seealso Use \code{\link{getRstoxFrameworkDefinitions}} to get the definitions.
#' 
initiateRstoxFramework <- function(){
    
    #### Define the default root directories of StoX: ####
    stoxRootDir <- "~/workspace/stox"
    stoxProjectDir <- file.path(stoxRootDir, "project")
    stoxReferenceDir <- file.path(stoxRootDir, "reference")
    
    #### The folders, data sources, model types and data types in a Stox project: ####
    stoxFolders <- c(
        input = "input", 
        output = "output", 
        process = "process"
    )
    stoxDataSources <- c(
        acoustic = "acoustic", 
        biotic = "biotic", 
        landing = "landing"
    )
    stoxModelTypes <- c(
        baseline = "baseline", 
        analysis = "statistics", 
        report = "report"
    )
    
    # Define the folder structure of StoX:
    stoxFolderStructure <- list(
        stoxDataSources, 
        stoxModelTypes, 
        ""
    )
    names(stoxFolderStructure) <- stoxFolders
    stoxFolderStructure <- unname(unlist(mapply(file.path, names(stoxFolderStructure), stoxFolderStructure)))
    
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
        stoxModelDataTypes = stoxModelDataTypes, 
        stoxProcessDataTypes = stoxProcessDataTypes, 
        stoxDataTypes = stoxDataTypes, 
        stoxTemplates = stoxTemplates, 
        projectXML = "project.xml"
    )
    
    #### Create the RstoxFrameworkEnv environment, holding definitions on folder structure and all the projects. This environment cna be accesses using RstoxFramework:::RstoxFrameworkEnv: ####
    utils::globalVariables("RstoxFrameworkEnv")
    assign("RstoxFrameworkEnv", new.env(), parent.env(environment()))
    
    assign("Definitions", Definitions, envir=get("RstoxFrameworkEnv"))
    assign("Projects", list(), envir=get("RstoxFrameworkEnv"))
    
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
#' A list of vital definitions in RstoxFramework.
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
    Definitions <- get("RstoxFrameworkEnv")$Definitions
    if(length(name)){
        Definitions <- Definitions[[name]]
    }
    
    l <- l[names(l) %in% names(Definitions)]
    if(length(l)){
        Definitions <- utils::modifyList(Definitions, l)
    }
    
    Definitions
}




getProjectSkeletonPaths <- function() {
    
    
    StoxFolders <- getRstoxFrameworkDefinitions("StoxFolders")
    StoxDataSources <- getRstoxFrameworkDefinitions("StoxDataSources")
    StoxModelTypes <- getRstoxFrameworkDefinitions("StoxModelTypes")
    
    
    file.path()

}



createProjectSkeleton <- function(ProjectName, ProjectDirectory = NULL) {
    
    # Get the paths of the root directory and StoX skeleton:
    StoxRoot <- getRstoxFrameworkDefinitions("StoxRoot")
    StoxFolderStructure <- getRstoxFrameworkDefinitions("StoxFolderStructure")
    
    # If the ProjectDirectory is not given, set it to the default StoxRoot:
    if(length(ProjectDirectory) == 0) {
        message("Creating the default StoX root directory ", StoxRoot)
        ProjectDirectory <- StoxRoot
    }
    
    thisStoxFolderStructure <- file.path()
    
    # Create the StoxRoot folder if missing:
    if(!file.exists(StoxRoot)) {
        message("Creating the 'stox' folder in the directory ", StoxRoot)
        dir.create(
            StoxRoot, recursive = TRUE, 
            showWarnings = FALSE
        )
    }
    
    # Create the project folder structure if the "stox" folder exists:
    if(!file.exists(StoxRoot)) {
        message("Creation failed, possibly due to missing permission. Try setting the directory in which to put the stox folder, using the parameter 'ProjectDirectory'")
    }
    else{
        # The directory paths$stox already exists:
        temp <- lapply(paths, dir.create, recursive = TRUE)
    }
    
    # Return the paths:
    paths
}



CreateProject <- function(
    ProjectName, 
    ProjectDirectory = NULL, 
    Template = "EmptyTemplate"
) {
    # Create the project folder structure:
    ProjectSkeleton <- createProjectSkeleton(
        ProjectName = ProjectName, 
        ProjectDirectory = ProjectDirectory
    )
    
    # Get the tempaltes:
    Templates <- getAvaiableTemplates()
    ThisTemplate <- Templates[[Template]]
    if(length(ThisTemplate) == 0) {
        stop("The requested template does not exist. See getAvaiableTemplates() for a list of the available templates (with list.out = TRUE if you wish to see what the dirrefent templates are.)")
    }
    
    # Create an empty ProjectDescription:
    ProjectDescription <- createEmptyProjectDescription()
}


CreateEmptyBaselineProcess <- function() {
    list(
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
    )
}

GetFunctionOutputDataType <- function(FunctionName) {
    attr(get(FunctionName), "FunctionOutputDataType")
}

GetFunctionCategory <- function(FunctionName) {
    attr(get(FunctionName), "FunctionCategory")
}

GetFunctionParametersInStoX <- function(FunctionName) {
    attr(get(FunctionName), "FunctionParametersInStoX")
}

GetFunctionInputs <- function(FunctionName) {
    attr(get(FunctionName), "FunctionInputs")
}


ModifyProcessFunctionName <- function(Process, FunctionName) {
    # Set the function name:
    if(!identical(Process$FunctionName, FunctionName)) {
        Process$FunctionName <- FunctionName
        
        
        # NOTE: WE NEED DEFAULKT VALUES
        
        # Change the function parameters:
        FunctionParameters <- GetFunctionParametersInStoX(FunctionName)
        Process$FunctionParameters <- FunctionParameters
        # Change the function inputs:
        FunctionInputs <- GetFunctionInputs(FunctionName)
        Process$FunctionInputs <- FunctionInputs
    }
    
    Process
}

ModifyProcessFunctionParameters <- function(Process, FunctionParameters) {
    # Get the names of the possible funciton parameters to modify:
    PossibleFunctionParameters <- GetFunctionParametersInStoX(Process$FunctionName)
    
    # Extract the valid parameters:
    ValidFunctionParameters <- intersect(names(FunctionParameters), PossibleFunctionParameters)
    FunctionParameters <- FunctionParameters[ValidFunctionParameters]
    
    for(ind in seq_along(FunctionParameters)) {
        Process$FunctionParameters[[names(FunctionParameters[ind])]] <- FunctionParameters[[ind]]
    }
 
    Process
}




ReadAcoustic = list(
    ProcessName = "ReadAcoustic", 
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
)












ModifyBaselineProcess <- function(Process, ...) {
    lll <- list(...)
    for(ind in seq_along(lll)) {
        Process <- ModifyProcessItem(Process, names(lll[ind]), lll[[ind]])
    }
    Process
}


ModifyProcessItem <- function(Process, ItemName, ItemValue) {
    
    # Search for the Item in the top level names (such as ProcessName, FunctionName):
    numChages <- 0
    Names1 <- names(Process)
    if(ItemName %in% Names1){
        Process[[ItemName]] <- ItemValue
        numChages <- numChages + 1
    }
    else {
        # Get the items which are lists, and try to modify the subitems:
        listItmesIndices <- which(sapply(Process, is.list))
        
        # Change the item in sublists:
        for(ind in listItmesIndices){
            Names2 <- names(Process[[ind]])
            if(ItemName %in% Names2){
                Process[[ind]][[ItemName]] <- ItemValue
                numChages <- numChages + 1
            }
        }
    }
    
    # Report an error if more than ome item has been changed:
    if(numChages == 0) {
        warning("Item ", ItemName, " not found in the process with process name ", Process$ProcessName)
    }
    else if(numChages > 1) {
        stop("There are non-unique items in the process")
    }
    
    Process
}


createEmptyProjectDescription <- function() {
    # Get the model types, and populate a list with these:
    ModelTypes <- getRstoxFrameworkDefinitions("StoxModelTypes")
    ProjectDescription <- vector("list", length(ModelTypes))
    names(ProjectDescription) <- ModelTypes
    ProjectDescription
}


getAvaiableTemplates <- function(list.out = FALSE) {
    # Get the templates:
    out <- getRstoxFrameworkDefinitions("StoxTemplates")
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


GetProjectDescriptionName <- 



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
		        UseProcessData = TRUE, 
		        
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
#setStoxFunctionAttributes <- function(x, FunctionCategory, FunctionInputs, FunctionParametersInStoX, FunctionOutputDataType) {
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
    StoxModelTypes <- getRstoxFrameworkDefinitions("StoxModelTypes")
    out <- FunctionCategory %in% StoxModelTypes
    if(!out) {
        stop(paste0("FunctionCategory must be one of ", paste(StoxModelTypes, collapse = ", "), ". Was ", FunctionCategory, "."))
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
    StoxDataTypes <- getRstoxFrameworkDefinitions("StoxDataTypes")
    out <- FunctionOutputDataType %in% StoxDataTypes
    
    if(!out) {
        stop("FunctionOutputDataType must be one of the valid data types. See getRstoxFrameworkDefinitions('StoxDataTypes')")
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


##################################################
##################################################
#' Get paths to the project directories and files
#' 
#' This function gets the paths to the top level, input and output folders, and the project recipe file.
#' 
#' @param ProjectName   The directory in which to put the "stox" folder, defaulted to the "workspace" folder in the home directory.
#' @param ProjectDirectory   The directory in which to put the "stox" folder, defaulted to the "workspace" folder in the home directory.
#' 
#' @return
#' A list of paths to the "stox" folder and sub folders.
#' 
#' @examples
#' getProjectPaths()
#' 
#' @export
#' @seealso Use \code{\link{createStoxSkeleton}} to create the folders.
#' 
getProjectPaths <- function(ProjectName, ProjectDirectory = NULL) {
    
    # Small function to convert a vector to a list and use the basenames as names:
    addBasenameAsListNames <- function(x) {
        out <- as.list(x)
        names(out) <- basename(x)
        out
    }
    
    # Get the paths to StoX:
    StoxPaths <- getStoxSkeletonPaths(ProjectDirectory = ProjectDirectory)
    # Get the project path:
    projectPath <- file.path(StoxPaths$project, ProjectName)
    
    # Get project folder names:
    projectFolders <- getRstoxFrameworkDefinitions()
    
    # Define top level of the project:
    projectDirs <- file.path(
        projectPath, 
        projectFolders$StoxFolders
    )
    
    # Define input folders of the project:
    projectInputDirs <- file.path(
        projectPath, 
        projectFolders$StoxFolders["input"], 
        projectFolders$StoxDataSources
    )
    
    # Define output folders of the project:
    projectOutputDirs <- file.path(
        projectPath, 
        projectFolders$StoxFolders["output"], 
        projectFolders$StoxModelTypes
    )
    
    # Define project file:
    projectXML <- file.path(projectPath, projectFolders$projectXML)
    
    # Define list of all folder and file names of the project:
    projectDirsList <- c(
        addBasenameAsListNames(projectDirs), 
        addBasenameAsListNames(projectInputDirs), 
        addBasenameAsListNames(projectOutputDirs), 
        list(projectXML = projectXML)
    )
    
    # Return a vector of paths to the individual folders:
    list(
        projectDirs = projectDirs, 
        projectInputDirs = projectInputDirs, 
        projectOutputDirs = projectOutputDirs, 
        projectXML = projectXML, 
        projectDirsList = projectDirsList
    )
}



