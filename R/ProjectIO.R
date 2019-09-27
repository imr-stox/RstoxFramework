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