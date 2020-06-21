##################################################
##################################################
#' General parameters of RstoxFramework.
#' 
#' All functions referring to a project, a model, a process or an output table use the same parameters, listed here.
#' 
#' @param projectPath The path to the StoX project, i.e., the folder of the project with the sub folders "input", "output" and "process". Can possibly be the path to a file inside the project folder.
#' @param modelName The name of the model (one of "baseline", "analysis" and "report").
#' @param processID The ID of the process.
#' @param processName The name of the process.
#' @param tableName The name of the output table to get from the process.
#' @param template A string naming the template to use when generating the project. See \code{getAvaiableTemplates} for a list of available templates.
#' @param ow Logical: If TRUE overwrite the project.
#' @param showWarnings      Logical: If TRUE display warninigs when creting the project folders.
#' 
#' @name general_arguments
#' 
NULL


#' This function gets the paths defined by \code{\link{initiateRstoxFramework}}.
#' 
#' @inheritParams general_arguments
#' @param name A string naming the path element to get. Set this to NULL to get all paths.
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

# Function for extracting the stoxFunctionAttributes of the package, and adding the package name and full function name (packageName::functionName) to each element (function) of the list.
getStoxFunctionAttributes <- function(packageName, requestedFunctionAttributeNames = NULL) {
    
    # Get the exported object 'stoxFunctionAttributes' from the package:
    if(!identical(packageName, "RstoxFramework")) {
        stoxFunctionAttributes <- tryCatch(
            getExportedValue(packageName, "stoxFunctionAttributes"), 
            error = function(err) NULL
        )
    }
    
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
        warning("StoX: The file ", argumentDescriptionFile, " does not exist.")
    }
    
    stoxFunctionAttributes
}

# Function to add the missing attributes of all functions.
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


# Function for reading the backwardCompatibility object of a package.
getBackwardCompatibility <- function(packageName) {
    
    backwardCompatibility <- tryCatch(
        getExportedValue(packageName, "backwardCompatibility"), 
        error = function(err) NULL
    )
    
    backwardCompatibility
}

# Function for validating a StoX function library package.
validateStoxLibraryPackage <- function(packageName) {
    
    if(identical(packageName, "RstoxFramework")) {
        return(TRUE)
    }
    
    # Get the StoX function attributes:
    stoxFunctionAttributes <- getStoxFunctionAttributes(packageName)
    
    # Return FALSE if the stox funciton attributes list does not exist:
    if(length(stoxFunctionAttributes) == 0) {
        warning("StoX: The package ", packageName, " does not export the required object 'stoxFunctionAttributes'.")
        return(FALSE)
    }
    
    # Check that all of the StoX functions are exported:
    exports <- getNamespaceExports(getNamespace(packageName))
    stoxFunctionNames <- names(stoxFunctionAttributes)
    stoxFunctionNamesPresent <- stoxFunctionNames %in% exports
    if(!all(stoxFunctionNamesPresent)) {
        warning("StoX: The package ", packageName, " specifies functions in the 'stoxFunctionAttributes' object that are not exported:\n", paste(stoxFunctionNames[!stoxFunctionNamesPresent], collapse = ", "))
        return(FALSE)
    }
    
    # Check that all the exported functions have a valid JSON schema:
    processDataSchema <- readProcessDataSchema(packageName)
    processDataSchemaNames <- names(processDataSchema)
    exportedProcessDataFunctions <- names(stoxFunctionAttributes)[sapply(stoxFunctionAttributes, "[[", "functionType") == "processData"]
    exportedProcessDataFunctionsSansDefine <- sub("Define", "", exportedProcessDataFunctions)
    
    if(!all(exportedProcessDataFunctionsSansDefine %in% processDataSchemaNames)) {
        missingJSONs <- setdiff(exportedProcessDataFunctionsSansDefine, processDataSchemaNames)
        warning("StoX: The package ", packageName, " exports processData functions specified in the 'stoxFunctionAttributes' object that are not documented with a JSON schema in the processDataSchema.json file:\n", paste(missingJSONs, collapse = ", "))
        #return(FALSE)
    }
    
    # Check that if any functions have format specified, the object "processPropertyFormats" must be exported:
    if(any(sapply(stoxFunctionAttributes, function(x) length(x$functionParameterFormat) && !all(unlist(x$functionParameterFormat)  == "none")))) {
        if(!"processPropertyFormats" %in% exports) {
            warning("StoX: The package ", packageName, " does not export the required object 'processPropertyFormats'.")
            return(FALSE)
        }
    }
    
    TRUE
}

# Get the package name from the full adress to a function.
getPackageNameFromPackageFunctionName <- function(functionName) {
    sub("\\::.*", "", functionName)
}
# Get the function name from the full adress to a function.
getFunctionNameFromPackageFunctionName <- function(functionName) {
    functionName <- substring(functionName, regexpr("::", functionName) + 2)
    if(length(functionName) == 0) {
        return("")
    }
    else {
        functionName
    }
}
# Get the full adress to a function.
getPackageFunctionName <- function(functionName) {
    if(grepl("::", functionName, fixed = TRUE)) {
        return(functionName)
    }
    stoxLibrary <- getRstoxFrameworkDefinitions("stoxLibrary")
    if(functionName %in% names(stoxLibrary)) {
        stoxLibrary[[functionName]]$functionName
    }
    else {
        ""
    }
}
# Get the package name from the function name.
getPackageNameFromFunctionName <- function(functionName) {
    getPackageNameFromPackageFunctionName(getPackageFunctionName(functionName))
}


# Function to check that the functionName refers to a valid funciton, i.e., that the function is exported from a valid package (see validateStoxLibraryPackage()), and that it is represented in the associated stoxFunctionAttributes list of that package.
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








#' Get avilable templates.
#' 
#' @param list.out Logical: If TRUE return a list of the full descriptions of the templates.
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

#' Get a template.
#' 
#' @param template A string naming the template to use.
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
        warning("StoX: Invalid template name ", template, ". Available templates are ", paste0(names(templates), collapse = ", "))
    }
    
    # Define the process IDs and return the template:
    defineProcessIDs(template)
}


# Set the processIDs to a project description object.
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
#' @inheritParams general_arguments
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
#' @inheritParams general_arguments
#' @param open              Logical: If TRUE open the project after creating it.
#' @param force             Logical: If TRUE reopen (close and then open) the project if already open.
#' @param reset             Logical: If TRUE reset each model to the start of the model.
#' @param save              Logical: If TRUE save the project before closing. Default (NULL) is to ask the user whether to save the project before closing.
#' @param newProjectPath    The path to the copied StoX project.
#' @param type              The type of file to save the project to.
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
        projectMemory = thisTemplate, 
        returnProcessTable = FALSE, 
        archive = FALSE, 
        add.defaults = FALSE
    )
    # Store the changes:
    archiveProject(projectPath)
    
    # Save the project, close it, and open:
    saveProject(projectPath)
    if(!open) {
        closeProject(projectPath, save = TRUE)
    }
    
    # Return the project path project name and saved status:
    list(
        projectPath = projectPath, 
        projectName = basename(projectPath), 
        saved = isSaved(projectPath)
    )
}
#' 
#' @export
#' @rdname Projects
#' 
openProject <- function(projectPath, showWarnings = FALSE, force = FALSE, reset = FALSE, type = c("RData", "JSON")) {
    if(!force && isOpenProject(projectPath)) {
        warning("StoX: Project ", projectPath, "is already open.")
        
        # Reset the active process if requested:
        if(reset) {
            stoxModelNames <- getRstoxFrameworkDefinitions("stoxModelNames")
            lapply(stoxModelNames, function(modelName) resetModel(
                projectPath = projectPath, 
                modelName = modelName
                )
            )
        }
        
        out <- list(
            projectPath = projectPath, 
            projectName = basename(projectPath), 
            saved = isSaved(projectPath)
        )
        return(out)
    }
    #else if(force) {
        closeProject(projectPath, save = FALSE)
    #}
    
    
    projectPath <- resolveProjectPath(projectPath)
    if(length(projectPath) == 0) {
        warning("StoX: The selected projectPath is not a StoX project or a folder/file inside a StoX project.")
        return(NULL)
    }
    
    # Create the project session folder structure:
    createProjectSessionFolderStructure(projectPath, showWarnings = showWarnings)
    
    # Read the project description file:
    projectMemory <- readProjectDescription(projectPath, type = type)
    
    # Set the active process ID to 0 for all models:
    initiateActiveProcessID(projectPath)
    
    
    # Set the project memory:
    temp <- addProcesses(
        projectPath = projectPath, 
        #modelName = names(projectMemory), 
        projectMemory = projectMemory, 
        returnProcessTable = FALSE, 
        archive = FALSE, 
        add.defaults = FALSE
    )
    # Store the changes:
    archiveProject(projectPath)
    
    
    # Set the status of the projcet as saved:
    setSavedStatus(projectPath, status = TRUE)
    
    # Return the project path project name and saved status:
    list(
        projectPath = projectPath, 
        projectName = basename(projectPath), 
        saved = isSaved(projectPath)
    )
}
#' 
#' @export
#' @rdname Projects
#' 
closeProject <- function(projectPath, save = NULL) {
    # Check that the project has been saved:
    #if(!isSaved(projectPath)) {
        if(isTRUE(save)) {
            saveProject(projectPath)
        }
        else if(is.character(save)) {
            saveProject(projectPath, type = save)
        }
        else if(!isFALSE(save)) {
            answer <- readline(paste("The project", projectPath, "has not been saved. Do you with to save before closing (y/n)?"))
            if(identical(tolower(answer), "y")) {
                saveProject(projectPath)
            }
        }
    #}
    
    # Delete the project session folder structure:
    projectSessionFolderStructure <- getProjectPaths(projectPath, "projectSessionFolder")
    unlink(projectSessionFolderStructure, recursive = TRUE, force = TRUE)
}
##' 
##' @export
##' @rdname Projects
##' 
#resetProject <- function(projectPath, save = NULL) {
#    #closeProject(projectPath, save = save)
#    openProject(projectPath, showWarnings = FALSE, force = TRUE)
#}
#' 
#' @export
#' @rdname Projects
#' 
saveProject <- function(projectPath, type = c("RData", "JSON")) {
    # Get the current project description and save it to the project.RData file:
    writeProjectDescription(projectPath, type = type)
    # Set the status of the projcet as saved:
    setSavedStatus(projectPath, status = TRUE)
    
    # Return the project path project name and saved status:
    list(
        projectPath = projectPath, 
        projectName = basename(projectPath), 
        saved = isSaved(projectPath)
    )
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
    
    # Return the project path project name and saved status:
    list(
        projectPath = newProjectPath, 
        projectName = basename(newProjectPath), 
        saved = isSaved(newProjectPath)
    )
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
#' @inheritParams general_arguments
#' @param type The type of file to read.
#' 
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
        warning("StoX: Project ", projectPath, " does not exist.")
        NA
    }
}

#' 
#' @export
#' @rdname ProjectUtils
#' 
readProjectDescriptionOld <- function(projectPath, type = c("RData", "JSON")) {
    # Read the project.RData or project.xml file depending on the 'type':
    type <- match.arg(type)
    switch(
        type,
        RData = readProjectDescriptionRData(projectPath),
        XML = readProjectDescriptionXML(projectPath)
    )
}
readProjectDescriptionRDataOld <- function(projectPath) {
    # Get the path to the project description file:
    projectRDataFile <- getProjectPaths(projectPath, "projectRDataFile")
    projectDescription <- NULL
    load(projectRDataFile) # Creates/replaces the object 'projectDescription'
    
    if(length(projectDescription) == 0) {
        stop("The project description ", projectRDataFile, " is empty.")
    }
    
    # Define the process IDs and return the project description:
    defineProcessIDs(projectDescription)
}





#' Read the project description.
#' 
#' @export
#' @rdname ProjectUtils
#' 
readProjectDescription <- function(projectPath, type = c("RData", "JSON")) {
    # Read the project.RData or json file depending on the 'type':
    type <- match.arg(type)
    
    projectDescriptionFile <- getProjectPaths(projectPath, paste0("project", type, "File"))
    
    if(!file.exists(projectDescriptionFile)) {
        stop("The project description file ", projectDescriptionFile, " does not exist.")
    }    
    
    # Run the appropriate reading function:
    functionName <- paste0("readProjectDescription", type)
    do.call(functionName, list(
        projectDescriptionFile = projectDescriptionFile
    ))    
}

#readProjectXML <- function(projectXMLFile) {
#    # This is Edvins work, which will be completed later. Process data needs to be converted from JSON using JSON2processData():
#    readProject(projectXMLFile)
#}
#

readProjectDescriptionJSON <- function(projectDescriptionFile) {
    
    # Validate json object against schema
    projectValidator <- getRstoxFrameworkDefinitions("projectValidator")
    valid <- projectValidator(projectDescriptionFile)
    if(!isTRUE(valid)) {
        cat("Output from project.json validator:\n")
        print(projectValidator(projectDescriptionFile, verbose = TRUE))
        stop("The file ", projectDescriptionFile, " is not a valid project.json file.")
    }
    
    # Read project.json file to R list:
    json <- jsonlite::read_json(projectDescriptionFile)
    
    # Get the model names:
    #modelNames <- sapply(json, "[[", "modelName")
    # Remove the model names from each model:
    #projectDescription <- lapply(json, removeNamedElement, name = "modelName")
    # Add the modelNames as names to the list of models:
    #names(projectDescription) <- modelNames
    
    # Move the processes one level up in each model:
    #projectDescription <- lapply(projectDescription, unlist, recursive = FALSE)
    
    projectDescription <- json$project$models
    
    # Introduce process IDs: 
    projectDescription <- defineProcessIDs(projectDescription)
    
    # Convert geojson to spatial object:
    projectDescription <- convertGeojsonToProcessData(projectDescription)
    
    return(projectDescription)
}

removeNamedElement <- function(list, name) {
    list[!names(list) %in% name]
}


convertGeojsonToProcessData <- function(projectDescription) {
    # Run through the processes and convert SpatialPolygonsDataFrame to geojson string:
    for(modelName in names(projectDescription)) {
        for(processIndex in seq_along(projectDescription [[modelName]])) {
            for(processDataIndex in names(projectDescription [[modelName]] [[processIndex]]$processData)) {
                this <- projectDescription [[modelName]] [[processIndex]]$processData[[processDataIndex]]
                #if(is.character(this) && grepl("FeatureCollection", substring(this, 1, 100))) {
                    #projectDescription [[modelName]] [[processIndex]]$processData[[processDataIndex]] <- geojsonio::geojson_sp(this)
                if(is.list(this) && "features" %in% tolower(names(this))) {
                    projectDescription [[modelName]] [[processIndex]]$processData[[processDataIndex]] <- geojsonio::geojson_sp(jsonlite::toJSON(projectDescription [[modelName]] [[processIndex]]$processData[[processDataIndex]], pretty = TRUE, auto_unbox = TRUE))
                }
            }
        }
    }
    
    return(projectDescription)
}



readProjectDescriptionRData <- function(projectDescriptionFile) {
    # Creates/replaces the object 'projectDescription':
    projectDescription <- NULL
    load(projectDescriptionFile)
    
    return(projectDescription)
}



# Convert a process data object to JSON string.
processData2JSON <- function(processData, digits = getRstoxFrameworkDefinitions("digits")$JSON) {
    if("StratumPolygon" %in% names(processData)) {
        #as.character(geojsonio::geojson_json(processData))
        as.character(geojsonio::geojson_json(processData, lon = "x", lat = "y"))
    }
    else {
        #jsonlite::toJSON(processData, digits = digits, pretty = TRUE, auto_unbox = TRUE)
        processData
    }
    
}

# Convert JSON string to a process data object.
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



#' Write the project description.
#' 
#' @export
#' @rdname ProjectUtils
#' 
writeProjectDescription <- function(projectPath, type = c("RData", "JSON")) {
    # Read the project.RData or project.xml file depending on the 'type':
    type <- match.arg(type)
    
    projectSessionFolder <- getProjectPaths(projectPath, "projectSessionFolder")
    if(!file.exists(projectSessionFolder)) {
        stop("The project memory folder ", projectSessionFolder, " does not exist. Project ", projectPath, " cannot be saved.")
    }
    
    # Get full project description:
    projectDescription <- getProjectMemoryData(projectPath, named.list = TRUE)
    # Get the file to write it to:
    projectDescriptionFile <- getProjectPaths(projectPath, paste0("project", type, "File"))
    
    # Run the appropriate saveing function:
    functionName <- paste0("writeProjectDescription", type)
    do.call(functionName, list(
        projectDescription = projectDescription, 
        projectDescriptionFile = projectDescriptionFile
    ))
}
writeProjectDescriptionRData <- function(projectDescription, projectDescriptionFile) {
    # Get the path to the project description file, and save the current project description:
    save(projectDescription, file = projectDescriptionFile)
}
writeProjectDescriptionXML <- function(projectDescription, projectDescriptionFile) {
    writeProjectXML(projectDescription, projectDescriptionFile)
}

##################################################
##################################################
#' Write project description to a file in json format
#' 
#' This function writes project description to json file.
#' 
#' @param projectDescription  a list of lists with project description.
#' @param projectDescriptionFile  a file name.
#' 
#' 
#'
#'
writeProjectDescriptionJSON <- function(projectDescription, projectDescriptionFile) {
    
    # Convert spatial to geojson string, and write to temporary files for modifying the project.json to have geojson instead of geojson string: 
    projectDescription <- convertProcessDataToGeojson(projectDescription)
    
    # Unname the models (removing procecssIDs):
    projectDescription <- lapply(projectDescription, unname)
    
    # Add attributes and wrap the models into an object:
    projectDescription <- list(
        project = list(
            Template = "", 
            RstoxFrameworkVersion = as.character(utils::packageVersion("RstoxFramework")),
            TimeSaved = strftime(as.POSIXlt(Sys.time(), "UTC", "%Y-%m-%dT%H:%M:%S") , "%Y-%m-%dT%H:%M:%S%z"), 
            RVersion = R.version.string, 
            models = projectDescription
        )
    )
    
    # Convert project description to json structure: 
    json <- jsonlite::toJSON(projectDescription)
    # Read any geojson objects stored in temporary file by convertProcessDataToGeojson():
    json <- replaceSpatialFileReference(json)
    
    # Fix pretty formatting by reading in and writing back the file:
    write(json, projectDescriptionFile)
    json <- jsonlite::toJSON(
        jsonlite::read_json(projectDescriptionFile), 
        pretty = TRUE, 
        auto_unbox = TRUE
    )
    
    # Validate the json structure with json schema
    projectValidator <- getRstoxFrameworkDefinitions("projectValidator")
    valid <- projectValidator(json)
    if(!isTRUE(valid)) {
        cat("Output from project.json validator:\n")
        print(projectValidator(json, verbose = TRUE))
        stop("Cannot write the project.json file. It is not a valid project.json file.")
    }
    #jsonvalidate::json_validate(json)
    
    # 5. Write the validated json to file: 
    write(json, projectDescriptionFile) 
}

convertProcessDataToGeojson <- function(projectDescription) {
    # Run through the processes and convert SpatialPolygonsDataFrame to geojson string:
    for(modelName in names(projectDescription)) {
        for(processIndex in seq_along(projectDescription [[modelName]])) {
            for(processDataIndex in names(projectDescription [[modelName]] [[processIndex]]$processData)) {
                this <- projectDescription [[modelName]] [[processIndex]]$processData[[processDataIndex]]
                if("SpatialPolygonsDataFrame" %in% class(this)) {
                    #projectDescription [[modelName]] [[processIndex]]$processData[[processDataIndex]] <- geojsonio::geojson_json(this)
                    projectDescription [[modelName]] [[processIndex]]$processData[[processDataIndex]] <- buildSpatialFileReferenceString(this)
                }
            }
        }
    }
    
    return(projectDescription)
}


replaceSpatialFileReference <- function(x) {
    spatialFileReferenceCodeStart <- getRstoxFrameworkDefinitions("spatialFileReferenceCodeStart")
    spatialFileReferenceCodeEnd <- getRstoxFrameworkDefinitions("spatialFileReferenceCodeEnd")
    start <- unlist(gregexpr(spatialFileReferenceCodeStart, x))
    
    # Return unaltered if no hits:
    if(length(start) == 1 && start == -1) {
        return(x)
    }
    
    end <- unlist(gregexpr(spatialFileReferenceCodeEnd, x))
    spatialFile <- mapply(substr, x, start + nchar(spatialFileReferenceCodeStart), end - 1)
    spatialString <- sapply(spatialFile, readCharAll)
    
    # Replace in reverse order to avoid messing up the start and end indices:
    for(ind in rev(seq_along(spatialString))) {
        x <- paste0(
            substr(x, 1, start[ind] - 2), 
            spatialString[ind], 
            substring(x, end[ind] + nchar(spatialFileReferenceCodeEnd) + 1)
        )
    }
    
    return(x)
}

readCharAll <- function(spatialFile) {
    readChar(spatialFile, file.info(spatialFile)$size)
}





buildSpatialFileReferenceString <- function(x) {
    filePath <- tempfile()
    write(geojsonio::geojson_json(x), file = filePath)
    SpatialFileReferenceString <- paste0(
        getRstoxFrameworkDefinitions("spatialFileReferenceCodeStart"), 
        filePath, 
        getRstoxFrameworkDefinitions("spatialFileReferenceCodeEnd")
    )
    return(SpatialFileReferenceString)
}


#' Initiate the actige processID.
#' 
#' @rdname ProjectUtils
#' 
initiateActiveProcessID <- function(projectPath) {
    # Read the active process ID for the model:
    activeProcessIDFile <- getProjectPaths(projectPath, "activeProcessIDFile")
    # Initiate with all zeros:
    #activeProcessIDTable <- data.table::as.data.table(matrix(NA, nrow = 1, ncol = 3))
    activeProcessIDTable <- data.table::data.table(
        modelName = getRstoxFrameworkDefinitions("stoxModelNames"), 
        processID = as.character(NA), 
        processDirty = NA, 
        propertyDirty = NA 
    )
    #colnames(activeProcessIDTable) <- getRstoxFrameworkDefinitions("stoxModelNames")
    data.table::fwrite(activeProcessIDTable, activeProcessIDFile, sep = "\t", na = "NA")
    activeProcessIDFile
}

#' Check or set if a project is running or not.
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



# Set the saved status of the project.
setSavedStatus <- function(projectPath, status) {
    # Get the path to the projectSavedStatusFile:
    projectSavedStatusFile <- getProjectPaths(projectPath, "projectSavedStatusFile")
    # Write the status to the file:
    writeLines(as.character(status), projectSavedStatusFile)
}


# Get the project path possibly from a path in side the project.
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

#' Get the active process.
#' 
#' @inheritParams general_arguments
#'
#' @export
#'
getActiveProcess <- function(projectPath, modelName = NULL) {
    # Read the active process ID for the model:
    activeProcessIDFile <- getProjectPaths(projectPath, "activeProcessIDFile")
    if(!file.exists(activeProcessIDFile)) {
        warning("StoX: The active process ID file has not been initiated.")
    }
    activeProcessIDTable <- data.table::fread(activeProcessIDFile, sep = "\t")
    
    if(length(modelName)) {
        #return(activeProcessIDTable[[modelName]])
        thisModelName <- modelName
        output <- activeProcessIDTable[modelName == thisModelName, ]
        output <- as.list(output[, modelName := NULL])
        return(output[])
        #return(activeProcessIDTable[modelName == modelName, ])
    }
    else {
        return(activeProcessIDTable)
    }
    
}

writeActiveProcessID <- function(projectPath, modelName, activeProcessID = NULL, processDirty = NULL, propertyDirty = NULL) {
    
    # Read the active process ID for the model:
    activeProcessIDTable <- getActiveProcess(projectPath, modelName = NULL)
    # Probably unelegant trick to avoid using the same name as the variable to modify:
    thisModelName <- modelName
    # Make sure the active processID is character prior to modification:
    if(!is.character(activeProcessIDTable$processID)) {
        activeProcessIDTable[, processID := as.character(processID)]
    }
    
    # Set the active process ID:
    if(length(activeProcessID)) {
        activeProcessIDTable[modelName == thisModelName, processID := ..activeProcessID]
    }
    # Set the processDirty status:
    if(length(processDirty)) {
        activeProcessIDTable[modelName == thisModelName, processDirty := ..processDirty]
    }
    # Set the propertyDirty status:
    if(length(propertyDirty)) {
        activeProcessIDTable[modelName == thisModelName, propertyDirty := ..propertyDirty]
    }
    
    # Write and return the activeProcessIDTable:
    activeProcessIDFile <- getProjectPaths(projectPath, "activeProcessIDFile")
    data.table::fwrite(activeProcessIDTable, activeProcessIDFile, sep = "\t", na = "NA")
    
    return(activeProcessIDFile)
}
writeActiveProcessIDFromTable <- function(projectPath, activeProcessIDTable) {
    # Read the active process ID for the model:
    activeProcessIDFile <- getProjectPaths(projectPath, "activeProcessIDFile")
    if(!file.exists(activeProcessIDFile)) {
        warning("StoX: The active process ID file has not been initiated.")
    }
    data.table::fwrite(activeProcessIDTable, activeProcessIDFile, sep = "\t", na = "NA")
}


#' Reset a StoX model.
#' 
#' @inheritParams general_arguments
#' @param processDirty Logical: Indicates whether the model has been modified when reseting.
#' @inheritParams unReDoProject
#' 
#' @export
#'
resetModel <- function(projectPath, modelName, processID = NULL, processDirty = FALSE, shift = 0, returnProcessTable = FALSE) {
    
    # Get the process ID to reset the model to:
    processIndexTable <- readProcessIndexTable(projectPath, modelName)
    
    # Get the processIndex, that is the index of the process to reset to:
    if(length(processID) == 0 || is.na(processID)) {
        processIndex <- 0
    }
    else {
        processIndex <- which(processIndexTable$processID == processID) + shift
        # Error if the process does not exist
        if(length(processIndex) == 0) {
            stop("processID not regocnized")
        }
    }
    
    # Read the active proces ID and reset if that is not NA:
    currentActiveProcessID <- getActiveProcess(projectPath = projectPath, modelName = modelName)$processID
    
    # If activevProcecssID is NA, do nothing, as this indicates that the model has not been run:
    if(!is.na(currentActiveProcessID)) {
        
        ##### (1) Set active process: #####
        # Get the current active process index:
        currentActiveProcessIndex <- which(processIndexTable$processID == currentActiveProcessID)
        
        # If the processIndex is 0 or processID not given, reset to the start of the model (activeProcessID = NA):
        if(processIndex == 0 || length(processID) == 0) {
            newActiveProcessID <- NA
        }
        else {
            # Reset only if the input process ID is before that the active:
            if(processIndex < currentActiveProcessIndex) {
                    newActiveProcessID <- processIndexTable$processID[processIndex]
                }
            else {
                newActiveProcessID <- currentActiveProcessID
            }
        }
        
        # Write the active process ID:
        #if(is.na(newActiveProcessID) || newActiveProcessID != currentActiveProcessID) {
            writeActiveProcessID(projectPath, modelName, newActiveProcessID, processDirty = processDirty)
        #}
        
        ##### (2) Delete process output of the processes from the new active process: #####
        if(currentActiveProcessIndex > processIndex) {
            # Get all processes from the process to reset to and on:
            allProcessIndex <- getProcessIndexFromProcessID(
                projectPath = projectPath, 
                modelName = modelName, 
                processIndexTable$processID
            )
            processIDsToDelete <- processIndexTable$processID[allProcessIndex > processIndex]
            foldersToDelete <- sapply(
                processIDsToDelete, 
                function(thisProcessID) getProcessOutputFolder(
                    projectPath = projectPath, 
                    modelName = modelName, 
                    processID = thisProcessID, 
                    type = "memory"
                )
            )
            unlink(foldersToDelete, recursive = TRUE, force = TRUE)
            
            ##### (3) Delete process output text files: #####
            # Get the list of output text files:
            folderPath <- getProjectPaths(projectPath = projectPath, name = modelName)
            outputTextFiles <- list.files(folderPath, full.names = TRUE)
            
            # Identify the output file with prefix later than the index of the process to reset to:
            prefix <- as.numeric(sub("\\_.*", "", basename(outputTextFiles)))
            filesToDelete <- outputTextFiles[prefix > processIndex]
            
            # Delete the files:
            unlink(filesToDelete, recursive = TRUE, force = TRUE)
        }
    }
    
    # Return a list of the active process and the process table:
    output <- list(
        activeProcess = getActiveProcess(projectPath = projectPath, modelName = modelName)
    )
    if(returnProcessTable) {
        output <- c(
            list(processTable = getProcessTable(projectPath = projectPath, modelName = modelName)), 
            output
        )
    }
   
    #output <- list(
    #    if(returnProcessTable) processTable = getProcessTable(projectPath = projectPath, modelName = modelName), 
    #    activeProcess = getActiveProcess(projectPath = projectPath, modelName = modelName)
    #)
    return(output)
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


# Unused.
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


# Function for getting the file path of one specific process argument file.
getNewArgumentFileSansExt <- function(projectPath, modelName, processID, argumentName) {
    
    # Get the folder holding the project descriptions:
    memoryModelsFolder <- getProjectPaths(projectPath, "memoryModelsFolder")
    argumentFolder <- file.path(memoryModelsFolder, modelName, processID, argumentName)
    
    addTimeToFileName(
        fileName = argumentName, 
        dir = argumentFolder
    )
    
    ## Define a string with time in ISO 8601 format:
    #timeString <- format(Sys.time(), tz = "UTC", format = "%Y%m%dT%H%M%OS3Z")
    ## Define the file name including the time string, and build the path to the file:
    ##fileName <- paste0(argumentName, "_", timeString, ".rds")
    #fileName <- paste0(argumentName, "_", timeString)
    #filePath <- file.path(argumentFolder, fileName)
    #filePath
}


# Function for getting the file path of a new project memory file.
getNewProjectMemoryFileSansExt <- function(projectPath) {
    # Get the folder holding the project descriptions:
    memoryFolder <- getProjectPaths(projectPath, "memoryHistoryFolder")
    
    addTimeToFileName(
        fileName = "projectMemory", 
        dir = memoryFolder
    )
    #
    ## Define a string with time in ISO 8601 format:
    #timeString <- format(Sys.time(), tz = "UTC", format = "%Y%m%dT%H%M%OS3Z")
    ## Define the file name including the time string, and build the path to the file:
    ##fileName <- paste0("projectMemory", "_", timeString, ".rds")
    #fileName <- paste0("projectMemory", "_", timeString)
    #filePath <- file.path(memoryFolder, fileName)
    #filePath
}




addTimeToFileName <- function(fileName, dir) {
    # Define a string with time in ISO 8601 format:
    timeString <- format(Sys.time(), tz = "UTC", format = "%Y%m%dT%H%M%OS3Z")
    # Define the file name including the time string, and build the path to the file:
    fileName <- paste0(fileName, "_", timeString)
    filePath <- file.path(dir, fileName)
    
    return(filePath)
}


# Function for saving an argument value to one process argument file:
saveArgumentFile <- function(projectPath, modelName, processID, argumentName, argumentValue, ext = "rds") {
    
    # Get the path to the new argument file:
    argumentFileSansExt <- getNewArgumentFileSansExt(projectPath, modelName, processID, argumentName)
    
    # Save the argument to the file, and return the file path:
    argumentFilePath <- writeMemoryFile(
        argumentValue, 
        filePathSansExt = argumentFileSansExt, 
        ext = ext
    )
    
    # Return the file path relative to the project path:
    relativePath <- sub(projectPath, "", argumentFilePath)
    
    return(relativePath)
}





# Unused.
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

# Unused.
removeFromArgumentFileTable <- function(argumentFileTable, modelName, processID) {
    
    # Get the rows in the argument file table to remove, which are those with processID as that specified by the user to remove:
    toRemove <- argumentFileTable$processID == processID & argumentFileTable$modelName == modelName
    
    # Remove the argument files of the process:
    if(any(toRemove)) {
        argumentFileTable <- argumentFileTable[!toRemove, ]
    }
    else {
        warning("StoX: The process with processID ", processID, " was not found in the current state of the model")
    }
    
    # Return the argument file table:
    argumentFileTable
}


deleteProjectMemoryFile <- function(projectPath, projectMemoryFileRelativePath) {
    unlink(file.path(projectPath, projectMemoryFileRelativePath))
}








# Split a projectMemoryData object to a list of the elements modelName, processID, argumentName, argumentValue.
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



# Function to read the projectDescriptionIndexFile:
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

# Function to write the projectDescriptionIndexFile:
writeProjectMemoryIndex <- function(projectPath, projectMemoryIndex) {
    # Read the projectDescriptionIndexFile:
    projectMemoryIndexFile <- getProjectPaths(projectPath, "projectMemoryIndexFile")
    data.table::fwrite(projectMemoryIndex, file =  projectMemoryIndexFile, sep = "\t")
}



#' Function to undo or redo, i.e., reset the current project description file and change the indices. There will be separate GUI functions for undo and redo:
#' 
#' @inheritParams general_arguments
#' @param shift The position relative to the current memory status to un/redo to.
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
    #file.copy(
    #    from = fileWithNewCurrentProjectMemory, 
    #    to = getProjectPaths(projectPath, "currentProjectMemoryFile"), 
    #    overwrite = TRUE, 
    #    copy.date = TRUE
    #)
    
    # Rewrite the text file holding processIndexTable, activeProcessIDTable and maxProcessIntegerIDTable:
    unwrapProjectMemoryFile(fileWithNewCurrentProjectMemory)
}

# Function to unwrap a project memory history file to multiple individual files
unwrapProjectMemoryFile <- function(projectMemoryFile) {
    # Read the project memory to get the data to write to the text files:
    #projectMemory <- readRDS(projectMemoryFile)
    projectMemory <- readMemoryFile(
        projectMemoryFile
    )
    
    # Unwrap and overwrite the process index table file:
    writeProcessIndexTable(projectPath, projectMemory$processIndexTable)
    
    # Unwrap and overwrite the active process ID file:
    writeActiveProcessIDFromTable(projectPath, projectMemory$activeProcessIDTable)
    
    # Unwrap and overwrite the maximum process integer ID file:
    writeMaxProcessIntegerIDTable(projectPath, projectMemory$maxProcessIntegerIDTable)
    
    stop("Here we need to code replacing the memory files!!!!!!!!!!!!!")
}






##################################
##### StoX function library: #####
##################################

#getStoxFunctionAttributes <- function(packageName) {
#    package <- paste0("package", packageName)
#    get("stoxFunctionAttributes", pos = package)
#}

#' Function returning the names of the StoX functions available for a model:
#' 
#' @inheritParams general_arguments
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
    
    # If empty function name, return empty list:
    if(length(functionName) == 0 || nchar(functionName) == 0) {
        if(length(metaDataName) == 0) {
            return(list())
        }
        else {
            return(NULL)
        }
    }
    
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
            warning("StoX: The requested meta data ", metaDataName, " is not included in the stoxFunctionAttributes for function ", functionName, ".")
        }
        NULL
    }
}

# Function to return the names of the arguments to show for a function:
getArgumentsToShow <- function(projectPath, modelName, processID, argumentFilePaths = NULL) {
    
    # Get the function name and arguments:
    functionName <- getFunctionName(projectPath = projectPath, modelName = modelName, processID = processID, argumentFilePaths = argumentFilePaths)
    functionInputs <- getFunctionInputs(projectPath = projectPath, modelName = modelName, processID = processID, argumentFilePaths = argumentFilePaths)
    functionParameters <- getFunctionParameters(projectPath = projectPath, modelName = modelName, processID = processID, argumentFilePaths = argumentFilePaths)
    functionArguments <- c(functionInputs, functionParameters)
    
    # Get the function argument hierarchy:
    functionArgumentHierarchy <- getStoxFunctionMetaData(functionName, "functionArgumentHierarchy", showWarnings = FALSE)
    
    # Loop through the arguments given by parent tags in the functionArgumentHierarchy, and set toShow to FALSE if not any of the criterias are fulfilled:
    toShow <- logical(length(functionArguments))
    names(toShow) <- names(functionArguments)
    for(argumentName in names(toShow)) {
        # Check whether the argument is given in the functionArgumentHierarchy. If not, it will be shown:
        atArgumentName <- which(argumentName == names(functionArgumentHierarchy))
        if(length(atArgumentName)) {
            # Loop through the occurrences of the argumentName in the functionArgumentHierarchy, applying &&:
            hitsOr <- logical(length(atArgumentName))
            for(ind in seq_along(atArgumentName)) {
                # Loop through the conditions and set hitsAnd to TRUE if at least one condition is fullfilled:
                conditionNames <- names(functionArgumentHierarchy[[atArgumentName[ind]]])
                hitsAnd <- logical(length(conditionNames))
                names(hitsAnd) <- conditionNames
                for(conditionName in conditionNames) {
                    # Added requirement that functionArguments[[conditionName]] has positie length:
                    if(length(functionArguments[[conditionName]]) && functionArguments[[conditionName]] %in% functionArgumentHierarchy[[atArgumentName[ind]]][[conditionName]]) {
                        hitsAnd[conditionName] <- TRUE
                    }
                }
                # Apply the AND condition, implying that hitsAnd is TRUE if all are TRUE:
                hitsOr[ind] <- all(hitsAnd)
            }
            toShow[[argumentName]] <- any(hitsOr)
        }
        else {
            toShow[[argumentName]] <- TRUE
        }
    }
    
    # Return only the names of the arguments to show:
    return(names(toShow)[toShow])
}

# Is the function a process data function?
isProcessDataFunction <- function(functionName) {
    # Get the function output data type and match against the defined process data types:
    #functionOutputDataType <- getStoxFunctionMetaData(functionName, "functionOutputDataType")
    #functionOutputDataType %in% getRstoxFrameworkDefinitions("stoxProcessDataTypes")
    identical(getStoxFunctionMetaData(functionName, "functionType"), "processData")
}

# Is the function a process data function?
isBootstrapFunction <- function(functionName) {
    # Get the function output data type and match against the defined process data types:
    #functionOutputDataType <- getStoxFunctionMetaData(functionName, "functionOutputDataType")
    #functionOutputDataType %in% getRstoxFrameworkDefinitions("stoxProcessDataTypes")
    identical(getStoxFunctionMetaData(functionName, "functionType"), "bootstrap")
}



# Function which gets the values defined for the parameters in the definition of a function:
getStoxFunctionParameterPossibleValues <- function(functionName, fill.logical = TRUE) {
    
    # Get the available functions:
    availableFunctions <- getRstoxFrameworkDefinitions("availableFunctions")
    
    functionName <- getFunctionNameFromPackageFunctionName(functionName)
    if(! functionName %in% availableFunctions) {
        warning("StoX: The function ", functionName, "is not an official StoX function.")
        return(list())
    }
    
    # Get all possible values:
    output <- getRstoxFrameworkDefinitions("availableFunctionPossibleValues")[[functionName]]
    
    # Insert c(FALSE, TRUE) for logicals:
    if(fill.logical) {
        areLogicals <- sapply(output, is.logical)
        if(sum(areLogicals)) {
            output[areLogicals] <- lapply(output[areLogicals], expandLogical)
        }
    }
    
    return(output)
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
getFunctionName <- function(projectPath, modelName, processID, argumentFilePaths = NULL) {
    getProjectMemoryData(
        projectPath, 
        modelName = modelName, 
        processID = processID, 
        argumentName = "functionName", 
        drop1 = TRUE, 
        argumentFilePaths = argumentFilePaths
    )
}

getFunctionInputs <- function(projectPath, modelName, processID, only.valid = FALSE, argumentFilePaths = NULL) {
    functionInputs <- getProjectMemoryData(
        projectPath, 
        modelName = modelName, 
        processID = processID, 
        argumentName = "functionInputs", 
        drop1 = TRUE, 
        argumentFilePaths = argumentFilePaths
    )
    
    if(only.valid) {
        argumentsToShow <- getArgumentsToShow(projectPath, modelName = modelName, processID = processID, argumentFilePaths = argumentFilePaths)
        functionInputs <- functionInputs[intersect(names(functionInputs), argumentsToShow)]
    }
    
    return(functionInputs)
}

getFunctionParameters <- function(projectPath, modelName, processID, only.valid = FALSE, argumentFilePaths = NULL) {
    functionParameters <- getProjectMemoryData(
        projectPath, 
        modelName = modelName, 
        processID = processID, 
        argumentName = "functionParameters", 
        drop1 = TRUE, 
        argumentFilePaths = argumentFilePaths
    )
    
    if(only.valid) {
        argumentsToShow <- getArgumentsToShow(projectPath, modelName = modelName, processID = processID, argumentFilePaths = argumentFilePaths)
        functionParameters <- functionParameters[intersect(names(functionParameters), argumentsToShow)]
    }
    
    return(functionParameters)
}

getProcessName <- function(projectPath, modelName, processID, argumentFilePaths = NULL) {
    getProjectMemoryData(
        projectPath, 
        modelName = modelName, 
        processID = processID, 
        argumentName = "processName", 
        drop1 = TRUE, 
        argumentFilePaths = argumentFilePaths
    )
}

getProcessParameters <- function(projectPath, modelName, processID, argumentFilePaths = NULL) {
    getProjectMemoryData(
        projectPath, 
        modelName = modelName, 
        processID = processID, 
        argumentName = "processParameters", 
        drop1 = TRUE, 
        argumentFilePaths = argumentFilePaths
    )
}

# This function gets the process data as stored in the process memory files. These process data may diffre from the process data output from the process, stored in the output data files, particularly if interactive functions have been used. In this case, the process must be run again with UseProcessData = TRUE (automatically set by RstoxFramework) to update the process output, which is used in runProcess() using getProcessOutput(). 
getProcessData <- function(projectPath, modelName, processID, argumentFilePaths = NULL) {
    getProjectMemoryData(
        projectPath, 
        modelName = modelName, 
        processID = processID, 
        argumentName = "processData", 
        drop1 = TRUE, 
        argumentFilePaths = argumentFilePaths
    )
}

getProcess <- function(projectPath, modelName, processID, argumentFilePaths = NULL) {
    getProjectMemoryData(
        projectPath, 
        modelName = modelName, 
        processID = processID, 
        drop1 = TRUE, 
        argumentFilePaths = argumentFilePaths
    )
}

getDataType <- function(projectPath, modelName, processID, argumentFilePaths = NULL) {
    # Get the function name:
    functionName <- getFunctionName(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID, 
        argumentFilePaths = argumentFilePaths
    )
    # Get the data type from the function name:
    functionOutputDataType <- getStoxFunctionMetaData(functionName, "functionOutputDataType")
    if(length(functionOutputDataType) == 0) {
        functionOutputDataType <- ""
    }
    
    return(functionOutputDataType)
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
    processIndex <- seq_len(nrow(processIndexTable))
    
    # Add the process ID and as the process after 'afterProcessID':
    toRearrange <- match(paste(modelName, processID), paste(processIndexTable$modelName, processIndexTable$processID))
    notToRearrange <- setdiff(processIndex, toRearrange)
    rearranged <- processIndexTable[toRearrange, ]
    rest <- processIndexTable[notToRearrange, ]
    
    afterProcessIndexInRest <- max(0, which(rest$modelName %in% modelName & rest$processID %in% afterProcessID))
    
    before <- rest[seq_len(afterProcessIndexInRest), ]
    if(afterProcessIndexInRest < nrow(rest)) {
        after <- rest[seq(afterProcessIndexInRest + 1, nrow(rest)), ]
    }
    else {
        after <- NULL
    }
    
    # Build the new processIndexTable:
    newProcessIndexTable <- rbind(
        before, 
        rearranged, 
        after
    )
    
    # Was there any change?:
    changed <- which(processIndexTable$processID != newProcessIndexTable$processID)
    
    # Write the file:
    if(any(changed)) {
        writeProcessIndexTable(projectPath = projectPath, processIndexTable = newProcessIndexTable)
        
        # Set the activev process index as the first changed process minus 1:
        activeProcessIndex <- min(changed) - 1
        
        # If reset to the start, return NA:
        if(activeProcessIndex == 0) {
            activeProcessID <- NA
        }
        else {
            activeProcessID <- processIndexTable$processID[activeProcessIndex]
        }
        
        return(activeProcessID)
    }
    else {
        return(NULL)
    }
    
}


modifyProcessNameInProcessIndexTable <- function(projectPath, modelName, processName, newProcessName) {
    # Get the process index file:
    processIndexTable <- readProcessIndexTable(projectPath = projectPath)
    
    # Modify the name of the process:
    toRename <- which(processIndexTable$modelName %in% modelName & processIndexTable$processName %in% processName)
    processIndexTable[toRename, processName := ..newProcessName]
    
    # Write the file:
    writeProcessIndexTable(projectPath = projectPath, processIndexTable = processIndexTable)
}



modifyProcessNameInFunctionInputs <- function(projectPath, modelName, processName, newProcessName) {
    # Get the process index file:
    processTable <- getProcessesSansProcessData(
        projectPath = projectPath, 
        modelName = modelName
    )
    nProcesses <- nrow(processTable)
    atProcess <- which(processTable$processName == newProcessName)
    
    # Modify the process name in the function inputs:
    if(atProcess < nProcesses) {
        for(index in seq(atProcess + 1, nProcesses)) {
            atProcessName <- unlist(processTable$functionInputs[[index]]) == processName
            if(any(atProcessName)) {
                # Define the list of function inputs to modify:
                dataType <- names(processTable$functionInputs[[index]][atProcessName])
                insertList <- list(
                    newProcessName
                )
                names(insertList) <- dataType
                
                # Modify the function inputs to the new process name:
                modifyFunctionInputs(
                    projectPath = projectPath, 
                    modelName = modelName, 
                    processID = processTable$processID[[index]], 
                    newFunctionInputs = insertList
                )
                
            }
        }
    }
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



##### Process table: #####
#' Functions to get the process table of a model.
#'
#' @inheritParams general_arguments
#' 
#' @export
#' 
getProcessTable <- function(projectPath, modelName, afterProcessID = NULL, beforeProcessID = NULL, argumentFilePaths = NULL) {
    
    # Read the memory file paths once, and insert to the get* functions below to speed things up:
    if(length(argumentFilePaths) == 0) {
        argumentFilePaths <- getArgumentFilePaths(projectPath)
    }
    
    # Get a table of all the processes including function inputs, parameters and input errors:
    processTable <- scanForModelError(
        projectPath = projectPath, 
        modelName = modelName, 
        afterProcessID = afterProcessID, 
        beforeProcessID = beforeProcessID, 
        argumentFilePaths = argumentFilePaths
    )
    # Return an empty data.table if the processTable is empty:
    if(nrow(processTable) == 0) {
        return(data.table::data.table())
    }
    
    # Check whether the data type can be shown in the map:
    processTable[, canShowInMap := getCanShowInMap(dataType = functionOutputDataType)]
    
    # Check whether the process returns process data:
    processTable[, hasProcessData := lapply(functionName, isProcessDataFunction)]
    
    # Add hasBeenRun:
    activeProcess <- getActiveProcess(projectPath = projectPath, modelName = modelName)
    processTable[, hasBeenRun := FALSE]
    if(!is.na(activeProcess$processID)) {
        activeProcessIndex <- getProcessIndexFromProcessID(
            projectPath = projectPath, 
            modelName = modelName, 
            processID = activeProcess$processID
        )
        processTable[seq_len(min(activeProcessIndex, nrow(processTable))), hasBeenRun := TRUE]
    }
    
    return(processTable[])
}
#' 
#' @export
#' @rdname getProcessTable
#' 
scanForModelError <- function(projectPath, modelName, afterProcessID = NULL, beforeProcessID = NULL, argumentFilePaths = NULL) {
    
    # Read the memory file paths once, and insert to the get* functions below to speed things up:
    if(length(argumentFilePaths) == 0) {
        argumentFilePaths <- getArgumentFilePaths(projectPath)
    }
    
    # Get the processes:
    processTable <- getProcessesSansProcessData(
        projectPath = projectPath, 
        modelName = modelName, 
        afterProcessID = afterProcessID, 
        beforeProcessID = beforeProcessID, 
        argumentFilePaths = argumentFilePaths
    )
    # Return an empty data.table if the processTable is empty:
    if(nrow(processTable) == 0) {
        return(data.table::data.table())
    }
    
    # Add output data type:
    processTable$functionOutputDataType <- mapply(
        getDataType, 
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processTable$processID, 
        MoreArgs = list(argumentFilePaths = argumentFilePaths), 
        SIMPLIFY = TRUE
    )
    
    # Add a column logging function input errors:
    processTable[, functionInputError := FALSE]
    # Run through the processes and detect model errors:
    for(processIndex in seq_len(nrow(processTable))) {
        if(length(processTable$functionInputs[[processIndex]])) {
            functionInputError <- checkFunctionInputs(processTable[seq_len(processIndex), ])
        }
        else {
            functionInputError <- FALSE
        }
        # Do any of the funciton inputs have error?
        processTable$functionInputError[processIndex] <- any(functionInputError)
    }
    
    return(processTable)
}
#' 
#' @export
#' @rdname getProcessTable
#' 
getProcessesSansProcessData <- function(projectPath, modelName, afterProcessID = NULL, beforeProcessID = NULL, argumentFilePaths = NULL) {
    
    # Read the memory file paths once, and insert to the get* functions below to speed things up:
    if(length(argumentFilePaths) == 0) {
        argumentFilePaths <- getArgumentFilePaths(projectPath)
    }
    
    # Get the processes:
    processTable <- getProcessAndFunctionNames(
        projectPath = projectPath, 
        modelName = modelName, 
        afterProcessID = afterProcessID, 
        beforeProcessID = beforeProcessID, 
        argumentFilePaths = argumentFilePaths
    )
    if(length(processTable) == 0) {
        return(processTable)
    }
    
    ##### (1) Add process parameters: #####
    processParameters <- mapply(
        getProcessParameters, 
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processTable$processID, 
        MoreArgs = list(argumentFilePaths = argumentFilePaths), 
        SIMPLIFY = FALSE
    )
    processParameters <- data.table::rbindlist(processParameters)
    processTable <- data.table::data.table(
        processTable, 
        processParameters
    )
    
    ##### (2) Add function inputs: #####
    functionInputs <- lapply(processTable$processID, function(processID) getFunctionInputs(projectPath, modelName, processID, only.valid = TRUE, argumentFilePaths = argumentFilePaths))
    processTable[, functionInputs := ..functionInputs]
    
    ##### (3) Add function parameters: #####
    functionParameters <- lapply(processTable$processID, function(processID) getFunctionParameters(projectPath, modelName, processID, only.valid = TRUE, argumentFilePaths = argumentFilePaths))
    processTable[, functionParameters := ..functionParameters]
    
    return(processTable)
}
#' 
#' @export
#' @rdname getProcessTable
#' 
getProcessAndFunctionNames <- function(projectPath, modelName, afterProcessID = NULL, beforeProcessID = NULL, argumentFilePaths = NULL) {
    
    # Read the memory file paths once, and insert to the get* functions below to speed things up:
    if(length(argumentFilePaths) == 0) {
        argumentFilePaths <- getArgumentFilePaths(projectPath)
    }
    
    ##### (1) Get the table of process name and ID: #####
    processIndexTable <- readProcessIndexTable(projectPath, modelName)
    # Return an empty data.table if the processIndexTable is empty:
    if(nrow(processIndexTable) == 0) {
        return(data.table::data.table())
    }
    
    # Subset the table to up until the reuqested beforeProcessID, if given:
    if(length(beforeProcessID)) {
        atProcessID <- which(beforeProcessID == processIndexTable$processID)
        if(length(atProcessID) == 0) {
            stop("The processID specified in 'beforeProcessID' does not exist in the model ", modelName, " of project ", projectPath, ".")
        }
        processIndexTable <- processIndexTable[seq_len(atProcessID - 1), ]
    }
    # Remove the table up until the reuqested afterProcessID, if given:
    if(length(afterProcessID)) {
        atProcessID <- which(afterProcessID == processIndexTable$processID)
        if(length(atProcessID) == 0) {
            stop("The processID specified in 'afterProcessID' does not exist in the model ", modelName, " of project ", projectPath, ".")
        }
        processIndexTable <- processIndexTable[- seq_len(atProcessID), ]
    }
    
    # Add the projectPath:
    processIndexTable[, projectPath := projectPath]
    
    ##### (2) Add function names: #####
    functionName <- mapply(
        getFunctionName, 
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processIndexTable$processID, 
        MoreArgs = list(argumentFilePaths = argumentFilePaths)
    )
    processIndexTable[, functionName := ..functionName]
    
    return(processIndexTable)
}


checkFunctionInput <- function(functionInput, functionInputDataType, processIndexTable) {
    
    # Expect an error, and return FALSE if all checks passes:
    functionInputError <- TRUE
    # (0) Chech that the function input is a string with positive number of characters:
    if(!is.character(functionInput)) {
        warning("StoX: Function input must be a character string (", functionInputDataType, ").")
    }
    # (1) Error if empty string:
    else if(nchar(functionInput) == 0) {
        warning("StoX: Function input must be a non-empty character string (", functionInputDataType, ").")
    }
    # (2) Error if not the name of a previous process:
    else if(! functionInput %in% processIndexTable$processName) {
        warning("StoX: Function input ", functionInput, " is not the name of a previous process (", functionInputDataType, ").")
    }
    else {
        atRequestedPriorProcess <- which(functionInput == processIndexTable$processName)
        outputDataTypeOfRequestedPriorProcess <- getStoxFunctionMetaData(processIndexTable$functionName[atRequestedPriorProcess], "functionOutputDataType")
        
        # (3) Error if the previous process returns the wrong data type:
        if(! functionInputDataType %in% outputDataTypeOfRequestedPriorProcess) {
            warning("StoX: Function input of process ", processIndexTable$processName[atRequestedPriorProcess], " does not return the correct data type (", functionInputDataType, ").")
        }
        # (4) Error if the previous process is not enabled:
        else if(!processIndexTable$enabled[atRequestedPriorProcess]) {
            warning("StoX: The process ", processIndexTable$processName[atRequestedPriorProcess], " is not enabled.")
        }
        # (5) Error if the previous process has input error:
        else if(processIndexTable$functionInputError[atRequestedPriorProcess]) {
            warning("StoX: The process ", processIndexTable$processName[atRequestedPriorProcess], " has input error.")
        }
        else {
            functionInputError <- FALSE
        }
    }
    return(functionInputError)
}

checkFunctionInputs <- function(processIndexTable) {
    # Get the function input name and value paris:
    functionInput <- processIndexTable$functionInputs[[nrow(processIndexTable)]]
    functionInputDataType <- names(processIndexTable$functionInputs[[nrow(processIndexTable)]])
    functionInputError <- mapply(
        checkFunctionInput, 
        functionInput = functionInput, 
        functionInputDataType = functionInputDataType, 
        MoreArgs = list(processIndexTable = processIndexTable)
    )
    
    # Error also if 
    
    return(functionInputError)
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


applyEmptyFunction <- function(process) {
    process$functionName <- ""
    process$functionInputs <- list()
    process$functionParameters <- list()
    return(process)
}


# This funciton is quite central, as it is resposible of setting the default values of funcitons. Only the function inputs and parameters introduced to a process using setFunctionName() can be modified:
setFunctionName <- function(process, newFunctionName, add.defaults = FALSE) {
    
    # If empty function name, return empty list:
    if(length(newFunctionName) == 0 || nchar(newFunctionName) == 0) {
        process <- applyEmptyFunction(process)
    }
    else {
        # Validate functionName:
        newFunctionName <- validateFunction(newFunctionName)
        
        # Insert the function name:
        process$functionName <- newFunctionName
        # Get the parameters to display, and their defaults:
        defaults <- getStoxFunctionParameterDefaults(process$functionName)
        
        # Detect which parameters are data types, which identifies them as function inputs (outputs from other processes):
        areInputs <- isFunctionInput(names(defaults))
        
        if(!add.defaults) {
            areNonEmpryString <- sapply(defaults, function(x) length(x) && is.character(x))
            if(any(areNonEmpryString)) {
                defaults[areNonEmpryString] <- lapply(defaults[areNonEmpryString], function(x) character(0))
            }
        }
        # Split the defaults into function parameters and function inputs:
        process$functionParameters <- defaults[!areInputs]
        process$functionInputs <- defaults[areInputs]
    }
    
    # Delete the processData, since these are no longer valid for the new function:
    process$processData <- list()
    
    # Return the process:
    process
}

onlyValidCharactersInProcessnName <- function(newProcessName) {
    # Check that the new process name ha one or more characters:
    positiveLength <- nchar(newProcessName) > 0
    
    # Check also for invalid characters:
    indValidCharacters <- gregexpr(getRstoxFrameworkDefinitions("validProcessNameSet"), newProcessName)[[1]]
    indInvalidCharacters <- setdiff(seq_len(nchar(newProcessName)), indValidCharacters)
    
    if(!positiveLength) {
        stop("Process names cannot be an empty string")
        #FALSE
    }
    else if(length(indInvalidCharacters)) {
        warning("Process names can only contain lower and upper letters, numbers, dot and underscore. Contained ", paste(strsplit(newProcessName, "")[indInvalidCharacters], collapse = ", "))
        FALSE
    }
    else {
        TRUE
    }
}

checkProcessNameAgainstExisting <- function(projectPath, newProcessName) {
    # Check the process names of the model:
    processIndexTable <- readProcessIndexTable(projectPath = projectPath)
    if(newProcessName %in% processIndexTable$processName) {
        #warning("StoX: The new process name (", newProcessName, ") cannot be identical to the name of an existing process #within the same model (", paste(processIndexTable$processName, collapse = ", "), ")")
        #FALSE
        stop("StoX: The new process name (", newProcessName, ") cannot be identical to the name of an existing process within the same model (", paste(processIndexTable$processName, collapse = ", "), ")")
    }
    else {
        TRUE
    }
}

validateProcessName <- function(projectPath, newProcessName) {
    onlyValidCharactersInProcessnName(newProcessName) & checkProcessNameAgainstExisting(projectPath = projectPath, newProcessName = newProcessName)
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
            # Added this if statement on 2020-04-03 (and re-added after some rebase trouble on 2020-05-25), since it prevents parameters from being deleted:
            if(!is.null(insertList[[ind]])) {
                list[[names(insertList[ind])]] <- insertList[[ind]]
            }
        }
    }
    
    list
}


##### Functions for modifying individual process arguments. These are called in the exported function modifyProcess(): #####
modifyFunctionName <- function(projectPath, modelName, processID, newFunctionName, archive = TRUE, add.defaults = FALSE) {
    
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
        process <- setFunctionName(process, newFunctionName, add.defaults = add.defaults)
        
        # Store the changes:
        setProcessMemory(
            projectPath = projectPath, 
            modelName = modelName, 
            processID = processID, 
            process = process, 
            archive = archive
        )
        
        # Return a flag TRUE if the function name was changed: 
        return(TRUE)
    }
    else {
        return(FALSE)
    }
    #process
}
modifyProcessName <- function(projectPath, modelName, processID, newProcessName, archive = TRUE) {
    
    # Get the current process name:
    processName <- getProcessName(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID
    )
    
    # Change the process name only if different from the existing:
    if(!identical(processName, newProcessName)) {
        # Validate the new process name (for invalid characters):
        if(validateProcessName(projectPath = projectPath, newProcessName = newProcessName)) {
            setProcessMemory(
                projectPath = projectPath, 
                modelName = modelName, 
                processID = processID, 
                argumentName = "processName", 
                argumentValue = newProcessName, 
                archive = archive
            )
        }
        
        # Modify the process name also in the proces index table:
        modifyProcessNameInProcessIndexTable(projectPath, modelName, processName, newProcessName)
        
        # Change the process name in all relevant function inputs of consecutivev processes:
        modifyProcessNameInFunctionInputs(projectPath, modelName, processName, newProcessName)
        
        # Return a flag TRUE if the process name was changed: 
        return(TRUE)
    }
    else {
        return(FALSE)
    }
    
    #processName
}
modifyFunctionParameters <- function(projectPath, modelName, processID, newFunctionParameters, archive = TRUE) {
    
    # Get the function parameters:
    functionParameters <- getFunctionParameters(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID
    )

    # Modify any file or directory paths to relative paths if possible:
    newFunctionParameters <- convertToRelativePaths(
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
            argumentValue = list(modifiedFunctionParameters),  # We need to list this to make it correspond to the single value of the argumentName parameter.
            archive = archive
        )
        
        # Return a flag TRUE if the function parameters were changed: 
        return(TRUE)
    }
    else {
        return(FALSE)
    }
    
    #modifiedFunctionParameters
}
modifyFunctionInputs <- function(projectPath, modelName, processID, newFunctionInputs, archive = TRUE) {
    
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
            argumentValue = list(modifiedFunctionInputs),  # We need to list this to make it correspond to the single value of the argumentName parameter.
            archive = archive
        )
    
        # Return a flag TRUE if the function inputs were changed: 
        return(TRUE)
    }
    else {
        return(FALSE)
    }
    
    #modifiedFunctionInputs
}
modifyProcessParameters <- function(projectPath, modelName, processID, newProcessParameters, archive = TRUE) {
    
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
            argumentValue = list(modifiedProcessParameters),  # We need to list this to make it correspond to the single value of the argumentName parameter.
            archive = archive
        )
    
        # Return a flag TRUE if the process parameters were changed: 
        return(TRUE)
    }
    else {
        return(FALSE)
    }
    
    #modifiedProcessParameters
}
modifyProcessData <- function(projectPath, modelName, processID, newProcessData, archive = TRUE, purge.processData = FALSE) {
    
    # Get the process data:
    processData<- getProcessData(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID
    )
    
    # Modify the process data:
    if(purge.processData) {
        modifiedProcessData <- newProcessData
    }
    else {
        modifiedProcessData <- setListElements(
            list = processData, 
            insertList = newProcessData, 
            projectPath = projectPath, 
            modelName = modelName, 
            processID = processID
        )
    }
    
    # Store the changes:
    if(!identical(processData, modifiedProcessData)) {
        setProcessMemory(
            projectPath = projectPath, 
            modelName = modelName, 
            processID = processID, 
            argumentName = "processData", 
            argumentValue = list(modifiedProcessData),  # We need to list this to make it correspond to the single value of the argumentName parameter.
            archive = archive
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
convertToRelativePaths <- function(functionParameters, projectPath, modelName, processID, warn = FALSE) {
    
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
        else if(warn) {
            warning("StoX: The specified file ", filePath, " is not present in the project folder (", projectPath, ")")
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
            warning("StoX: The file ", filePath, " does not exist.")
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



#' Modify a process
#' 
#' @inheritParams general_arguments
#' 
#' @export
#' 
modifyProcess <- function(projectPath, modelName, processName, newValues, archive = TRUE, add.defaults = FALSE, purge.processData = FALSE) {
    
    # The values of the process must be changed in the following order:
    # 1. Function name
    # 2. Function parameters
    # 2. Function inputs
    # 1. Process name
    # 1. Process parameters
    # 1. Process data
    if(!isOpenProject(projectPath)) {
        warning("StoX: The project ", projectPath, " is not open. Use openProject() to open the project.")
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
            newFunctionName = newValues$functionName, 
            archive = archive, 
            add.defaults = add.defaults
        )
    }
    
    # Function parameters:
    if(length(newValues$functionParameters)) {
        modified <- modified | modifyFunctionParameters(
            projectPath = projectPath, 
            modelName = modelName, 
            processID = processID, 
            newFunctionParameters = newValues$functionParameters, 
            archive = archive
        )
    }
    
    # Function inputs:
    if(length(newValues$functionInputs)) {
        modified <- modified | modifyFunctionInputs(
            projectPath = projectPath, 
            modelName = modelName, 
            processID = processID, 
            newFunctionInputs = newValues$functionInputs, 
            archive = archive
        )
    }
    
    # Process name:
    if(length(newValues$processName)) {
        modified <- modified | modifyProcessName(
            projectPath = projectPath, 
            modelName = modelName, 
            processID = processID, 
            newProcessName = newValues$processName, 
            archive = archive
        )
    }
    
    # Process parameters:
    if(length(newValues$processParameters)) {
        modified <- modified | modifyProcessParameters(
            projectPath = projectPath, 
            modelName = modelName, 
            processID = processID, 
            newProcessParameters = newValues$processParameters, 
            archive = archive
        )
    }
    
    # Process data:
    if(length(newValues$processData)) {
        modified <- modified | modifyProcessData(
            projectPath = projectPath, 
            modelName = modelName, 
            processID = processID, 
            newProcessData = newValues$processData, 
            archive = archive, 
            purge.processData = purge.processData
        )
    }
    
    return(modified)
}

#' 
#' 
#' @param parameter 
#' @param simplifyVector 
#' 
#' @export
#' 
# Convert JSON input to list:
parseParameter <- function(parameter, simplifyVector = TRUE) {
    # If empty string, convert to NULL for non-character type:
    if(is.character(parameter) && nchar(parameter) == 0) {
        return(NULL)
    }
    
    # Parse the JSON:
    out <- jsonlite::fromJSON(parameter, simplifyVector = simplifyVector)
    # If data.frame, convert to data.table:
    if(is.data.frame(out)) {
        out <- data.table::as.data.table(out)
    }
    return(out)
}
#parseParameter <- function(parameter, simplifyVector = TRUE) {
#    # If the parameter is JSON, convert to list:
#    if("json" %in% class(parameter)) {
#        parameter <- jsonlite::fromJSON(parameter, simplifyVector = simplifyVector)
#    }
#    #else if(is.character(parameter) && jsonlite::validate(parameter)) {
#    # No need to validate, as inavlid json will lead to error in jsonlite::parse_json:
#    else if(is.character(parameter)) {
#        parameter <- jsonlite::parse_json(parameter, simplifyVector = simplifyVector)
#    }
#    parameter
#}





getNewDefaultProcessName <- function(projectPath, modelName) {
    
    # Get all process names of the specified model:
    processIndexTable <- readProcessIndexTable(projectPath, modelName)
    processNames <- processIndexTable$processName
    
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
    if(length(integerID)) {
        processID <- paste0("P", formatC(integerID, width = numDigitsOfProcessIntegerID, format = "d", flag = "0"))
    }
    else {
        processID <- NULL
    }
    
    return(processID)
}



#' 
#' @export
#' 
addEmptyProcess <- function(projectPath, modelName, processName = NULL, archive = TRUE) {
    
    # Get a default new process name, or check the validity of the given process name:
    if(length(processName)) {
        validProcessnName <- validateProcessName(
            projectPath = projectPath, 
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
        process = process, 
        archive = archive
    )
    
    # Update the process index table:
    addToProcessIndexTable(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID, 
        processName = processName
    )
    
    # Return a data frame with process ID and name suited for appending to the process index table using addToProcessIndexTable():
    data.table::data.table(
        processID = processID, 
        processName = processName
    )
}

# Function to add all processes of template or project description:
addProcesses <- function(projectPath, projectMemory, returnProcessTable = TRUE, archive = TRUE, add.defaults = FALSE) {
    # Get the possible models:
    stoxModelNames <- getRstoxFrameworkDefinitions("stoxModelNames")
    
    # Loop through the possible models and add the processes:
    processes <- vector("list", length(stoxModelNames))
    names(processes) <- stoxModelNames
    
    for(modelName in stoxModelNames){
        for(ind in seq_along(projectMemory[[modelName]])){
            #processes [[modelName]] [[ind]] <- 
            temp <- addProcess(
                projectPath = projectPath, 
                modelName = modelName, 
                values = projectMemory[[modelName]][[ind]], 
                returnProcessTable = returnProcessTable, 
                archive = archive, 
                add.defaults = add.defaults
            )
        }
    }
    
    # Return the process table:
    if(returnProcessTable) {
        processTable <- getProcessTable(projectPath = projectPath, modelName = modelName)
        activeProcess <- getActiveProcess(projectPath = projectPath, modelName = modelName)
        return(
            list(
                processTable = processTable, 
                activeProcess = activeProcess, 
                saved = isSaved(projectPath)
            )
        )
    }
    else {
        return(TRUE)
    }
    
}


#' Add a StoX process to a model.
#' 
#' @inheritParams Projects
#' @inheritParams getProcessOutput
#' @param values A list of values to assign to the process, such as list(processName = "ReadBiotic", functionName = "RstoxBase::ReadBiotic").
#' @param returnProcessTable Logical: If TRUE return the process table.
#' 
#' @export
#' 
addProcess <- function(projectPath, modelName, values = NULL, returnProcessTable = TRUE, archive = TRUE, add.defaults = FALSE) {
    
    # values must be a list:
    if(length(values) && !is.list(values)) {
        warning("StoX: Process not added. Values must be a list of specifics of the process.")
    }
    
    # Create an empty process:
    process <- addEmptyProcess(
        projectPath = projectPath, 
        modelName = modelName, 
        processName = values$processName, 
        archive = FALSE
    )
    
    # Apply the arguments:
    valuesSansProcessName <- values[names(values) != "processName"]
    modifyProcess(
        projectPath = projectPath, 
        modelName = modelName, 
        processName = process$processName, 
        newValues = valuesSansProcessName, 
        archive = archive, 
        add.defaults = add.defaults
    )
    
    # Return the process:
    #process <- getProcess(
    #    projectPath = projectPath, 
    #    modelName = modelName, 
    #    processID = process$processID
    #)
    
    # Set the status as not saved (saving is done when running a process):
    setSavedStatus(projectPath, status = FALSE)
    
    # Return the process table if requested:
    if(returnProcessTable) {
        processTable <- getProcessTable(projectPath = projectPath, modelName = modelName)
        activeProcess <- getActiveProcess(projectPath = projectPath, modelName = modelName)
        return(
            list(
                processTable = processTable, 
                activeProcess = activeProcess, 
                saved = isSaved(projectPath)
            )
        )
    }
    else {
        return(TRUE)
    }
}
#' 
#' @export
#' 
removeProcess <- function(projectPath, modelName, processID) {
    
    # Reset the model to the process just before the removed process:
    resetModel(projectPath = projectPath, modelName = modelName, processID = processID, shift = -1)
    
    # Update the project memory:
    removeProcessMemory(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID
    )
    
    # Update the process index table:
    removeFromProcessIndexTable(projectPath = projectPath, modelName = modelName, processID = processID)
    
    # Set the status as not saved (saving is done when running a process):
    setSavedStatus(projectPath, status = FALSE)
    
    # Return the process table:
    processTable <- getProcessTable(projectPath = projectPath, modelName = modelName)
    activeProcess <- getActiveProcess(projectPath = projectPath, modelName = modelName)
    return(
        list(
            processTable = processTable, 
            activeProcess = activeProcess, 
            saved = isSaved(projectPath)
        )
    )
}


#' 
#' @export
#' 
rearrangeProcesses <- function(projectPath, modelName, processID, afterProcessID = NULL) {
    # Rearrange the process index table defining the order of the processes:
    #if(length(afterProcessID)) {
    activeProcessID <- rearrangeProcessIndexTable(projectPath, modelName, processID, afterProcessID)
    #}
    
    # Reset the model to the first of afterProcessID and the processes to be rearranged, but only if there was any change:
    if(length(activeProcessID)) {
        resetModel(projectPath, modelName, processID = activeProcessID)
        # Set the status as not saved (saving is done when running a process):
        setSavedStatus(projectPath, status = FALSE)
    }
    
    # Return the process table:
    processTable <- getProcessTable(projectPath = projectPath, modelName = modelName)
    activeProcess <- getActiveProcess(projectPath = projectPath, modelName = modelName)
    return(
        list(
            processTable = processTable, 
            activeProcess = activeProcess, 
            saved = isSaved(projectPath)
        )
    )
}


#### Functions to run models: ####
#' 
#' @export
#' 
runProcess <- function(projectPath, modelName, processID, msg = TRUE, save = TRUE, fileOutput = NULL, setUseProcessDataToTRUE = TRUE, purge.processData = FALSE, replaceArgs = list(), replaceData = NULL, output.file.type = c("text", "binary")) {
    
    # Get the process:
    process <- getProcess(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID
    )
    process$processID <- processID
    
    # Check that the function name is given:
    if(length(process$functionName) == 0 || nchar(process$functionName) == 0) {
        stop("The process with process ID ", processID, " does not specify a function name.")
    }
    
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
    # Add the processPath if a bootstrap function:
    if(isBootstrapFunction(process$functionName)) {
        functionArguments$projectPath <- projectPath
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
    functionArguments <- c(
        functionArguments, 
        functionInputs, 
        process$functionParameters
    )
    
    # Insert any arguments in replaceArgs:
    replaceArgsToInsert <- intersect(names(replaceArgs), names(functionArguments))
    if(length(replaceArgsToInsert)) {
        functionArguments[replaceArgsToInsert] <- replaceArgs[replaceArgsToInsert]
    }
    
    # Get absolute paths:
    functionArguments <- getAbsolutePaths(
        functionParameters = functionArguments, 
        projectPath = projectPath, 
        modelName = modelName,
        processID = processID
    )
    
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
            functionArguments, 
            envir = as.environment(paste("package", getPackageNameFromPackageFunctionName(process$functionName), sep = ":"))
        ), 
        error = function(err) {
            failed <<- TRUE
            stop(err)
        }
    )
    # Apply the replaceData, which can be a function with first parameter the processOutput and additional parameters gievn in ..., or an actual object to replaec the output by:
    #thisReplaceData <- replaceData[[process$processName]]
    thisReplaceData <- replaceData
    if(is.list(thisReplaceData) && is.character(thisReplaceData[[1]]) && exists(thisReplaceData[[1]])) {
        processOutput <- do.call(thisReplaceData[[1]], c(list(processOutput), thisReplaceData[[2]]))
    }
    else if(length(replaceData)) {
        processOutput <- thisReplaceData
    }
    
    # Return the process output if not to be saved:
    if(!save) {
        return(processOutput)
    }
    
    if(failed){
        return(FALSE)
    }
    else{
        # Update the active process ID:
        writeActiveProcessID(projectPath, modelName, processID, processDirty = FALSE)
        
        # If a valid output class, wrap the function output to a list named with the data type:
        if(firstClass(processOutput) %in% getRstoxFrameworkDefinitions("validOutputDataClasses")) {
            processOutput <- list(processOutput)
            names(processOutput) <- getStoxFunctionMetaData(process$functionName, "functionOutputDataType")
        }
        
        # Store the processData (this must be a named list of only one data table):
        if(isProcessDataFunction(process$functionName)) {
            modifyProcessData(projectPath, modelName, processID, processOutput, purge.processData = purge.processData)
            
            # Set the function parameters UseProcessData to TRUE:
            if(setUseProcessDataToTRUE) {
                setUseProcessDataToTRUE(projectPath, modelName, processID)
            }
        }
        else {
            # Set the propertyDirty flag to TRUE, so that a GUI can update the properties:
            writeActiveProcessID(projectPath, modelName, propertyDirty = FALSE) 
        }
        
        # Write to memory files:
        writeProcessOutputMemoryFiles(processOutput = processOutput, projectPath = projectPath, modelName = modelName, processID = process$processID, type = "memory")
        
        # Write to text files:
        # Use fileOutput if given and process$processParameters$fileOutput otherwise to determine whether to write the output to the output.file.type:
        if(if(length(fileOutput)) fileOutput else process$processParameters$fileOutput) {
            writeProcessOutput(processOutput = processOutput, projectPath = projectPath, modelName = modelName, processID = process$processID, processName = process$processName, output.file.type = output.file.type)
        }
        
        #invisible(processOutput)
        TRUE
    }
}


setUseProcessDataToTRUE <- function(projectPath, modelName, processID) {
    # Try setting UseProcessData to TRUE:
    modified <- modifyFunctionParameters(projectPath, modelName, processID, list(UseProcessData = TRUE))
    # If modified, set propertyDirty to TRUE:
    if(modified) {
        writeActiveProcessID(projectPath, modelName, propertyDirty = TRUE) 
    }
}



##################################################
##################################################
#' Get output of a StoX process.
#' 
#' Gets the output of a process that has been run.
#' 
#' @inheritParams fixedWidthDataTable
#' @inheritParams readProcessOutputFile
#' @param modelName The name of the model (one of "baseline", "analysis" and "report").
#' @param processID The ID of the process.
#' @param tableName The name of the table to extract from the process.
#' @param subFolder If the process returns subfolders (ReadBiotic and ReadAcoustic, where the subfolders represent files), specify the name of the folder with this parameter.
#' @param drop Logical: If TRUE drop the list if only one element.
#' @param drop.datatype Logical: If TRUE drop the top level of the output if in a list, which is the level named by the data type.
#' 
#' @export
#' 
#' @inheritParams Projects
#' @export
#' 
getProcessOutput <- function(projectPath, modelName, processID, tableName = NULL, subFolder = NULL, flatten = FALSE, pretty = FALSE, pageindex = integer(0), linesPerPage = 1000L, columnSeparator = " ", lineSeparator = NULL, na = "-", list.pretty = FALSE, drop = FALSE, drop.datatype = TRUE) {
    
    # If the 'tableName' contains "/", extract the 'subFolder' and 'tableName':
    if(any(grepl("/", tableName))) {
        subFolder_tableName <- strsplit(tableName, "/")
        subFolder <- sapply(subFolder_tableName, "[", 1)
        tableName <- sapply(subFolder_tableName, "[", 2)
    }
   
    # Get the files 
    processOutputFiles <- getProcessOutputFiles(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID
    )
    
    # Get the directory holding the output files:
    folderPath <- getProcessOutputFolder(projectPath = projectPath, modelName = modelName, processID = processID, type = "memory")
    # Detect whether the output is a list of tables (depth 1) or a list of lists of tables (depth 2):
    folderDepth <- getFolderDepth(folderPath)
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
        warning("StoX: Invalid specification of projectPath, modelName, processID or tableName (most likely tableName).")
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


unlistProcessOutput <- function(processOutput) {
    if(is.list(processOutput[[1]]) && !data.table::is.data.table(processOutput[[1]])) {
        names1 <- names(processOutput)
        names2 <- lapply(processOutput, names)
        processOutput <- unlist(processOutput, recursive = FALSE)
        names(processOutput) <- paste(rep(names1, lengths(names2)), unlist(names2), sep = "/")
    }
    return(processOutput)
}

#' Function to read a single process output file, possibly by pages and in flattened and pretty view:
#' 
#' @inheritParams fixedWidthDataTable
#' @param filePath The file path of the process output file to read.
#' @param flatten Logical: Should the output tables that contain cells of length > 1 be expanded to that the other columns are repeated, resulting in a regular table.
#' @param pretty Logical: If TRUE pad with space in each cell to the maximum number of characters of the column including header.
#' @param pageindex A vevctor of the pages to return with \code{linesPerPage} number of lines (rows). Default is to not split into pages.
#' @param linesPerPage The number of lines per page if \code{pageindex} is given.
#' 
readProcessOutputFile <- function(filePath, flatten = FALSE, pretty = FALSE, pageindex = integer(0), linesPerPage = 1000L, columnSeparator = " ", lineSeparator = NULL, na = "-", list.pretty = FALSE) {
    
    # Read the process output file:
    data <- readMemoryFile(filePath)
    
    # Flatten the output so that cells which are vectors are transposed and the non-vector cells of the same line repeated:
    if(flatten) {
        data <- flattenProcessOutput(data)
    }
    
    # If a table, allow additional options:
    if(data.table::is.data.table(data)) {
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
    }
    else {
        # Add numberOfLines = 1 and numberOfPages = 1 to conform to the output used for tables in the GUI:
        if(pretty) {
            data <- list(
                data = list(data), 
                numberOfLines = 1, 
                numberOfPages = 1
            )
        }
        else {
            
        }
    }
    
    return(data)
}

flattenProcessOutput <- function(processOutput) {
    #if(firstClass(processOutput) == "SpatialPolygons") {
    if(firstClass(processOutput) == "SpatialPolygonsDataFrame") {
        geojsonio::geojson_json(processOutput, pretty = TRUE)
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
#' @inheritParams getProcessOutput
#' @param onlyTableNames Logical: If TRUE return only table names.
#' @export
#' 
getProcessOutputFiles <- function(projectPath, modelName, processID, onlyTableNames = FALSE, type = "memory") {
    
    # Get the directory holding the output files:
    folderPath <- getProcessOutputFolder(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID, 
        type = type
    )
    
    if(length(processID) > 1) {
        stop("processID must have length 1 (was ", length(processID), ")")
    }
    
    # If the folder does not exist, it is a sign that the process does not exist:
    if(length(folderPath) == 0 || !file.exists(folderPath)) {
        #processName <- getProcessName(projectPath, modelName, processID)
        stop("Has the previous processes been run? The folder ", folderPath, " does not exist.")
    }
    
    # Detect whether the output is a list of tables (depth 1) or a list of lists of tables (depth 2):
    folderDepth <- getFolderDepth(folderPath)
    
    # Get the file paths of the memory files and prepare the processOutput for writing to these files:
    processOutputFiles <- getFilesRecursiveWithOrder(folderPath)
    #if(folderDepth == 1) {
    #    processOutputFiles <- listMemoryFiles(folderPath)
    #}
    #else {
    #    # Get the sub folder paths and create the folders:
    #    folderPaths <- list.dirs(folderPath, recursive = FALSE)
    #    processOutputFiles <- lapply(folderPaths, listMemoryFiles)
    #    names(processOutputFiles) <- basename(folderPaths)
    #}
    
    if(onlyTableNames) {
        # Strip to only the table names of the folderPath:
        processOutputFiles <- gsub(path.expand(folderPath), "", unname(unlist(processOutputFiles)))
        # Remove the resulting trailing "/" and the file extension:
        processOutputFiles <- substring(processOutputFiles, 2)
        processOutputFiles <- tools::file_path_sans_ext(processOutputFiles)
    }
    
    processOutputFiles
}


# Function to get the file paths of the memory files recursively:
getFilesRecursiveWithOrder <- function(folderPath) {
    dirs <- list.dirs(folderPath, recursive = FALSE)
    if(length(dirs)) {
        output <- lapply(dirs, getFilesRecursiveWithOrder)
        names(output) <- basename(dirs)
        return(output)
    }
    else {
        listMemoryFiles(folderPath)
    }
}


# Function to list RDS file in a folder:
listMemoryFiles <- function(folderPath) {
    # Create a list of the files, and name it with the file names sans ext representing the output name:
    ext <- getRstoxFrameworkDefinitions("allMemoryFileFormats")
    extPattern <- paste0("\\.", ext, "$", collapse = "|")
    #out <- as.list(list.files(folderPath, full.names = TRUE, pattern = "\\.rds$"))
    out <- as.list(list.files(folderPath, full.names = TRUE, pattern = extPattern))
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

#' 
#' @inheritParams Projects
#' @export
#' 
getProcessOutputTableNames <- function(projectPath, modelName, processID) {
    # Get the output file names, and add the process name:
    tableNames <- getProcessOutputFiles(projectPath, modelName, processID, onlyTableNames = TRUE)
    processName <- getProcessName(projectPath, modelName, processID)
    #tableNames <- paste(processName, tableNames, sep ="_")
    
    # Ensure that this is a vector in JSON after auto_unbox = TRUE, by using as.list():
    tableNames <- as.list(tableNames)
    return(tableNames)
}


deleteProcessOutput <- function(projectPath, modelName, processID, type = c("memory", "output")) {
    # Get the directory holding the output files:
    folderPath <- getProcessOutputFolder(projectPath = projectPath, modelName = modelName, processID = processID, type = type)
    unlink(folderPath, recursive = FALSE, force = TRUE)
}




getProcessOutputFolder <- function(projectPath, modelName, processID, type = c("memory", "output"), subfolder = NULL) {
    type <- match.arg(type)
    if(type == "memory") {
        folderPath <- file.path(getProjectPaths(projectPath, "dataModelsFolder"), modelName, processID)
    }
    else if(type == "output") {
        # Get the processName and build the folderPath:
        processName <- getProcessNameFromProcessID(projectPath, modelName, processID)
        folderPath <- file.path(getProjectPaths(projectPath, "Output"), modelName, processName)
        # Add subfolder:
        if(length(subfolder)) {
            folderPath <- file.path(folderPath, subfolder)
        }
    }
    else {
        stop("typetype must be one of \"memory\" and \"output\"")
    }
    return(folderPath)
}



getProcessIndexFromProcessID <- function(projectPath, modelName, processID) {
    processIndexTable <- readProcessIndexTable(projectPath, modelName)
    processIndex <- which(processIndexTable$processID == processID)
    processIndex
}

getProcessNameFromProcessID <- function(projectPath, modelName, processID) {
    processIndexTable <- readProcessIndexTable(projectPath, modelName)
    thisProcessID <- processID
    processIndexTable[processID == thisProcessID, processName]
}



# Function to write process output to a text file in the output folder:
writeProcessOutput <- function(processOutput, projectPath, modelName, processID, processName, output.file.type = c("text", "binary")) {
    
    output.file.type <- match.arg(output.file.type)
    
    # Return NULL for empty process output:
    if(length(processOutput)) {
        # Unlist introduces dots, and we replace by underscore:
        processOutput <- unlistToDataType(processOutput)
        #names(processOutput) <- gsub(".", "_", names(processOutput), fixed = TRUE)
        
        folderPath <- getProjectPaths(
            projectPath = projectPath, 
            name = modelName
        )
        
        #processIndex <- getProcessIndexFromProcessID(projectPath = projectPath, modelName = modelName, processID = processID)
        #fileNamesSansExt <- paste(processIndex, processName, names(processOutput), sep = "_")
        
        # Added on 2020-06-16. Add the data type in the file name only if multiple outputs:
        if(length(processOutput) > 1) {
            fileNamesSansExt <- paste(processName, names(processOutput), sep = "_")
        }
        else {
            fileNamesSansExt <- processName
        }
        filePathsSansExt <- file.path(folderPath, paste(fileNamesSansExt))
        
        # Set the file name:
        mapply(reportFunctionOutputOne, processOutput, filePathsSansExt, output.file.type = output.file.type)
        # lapply(processOutput, reportFunctionOutputOne)
    }
    else {
        NULL
    }
}

# Function for writing one element of the function output list:
reportFunctionOutputOne <- function(processOutputOne, filePathSansExt, output.file.type) {
    
    if(output.file.type == "text") {
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
                if(length(processOutputOne) == 0) {
                    cat("", file = filePath)
                }
                else {
                    data.table::fwrite(processOutputOne, filePath, sep = "\t")
                }
            }
            else {
                stop("Unknown function output: ", class(processOutputOne))
            }
        }
    }
    else if(output.file.type == "binary") {
        if(length(processOutputOne)){
            # Add file extension:
            filePath <- paste(filePathSansExt, "rds", sep = ".")
            # Write to rds file:
            saveRDS(processOutputOne, file = filePath)
        }
    }
    else {
        stop("output.file.type must be one of \"text\" and \"binary\"")
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


# Function to write process output to a memory file:
writeProcessOutputMemoryFiles <- function(processOutput, projectPath, modelName, processID, type = c("memory", "output"), subfolder = NULL) {
    if(length(processOutput)) {
        # Get the path to the folder to place the memory file in:
        folderPath <- getProcessOutputFolder(projectPath = projectPath, modelName = modelName, processID = processID, type = type, subfolder = subfolder)
        writeProcessOutputTables(
            processOutput, 
            folderPath = folderPath, 
            writeOrderFile = TRUE)
    }
    else {
        NULL
    }
}


# Function to write process output to a memory file:
writeProcessOutputTables <- function(processOutput, folderPath, writeOrderFile = TRUE) {
    if(length(processOutput)) {
        # Create the folder if not existing:
        dir.create(folderPath, recursive = TRUE, showWarnings = FALSE)
        
        # Detect whether the output is a list of tables (depth 1) or a list of lists of tables (depth 2):
        outputDepth <- getOutputDepth(processOutput)
        
        # Get the file paths of the memory files and prepare the processOutput for writing to these files:
        if(outputDepth == 1) {
            #fileNames <- getProcessOutputMemoryFileNames(processOutput)
            fileNamesSansExt <- names(processOutput)
            filePaths <- file.path(folderPath, fileNamesSansExt)
            
            # Wrap in a list to coordinate with using the saveRDSs():
            filePaths <- list(filePaths)
            processOutput <- list(processOutput)
        }
        else {
            # Get the sub folder paths and create the folders:
            folderPaths <- file.path(folderPath, names(processOutput))
            lapply(folderPaths, dir.create, recursive = TRUE, showWarnings = FALSE)
            
            # Create the file names and add the folder paths to the file names (flattening the output):
            fileNamesSansExt <- lapply(processOutput, names)
            filePaths <- mapply(file.path, folderPaths, fileNamesSansExt, SIMPLIFY = FALSE)
        }
        # Write the individual tables:
        mapply(writeMemoryFiles, processOutput, filePaths, writeOrderFile = writeOrderFile)
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


#' Delete the contents of the output folder of a model.
#' 
#' This function is run by \code{\link[RstoxAPI]{runModel}} to clear off any files present in the output folder of a model. It should also be used by GUIs when running a model.
#' 
#' @inheritParams general_arguments
#' 
#' @export
#' 
purgeOutput <- function(projectPath, modelName) {
    folderPath <- getProjectPaths(projectPath = projectPath, name = modelName)
    unlink(folderPath, recursive = TRUE, force = TRUE)
    dir.create(folderPath)
}

#' 
#' @export
#' 
#runModel <- function(projectPath, modelName, startProcess = 1, endProcess = Inf, save = TRUE, force = FALSE) {
runProcesses <- function(projectPath, modelName, startProcess = 1, endProcess = Inf, msg = TRUE, save = TRUE, force.restart = FALSE, fileOutput = NULL, setUseProcessDataToTRUE = TRUE, purge.processData = FALSE, replaceData = list(), replaceArgs = list(), output.file.type = c("text", "binary"), ...) {

    # Get the processIDs:
    processIndexTable <- readProcessIndexTable(projectPath, modelName, startProcess = startProcess, endProcess = endProcess)
    processIDs <- processIndexTable$processID
    processNames <- processIndexTable$processName
    if(!length(processIDs)) {
        warning("StoX: Empty project, ", projectPath)
        return(NULL)
    }
    
    # Check for parameters to override the processes by in "...":
    #replaceArgs <- getReplaceArgs(replaceArgs, ..., processNames = processNames)
    replaceArgs <- getReplaceArgs(replaceArgs, ...)
    
    # Check that the project exists:
    failedVector <- logical(length(processIDs))
    if(!isProject(projectPath)) {
        warning("StoX: The StoX project ", projectPath, " does not exist")
        return(failedVector)
    }
    
    # Check that the project is open:
    if(!isOpenProject(projectPath)) {
        warning("StoX: The StoX project ", projectPath, " is not open. Use openProject() to open the project.")
        return(failedVector)
    }
    
    # Chech that none of the models of the project are running:
    if(isRunning(projectPath) && !force.restart) {
        warning("StoX: The project is running (", projectPath, "). Use force.restart = TRUE to force restart the project.")
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
    
    # Reset the model to the process just before the removed process:
    resetModel(projectPath = projectPath, modelName = modelName, processID = processIDs[1], shift = -1)
    
    # Try running the processes, and retun the failedVector if craching:
    #tryCatch(
    
    #if(length(fileOutput)) {
    #    replaceArgs <- list(fileOutput = fileOutput)
    #}
    #else {
    #    replaceArgs = list()
    #}
    #    {
    
    
    
    # Empty the folder:
    #folderPath <- getProjectPaths(projectPath = projectPath, name = modelName)
    #unlink(folderPath, recursive = TRUE, force = TRUE)
    #dir.create(folderPath)
    
    #for(processID in processIDs) {
    #    #status[processID] <- runProcess(projectPath = projectPath, modelName = modelName, processID = processID)
    #    runProcess(projectPath = projectPath, modelName = modelName, processID = processID, replaceArgs = replaceArgs[[]], fileOutput = fileOutput, setUseProcessDataToTRUE = setUseProcessDataToTRUE, purge.processData = purge.processData)
    #}
    
    mapply(
        runProcess, 
        processID = processIDs, 
        replaceArgs = replaceArgs[processNames], 
        replaceData = replaceData[processNames], 
        MoreArgs = list(
            projectPath = projectPath, 
            modelName = modelName, 
            fileOutput = fileOutput, 
            setUseProcessDataToTRUE = setUseProcessDataToTRUE, 
            purge.processData = purge.processData, 
            msg = msg, 
            output.file.type = output.file.type
        )
    )
    
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
        processTable = getProcessTable(projectPath = projectPath, modelName = modelName), 
        interactiveMode = getInteractiveMode(projectPath = projectPath, modelName = modelName, processID = utils::tail(processIDs, 1)), 
        activeProcess = getActiveProcess(projectPath = projectPath, modelName = modelName), 
        saved = isSaved(projectPath)
    )
}


#getReplaceArgs <- function(replaceArgs = list(), ..., processNames = NULL){
#    
#    # Get the specifications given as '...':
#    dotlist <- list(...)
#    
#    # Merge and unique the inputs:
#    replaceArgs <- c(replaceArgs, dotlist)
#    # parlist <- unique(parlist) THIS REMOVED THE NAMES AND SHOULD NOT BE USED
#    replaceArgs <- replaceArgs[!duplicated(replaceArgs)]
#    
#    # Keep only the elements named by a process:
#    if(length(processNames)) {
#        replaceArgs <- replaceArgs[names(replaceArgs) %in% processNames]
#    }
#    
#    return(replaceArgs)
#}
getReplaceArgs <- function(replaceArgs = list(), ...){
    
    # Get the specifications given as '...':
    dotlist <- list(...)
    
    # Merge and unique the inputs:
    replaceArgs <- c(replaceArgs, dotlist)
    # parlist <- unique(parlist) THIS REMOVED THE NAMES AND SHOULD NOT BE USED
    replaceArgs <- replaceArgs[!duplicated(replaceArgs)]
    
    return(replaceArgs)
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
# This function applies the conversion functions defined in the \code{stoxFunctionAttributes} list defined in each StoX function package.
convertProjectDescription1.92 <- function(projectDescription) {
    # Get the current project description:
    #projectDescription <- getProjectMemoryData(projectPath)
    
    ## Get the StoX version:
    #StoxVersion <- attr(projectDescription, "StoxVersion")
    #if(resourceversion < "1.92") {
    #    stop("Backward compatibility not supported for versions of StoX prior to 2.7")
    #}
    
    # Checkc the version, issuing an error if resourceVersion is set and lower than "1.92":
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
# Check the version for backwards compatibility
checkVersion <- function(projectDescription, resourceVersion = NULL, RstoxFrameworkVersion = NULL) {
    # Get the StoxVersion from the attributes:
    savedResourceVersion <- attr(projectDescription, "resourceversion")
    savedRstoxFrameworkVersion <- attr(projectDescription, "RstoxFrameworkVersion")
    
    # Issue an error if the project.xml is before the backwards compatibility time limit:
    if(savedResourceVersion < "1.92") {
        stop("Backward compatibility not supported for versions of StoX prior to 2.7 (resourceversion 1.92)")
    }
    
    if(length(resourceVersion) && savedResourceVersion == resourceVersion) {
        return(TRUE)
    }
    else if(length(RstoxFrameworkVersion) && savedRstoxFrameworkVersion == RstoxFrameworkVersion) {
        return(TRUE)
    }
    else {
        return(FALSE)
    }
}


