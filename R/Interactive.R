
# Decided after discussion with Åsmund on 2019-11-12:

# 1. We will strive for minimum redundancy in the process data. Process data will be saved as data tables with the possibility of variable length vectors in each cell, such as StratumID, StratumName and a vector of PSUs, and saved to the project.xml as JSON






#- Add formatProjectDescription() which converts process data to JSON strings (except Stratum, which is GEOJSON) using processData2JSON()
#- 
#



############################################################
############################################################
#' Add or remove biotic stations from assignments
#' 
#' The functions \code{addStations} and \code{removeStations} adds or removes biotic stations from the Assignment process data of the specified process.
#' 
#' @param acousticPSU   The acoustic primary sampling unit (PSU) for which to remove the haul
#' @param acousticLayer The acoustic Layer for which to remove the haul
#' @param Haul       The biotic haul to remove (can be a vector of hauls).
#' 
#' @details 
#' The assignment IDs are refreshed for every change, after sorting the assignemnts by the PSU column.
#' 
#' @examples
#' # Create artificial assignment data:
#' Assignment <- data.table::data.table(
#'     PSU = paste0("T", c(1,1, 2, 3,3)), 
#'     Layer = "L1", 
#'     Haul = c(1,2, 2, 1,2), 
#'     AssignmentID = c(1,1, 2, 1,1)
#' )
#' 
#' # Add a haul:
#' Assignment1 <- assignment_addStations("T3", "L1", 4, Assignment)
#' all.equal(Assignment, Assignment2)
#' 
#' # Remove the same haul:
#' Assignment2 <- assignment_removeStations("T3", "L1", 4, Assignment1)
#' all.equal(Assignment, Assignment2)
#' 
#' @inheritParams getProcessOutput
#' @name Assignment
#' 
NULL
#' 
#' @export
#' @rdname Assignment
#' 
addHauls <- function(acousticPSU, acousticLayer, Haul, projectPath, modelName, processID) {
    modifyAssignment(
        acousticPSU, 
        acousticLayer, 
        Haul, 
        projectPath, 
        modelName, 
        processID, 
        action = "add"
    )
}
#' 
#' @export
#' @rdname Assignment
#' 
removeHauls <- function(acousticPSU, acousticLayer, Haul, projectPath, modelName, processID) {
    modifyAssignment(
        acousticPSU, 
        acousticLayer, 
        Haul, 
        projectPath, 
        modelName, 
        processID, 
        action = "remove"
    )
}
#' 
#' @export
#' @rdname Assignment
#' 
modifyAssignment <- function(acousticPSU, acousticLayer, Haul, projectPath, modelName, processID, action = c("add", "remove")) {
    
    # Check that the process returns Assigment process data:
    if(!checkDataType("Assignment", projectPath, modelName, processID)) {
        stop("The process ", getProcessName(projectPath, modelName, processID), " does not return Assignment data.")
    }
    
    # Get the process data of the process, a table of PSU, Layer, Haul and HaulWeight:
    Assignment <- getProcessData(projectPath, modelName, processID)$Assignment
    
    # Check for existing PSU:
    if(!acousticPSU %in% Assignment$PSU) {
        warning("The acoustic PSU with name ", acousticPSU, " does not exist. Please choose a different PSU name or add the PSU using DefineAcousticPSU.")
    }
    
    # Add the stations and update the assignment IDs:
    utilityFunctionName <- paste0("assignment_", action[1], "Stations")
    Assignment <- do.call(
        utilityFunctionName, 
        list(
            acousticPSU = acousticPSU, 
            acousticLayer = acousticLayer, 
            Haul = Haul, 
            Assignment = Assignment
        )
    )
    
    # Set the Assignment back to the process data of the process:
    setProcessMemory(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID, 
        argumentName = "processData", 
        argumentValue = list(list(Assignment = Assignment)) # We need to list this to make it correspond to the single value of the argumentName parameter.
    )
    
    # Return the assignments:
    Assignment
}
#' 
#' @export
#' @rdname Assignment
#' 
assignment_addStations <- function(acousticPSU, acousticLayer, Haul, Assignment) {
    
    # Add the hauls:
    toAdd <- data.table::data.table(
        PSU = acousticPSU, 
        Layer = acousticLayer, 
        Haul = Haul, 
        HaulWeight = 1
    )
    Assignment <- rbind(
        Assignment, 
        toAdd
    )
    
    # Return the assignments:
    Assignment
}
#' 
#' @export
#' @rdname Assignment
#' 
assignment_removeStations <- function(acousticPSU, acousticLayer, Haul, Assignment) {
    
    # Get the row indixces of the PSU and Layer:
    atPSU <- Assignment$PSU %in% acousticPSU
    atLayer <- Assignment$Layer  %in% acousticLayer
    at <- which(atPSU & atLayer)
    
    # Get the indices in 'at' to remove:
    atStations <- Assignment[at, ]$Haul %in% Haul
    
    # Remove the stations:
    Assignment <- Assignment[-at[atStations], ]
    
    # Return the assignments:
    Assignment
}




############################################################
############################################################
#' Add or remove biotic stations from assignments
#' 
#' The functions \code{addStations} and \code{removeStations} adds or removes biotic stations from the Assignment process data of the specified process.
#' 
#' @details 
#' The assignment IDs are refreshed for every change, after sorting the assignemnts by the PSU column.
#' 
#' @inheritParams getProcessOutput
#' @name AcousticPSU
#' 
NULL
#' 
#' @export
#' @rdname AcousticPSU
#' 
addAcousticPSU <- function(Stratum, PSU = NULL, projectPath, modelName, processID) {
    
    # Get the process data of the process, a table of PSU, Layer, AssignmentID, Haul and HaulWeight:
    AcosticPSU <- getProcessData(projectPath, modelName, processID)
    
    # If the acousticPSU ID is not given, set it to one larger than the largest integer:
    #acousticPSUNumeric <- 
    
    # Check that the acoustic PSU does not exist: 
    
    
    # Add the acsoutic PSU:
    
    toAdd <- data.table::data.table(
        Stratum = Stratum, 
        PSU = acousticPSU, 
        EDSU = NA
    )
    AcosticPSU <- data.table::data.table(
        AcosticPSU, 
        toAdd
    )
    
    AcosticPSU
}
#' 
#' @export
#' @rdname AcousticPSU
#' 
removeAcousticPSU <- function(PSU, projectPath, modelName, processID) {
    
    # Get the process data of the process, a table of PSU, Layer, AssignmentID, Haul and HaulWeight:
    AcosticPSU <- getProcessData(projectPath, modelName, processID)
    
    # Add the acsoutic PSU:
    toRemove <- AcosticPSU$PSU == acousticPSU
        
    AcosticPSU <- data.table::data.table(
        AcosticPSU, 
        toAdd
    )
    
    AcosticPSU
    
}
#' 
#' @export
#' @rdname AcousticPSU
#' 
renameAcousticPSU <- function(PSU, newPSUName, projectPath, modelName, processID) {
    
    # Get the process data of the process, a table of PSU, Layer, AssignmentID, Haul and HaulWeight:
    AcosticPSU <- getProcessData(projectPath, modelName, processID)
    
    # Add the acsoutic PSU:
    toRemove <- AcosticPSU$PSU == acousticPSU
    
    AcosticPSU <- data.table::data.table(
        AcosticPSU, 
        toAdd
    )
    
    AcosticPSU
    
}
#' 
#' @export
#' @rdname AcousticPSU
#' 
addEDSU <- function(PSU, EDSU, projectPath, modelName, processID) {
    
    
}
#' 
#' @export
#' @rdname AcousticPSU
#' 
removeEDSU <- function(acousticPSU, EDSU, projectPath, modelName, processID) {
    
    
}














createNode <- function(PSU, EDSU, projectPath, modelName, processID) {
    
}




# 
# # DefineStrata	
createNode <- function(projectPath, modelName, processID, StratumID, NewNode, NodeIndex1, NodeIndex2) {
    
}
deleteNode <- function(projectPath, modelName, processID, StratumID, NodeIndex) {
    
}
modifyNode <- function(projectPath, modelName, processID, StratumID, NewNode, NodeIndex) {
    
}
# 
# # DefineAcousticPSU	
# addAcousticPSU(Stratum, PSU), 
# removeAcousticPSU(Stratum, PSU), 
# addEDSU(PSU, EDSU), 
# removeEDSU(PSU, EDSU)
# 
# # DefineSweptAreaPSU	
# addSweptAreaPSU(Stratum), 
# addStation(Station, PSU), 
# removeStation(Station, PSU)
# 
