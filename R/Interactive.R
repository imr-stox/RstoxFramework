
# Decided after discussion with Åsmund on 2019-11-12:

# 1. We will strive for minimum redundancy in the process data. Process data will be saved as data tables with the possibility of variable length vectors in each cell, such as StratumID, StratumName and a vector of PSUs, and saved to the project.xml as JSON






#- Add formatProjectDescription() which converts process data to JSON strings (except Stratum, which is GEOJSON) using processData2JSON()
#- 
#



############################################################
############################################################
#' Add or remove biotic hauls from assignments
#' 
#' The functions \code{addHaulsToAssignment} and \code{removeHaulsFromAssignment} adds or removes biotic hauls from the Assignment process data of the specified process.
#' 
#' @param PSU   The acoustic primary sampling unit (PSU) for which to remove the haul
#' @param Layer The acoustic Layer for which to remove the haul
#' @param Haul       The biotic haul to remove (can be a vector of hauls).
#' 
#' @details 
#' The assignment IDs are refreshed for every change, after sorting the assignemnts by the PSU column.
#' 
#' @examples
#' ## # Create artificial assignment data:
#' ## Assignment <- data.table::data.table(
#' ##     PSU = paste0("T", c(1,1, 2, 3,3)), 
#' ##     Layer = "L1", 
#' ##     Haul = c(1,2, 2, 1,2), 
#' ##     AssignmentID = c(1,1, 2, 1,1)
#' ## )
#' ## 
#' ## # Add a haul:
#' ## Assignment1 <- assignment_addHauls("T3", "L1", 4, Assignment)
#' ## all.equal(Assignment, Assignment2)
#' ## 
#' ## # Remove the same haul:
#' ## Assignment2 <- assignment_removeHauls("T3", "L1", 4, Assignment1)
#' ## all.equal(Assignment, Assignment2)
#' 
#' @inheritParams getProcessOutput
#' @name Assignment
#' 
NULL
#' 
#' @export
#' @rdname Assignment
#' 
addHaulToAssignment <- function(PSU, Layer, Haul, projectPath, modelName, processID) {
    modifyAssignment(
        PSU, 
        Layer, 
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
removeHaulFromAssignment <- function(PSU, Layer, Haul, projectPath, modelName, processID) {
    modifyAssignment(
        PSU, 
        Layer, 
        Haul, 
        projectPath, 
        modelName, 
        processID, 
        action = "remove"
    )
}

# Generic function to add or remove a haul:
modifyAssignment <- function(PSU, Layer, Haul, projectPath, modelName, processID, action = c("add", "remove")) {
    
    # Check that the process returns Assigment process data:
    if(!checkDataType("Assignment", projectPath, modelName, processID)) {
        stop("The process ", getProcessName(projectPath, modelName, processID), " does not return Assignment data.")
    }
    
    # Get the process data of the process, a table of PSU, Layer, Haul and HaulWeight:
    Assignment <- getProcessData(projectPath, modelName, processID)$Assignment
    
    # Check for existing PSU:
    if(!PSU %in% Assignment$PSU) {
        warning("The acoustic PSU with name ", PSU, " does not exist. Please choose a different PSU name or add the PSU using DefineAcousticPSU.")
    }
    
    # Add the hauls and update the assignment IDs:
    utilityFunctionName <- paste0("assignment_", action[1], "Haul")
    Assignment <- do.call(
        utilityFunctionName, 
        list(
            PSU = PSU, 
            Layer = Layer, 
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

# Function that adds a haul to the assignment data:
assignment_addHaul <- function(PSU, Layer, Haul, Assignment) {
    
    # Add the hauls:
    toAdd <- data.table::data.table(
        PSU = PSU, 
        Layer = Layer, 
        Haul = Haul, 
        WeightingFactor = 1
    )
    Assignment <- rbind(
        Assignment, 
        toAdd
    )
    
    # Return the assignments:
    Assignment
}

# Function that removes a haul from the assignment data:
assignment_removeHaul <- function(PSU, Layer, Haul, Assignment) {
    
    # Get the row indixces of the PSU and Layer:
    atPSU <- Assignment$PSU %in% PSU
    atLayer <- Assignment$Layer  %in% Layer
    at <- which(atPSU & atLayer)
    
    # Get the indices in 'at' to remove:
    atHauls <- Assignment[at, ]$Haul %in% Haul
    
    # Remove the hauls:
    Assignment <- Assignment[-at[atHauls], ]
    
    # Return the assignments:
    Assignment
}




############################################################
############################################################
#' Add or remove biotic acoustic PSUs and EDSUs
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
    AcosticPSU <- getProcessData("AcosticPSU", projectPath, modelName, processID)
    
    # If the PSU is not given, use the default PSU name:
    if(length(PSU) == 0) {
        PSU <- getNewDefaultName(AcosticPSU$Stratum_PSU$PSU, getRstoxFrameworkDefinitions("process_Prefix"))
    }
    # Check that the acoustic PSU does not exist:
    if(any(AcosticPSU$Stratum_PSU$PSU == PSU)) {
        stop("The name of the Acoustic PSU (", ")")
    }
    
    
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
