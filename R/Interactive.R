# 2019-11-12:

#1. Should we merge to one table per process data? This involves inserting a line with EDSU = NA for AcousticPSU, and then removing this line when #there are lines for the same PSU which are non-NA! Should we rather use a JSON object and define Stratum as a list of PSUs, and PSU as a list of #EDSUs. The write to output will create a table.
#
#2. Shoudl we introduce IDs for PSUs, Stratum and Layers?
#



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
#' @param acousticPSU   The acoustic primary sampling unit (PSU) for which to remove the station.
#' @param acousticLayer The acoustic Layer for which to remove the station.
#' @param Station       The biotic station to remove (can be a vector of stations).
#' 
#' @details 
#' The assignment IDs are refreshed for every change, after sorting the assignemnts by the PSU column.
#' 
#' @examples
#' # Create artificial assignment data:
#' Assignment <- data.table::data.table(
#'     PSUName = paste0("T", c(1,1, 2, 3,3)), 
#'     Layer = "L1", 
#'     Station = c(1,2, 2, 1,2), 
#'     AssignmentID = c(1,1, 2, 1,1)
#' )
#' 
#' # Add a station:
#' Assignment1 <- assignment_addStations("T3", "L1", 4, Assignment)
#' all.equal(Assignment, Assignment2)
#' 
#' # Remove the same station:
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
addStations <- function(acousticPSU, acousticLayer, Station, projectPath, modelName, processID) {
    modifyAssignment(
        acousticPSU, 
        acousticLayer, 
        Station, 
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
removeStations <- function(acousticPSU, acousticLayer, Station, projectPath, modelName, processID) {
    modifyAssignment(
        acousticPSU, 
        acousticLayer, 
        Station, 
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
modifyAssignment <- function(acousticPSU, acousticLayer, Station, projectPath, modelName, processID, action = c("add", "remove")) {
    
    # Check that the process returns Assigment process data:
    if(!checkDataType(projectPath, modelName, processID)) {
        stop("The process ", getProcessName(projectPath, modelName, processID), " does not return Assignment data.")
    }
    
    # Get the process data of the process, a table of PSU, Layer, AssignmentID, Station and StationWeight:
    Assignment <- getProcessData(projectPath, modelName, processID)
    
    # Check for existing PSUName:
    if(!acousticPSU %in% Assignment$PSUName) {
        warning("The acoustic PSU with name ", acousticPSU, " does not exist. Please choose a different PSU name or add the PSU using DefineAcousticPSU.")
    }
    
    # Add the stations and update the assignment IDs:
    utilityFunctionName <- paste0("assignment_", action[1], "Stations")
    Assignment <- do.call(
        utilityFunctionName, 
        list(
            acousticPSU = acousticPSU, 
            acousticLayer = acousticLayer, 
            Station = Station, 
            Assignment = Assignment
        )
    )
    
    # Set the Assignment back to the process data of the process:
    setProcessMemory(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID, 
        argumentName = "processData", 
        argumentValue = list(Assignment) # We need to list this to make it correspond to the single value of the argumentName parameter.
    )
    
    # Return the assignments:
    Assignment
}
#' 
#' @export
#' @rdname Assignment
#' 
assignment_addStations <- function(acousticPSU, acousticLayer, Station, Assignment) {
    browser()
    # Add the stations:
    toAdd <- data.table::data.table(
        PSU = acousticPSU, 
        Layer = acousticLayer, 
        AssignmentID = NA, 
        Station = Station, 
        StationWeight = 1
    )
    Assignment <- rbind(
        Assignment, 
        toAdd
    )
    
    # Check for duplicates (e.g. if the user added an existing station):
    dup <- duplicated(Assignment[, c("PSU", "Layer", "Station")])
    if(any(dup)) {
        Assignment <- Assignment[!dup, ] 
    }
    
    # Set unique assignment IDs and order by PSU:
    Assignment <- refreshAssignmentIDs(Assignment)
    
    # Return the assignments:
    Assignment
}
#' 
#' @export
#' @rdname Assignment
#' 
assignment_removeStations <- function(acousticPSU, acousticLayer, Station, Assignment) {
    
    # Get the row indixces of the PSU and Layer:
    atPSU <- Assignment$PSU %in% acousticPSU
    atLayer <- Assignment$Layer  %in% acousticLayer
    at <- which(atPSU & atLayer)
    
    # Get the indices in 'at' to remove:
    atStations <- Assignment[at, ]$Station %in% Station
    
    # Remove the stations:
    Assignment <- Assignment[-at[atStations], ]
    
    # Set unique assignment IDs:
    Assignment <- refreshAssignmentIDs(Assignment)
    
    # Return the assignments:
    Assignment
}

refreshAssignmentIDs <- function(Assignment) {
    # Group the stations per acosutic PSU, and get the unique combinations of stations:
    AssignmentIDs <- Assignment[, .(Station = list(Station)), by = PSU]
    uniqueAssignmentIDs <- unique(AssignmentIDs$Station)
    # Generate the assignmentIDs by matching the station groups by the unique station groups:
    atAssignmentIDs <- match(AssignmentIDs$Station, uniqueAssignmentIDs)
    names(atAssignmentIDs) <- AssignmentIDs$PSU
    # Set the assignment IDs:
    Assignment$AssignmentID <- atAssignmentIDs[Assignment$PSU]
    
    # Order by PSUs:
    Assignment <- orderAssignmentsByPSU(Assignment)
    # Return the assignments:
    Assignment
}

orderAssignmentsByPSU <- function(Assignment, ...) {
    Assignment[order(Assignment$PSU, ...), ]
}




BioticStationAssignment <- function(
    # Here EstLayers has been left out, as it is not clear how to incorporate the layers in the assignment.
    processData, 
    StratumPolygon, AcousticPSU, StoxBioticData, StoxAcousticData, 
    AssignmentMethod = c("Stratum", "Radius", "EllipsoidalDistance"), 
    Radius = 1, 
    MinNumStations = integer(), 
    RefGCDistance = integer(), 
    RefTime = integer(), 
    RefBotDepth = integer(), 
    RefLatitude	= integer(), 
    RefLongitude = integer(), 
    UseProcessData = FALSE) {
 
    message("Here EstLayers has been left out, as it is not clear how to incorporate the layers in the assignment.")
    
    # Locate stations inside the stratum polygons:
    if(AssignmentMethod == "Stratum") {
        # Create a spatial points object:
        #StationsSP <- SpatialPoints(StoxBioticData$Station[c("StartLatitude", "StartLongitude "), ]))
        
        # Get the stratum for each point:
        Stratum <- sp::over(dat, StratumPolygon)
        
        data.table::data.table(
            #acousticPSU = ********************
        )
        
        
    }
       
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
addAcousticPSU <- function(Stratum, PSUName = NULL, projectPath, modelName, processID) {
    
    # Get the process data of the process, a table of PSU, Layer, AssignmentID, Station and StationWeight:
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
removeAcousticPSU <- function(PSUName, projectPath, modelName, processID) {
    
    # Get the process data of the process, a table of PSU, Layer, AssignmentID, Station and StationWeight:
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
renameAcousticPSU <- function(PSUName, newPSUName, projectPath, modelName, processID) {
    
    # Get the process data of the process, a table of PSU, Layer, AssignmentID, Station and StationWeight:
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
addEDSU <- function(PSUName, EDSU, projectPath, modelName, processID) {
    
    
}
#' 
#' @export
#' @rdname AcousticPSU
#' 
removeEDSU <- function(acousticPSU, EDSU, projectPath, modelName, processID) {
    
    
}








## 
## # Create artificial assignment data:
## set.seed(0)
## AcosticPSU <- data.table::data.table(
##     Stratum = paste0("Stratum", sample(1:5, 15, replace = TRUE))
## )
## # Sort by PSU and add Station and StationWeight:
## AcosticPSU$PSU <- sequence(table(AcosticPSU$Stratum))
## AcosticPSU$EDSU <- seq_len(nrow(AcosticPSU))
## 
## # Add a station:
## Assignment1 <- assignment_addStations("T3", "L1", 4, Assignment)
## all.equal(Assignment, Assignment2)
## 
## # Remove the same station:
## Assignment2 <- assignment_removeStations("T3", "L1", 4, Assignment1)
## all.equal(Assignment, Assignment2)


createNode <- function(PSUName, EDSU, projectPath, modelName, processID) {
    
}




# 
# # DefineStrata	
# createNode(NewNode, Node1, Node2), 
# deleteNode(Node), 
# modifyNode(NewNode, Node)
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
