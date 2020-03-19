
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
    checkDataType("Assignment", projectPath, modelName, processID)
    
    # Get the process data of the process, a table of PSU, Layer, Haul and HaulWeight:
    Assignment <- getProcessData(projectPath, modelName, processID)$Assignment
    
    # Check for existing PSU:
    if(!PSU %in% Assignment$PSU) {
        warning("The acoustic PSU with name ", PSU, " does not exist. Please choose a different PSU name or add the PSU using DefineAcousticPSU.")
    }
    
    # Add the hauls:
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
    
    # Revert the active process ID to the previous process:
    resetModel(projectPath, modelName, processID = processID, modified = TRUE)
    
    # Return the active process:
    activeProcess <- getActiveProcess(projectPath, modelName = modelName)
    return(list(activeProcess = activeProcess))
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
    
    # Revert the active process ID to the previous process:
    resetModel(projectPath, modelName, processID = processID, modified = TRUE)
    
    # Return the active process:
    activeProcess <- getActiveProcess(projectPath, modelName = modelName)
    return(list(activeProcess = activeProcess))
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
    
    # Revert the active process ID to the previous process:
    resetModel(projectPath, modelName, processID = processID, modified = TRUE)
    
    # Return the active process:
    activeProcess <- getActiveProcess(projectPath, modelName = modelName)
    return(list(activeProcess = activeProcess))
}




############################################################
############################################################
#' Add or remove acoustic PSUs and EDSUs
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
    
    # Check that the process returns Assigment process data:
    checkDataType("AcousticPSU", projectPath, modelName, processID)
    
    # Get the process data of the process, a table of PSU, Layer, AssignmentID, Haul and HaulWeight:
    AcousticPSU <- getProcessData(projectPath, modelName, processID)
    
    # If the PSU is not given, use the default PSU name:
    if(length(PSU) == 0) {
        PSU <- getNewDefaultName(AcousticPSU$Stratum_PSU$PSU, prefix = RstoxBase::getRstoxBaseDefinitions("AcousticPSUPrefix"))
    }
    # Check whether the acoustic PSU already exists:
    if(any(AcousticPSU$Stratum_PSU$PSU == PSU)) {
        stop("The name of the Acoustic PSU (", PSU, ") already exists.")
    }
    
    # Add the acsoutic PSU:
    toAdd <- data.table::data.table(
        Stratum = Stratum, 
        PSU = PSU
    )
    AcousticPSU$Stratum_PSU <- rbind(
        AcousticPSU$Stratum_PSU, 
        toAdd
    )
    
    # Set the Assignment back to the process data of the process:
    setProcessMemory(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID, 
        argumentName = "processData", 
        argumentValue = list(AcousticPSU) # We need to list this to make it correspond to the single value of the argumentName parameter.
    )
    
    # Revert the active process ID to the previous process:
    resetModel(projectPath, modelName, processID = processID, modified = TRUE)
    
    # Return the active process:
    activeProcess <- getActiveProcess(projectPath, modelName = modelName)
    return(
        list(
            activeProcess = activeProcess, 
            PSU = PSU
        )
    )
}
#' 
#' @export
#' @rdname AcousticPSU
#' 
removeAcousticPSU <- function(PSU, projectPath, modelName, processID) {
    
    # Get the process data of the process, a table of PSU, Layer, AssignmentID, Haul and HaulWeight:
    AcousticPSU <- getProcessData(projectPath, modelName, processID)
    
    # Add the acsoutic PSU:
    PSUsToKeep <- !AcousticPSU$Stratum_PSU$PSU %in% PSU
    EDSUsToKeep <- !AcousticPSU$EDSU_PSU$PSU %in% PSU
    
    AcousticPSU$Stratum_PSU <- AcousticPSU$Stratum_PSU[PSUsToKeep, ]
    AcousticPSU$EDSU_PSU <- AcousticPSU$EDSU_PSU[EDSUsToKeep, ]
    
    # Set the Assignment back to the process data of the process:
    setProcessMemory(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID, 
        argumentName = "processData", 
        argumentValue = list(AcousticPSU) # We need to list this to make it correspond to the single value of the argumentName parameter.
    )
    
    # Revert the active process ID to the previous process:
    resetModel(projectPath, modelName, processID = processID, modified = TRUE)
    
    # Return the active process:
    activeProcess <- getActiveProcess(projectPath, modelName = modelName)
    return(list(activeProcess = activeProcess))
}
#' 
#' @export
#' @rdname AcousticPSU
#' 
renameAcousticPSU <- function(PSU, newPSUName, projectPath, modelName, processID) {
    
    # Get the process data of the process, a table of PSU, Layer, AssignmentID, Haul and HaulWeight:
    AcousticPSU <- getProcessData(projectPath, modelName, processID)
    
    # Add the acsoutic PSU:
    PSUsToRename <- AcousticPSU$Stratum_PSU$PSU %in% PSU
    EDSUsToRename <- AcousticPSU$EDSU_PSU$PSU %in% PSU
    
    AcousticPSU$Stratum_PSU$PSU[PSUsToRename] <- newPSUName
    AcousticPSU$EDSU_PSU$PSU[EDSUsToRename] <- newPSUName
    
    # Set the Assignment back to the process data of the process:
    setProcessMemory(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID, 
        argumentName = "processData", 
        argumentValue = list(AcousticPSU) # We need to list this to make it correspond to the single value of the argumentName parameter.
    )
    
    # Revert the active process ID to the previous process:
    resetModel(projectPath, modelName, processID = processID, modified = TRUE)
    
    # Return the active process:
    activeProcess <- getActiveProcess(projectPath, modelName = modelName)
    return(
        list(
            activeProcess = activeProcess, 
            PSU = PSU
        )
    )
}
#' 
#' @export
#' @rdname AcousticPSU
#' 
addEDSU <- function(PSU, EDSU, projectPath, modelName, processID) {
    
    # Get the process data of the process, a table of PSU, Layer, AssignmentID, Haul and HaulWeight:
    AcousticPSU <- getProcessData(projectPath, modelName, processID)
    
    # Set the PSU column for the given EDSUs:
    atEDSUs <- AcousticPSU$EDSU_PSU$EDSU %in% EDSU
    AcousticPSU$EDSU_PSU[atEDSUs, PSU := ..PSU]
    
    ## Add the acsoutic PSU:
    #toAdd <- data.table::data.table(
    #    PSU = PSU, 
    #    EDSU = EDSU
    #)
    #AcousticPSU$PSU_EDSU <- data.table::data.table(
    #    AcousticPSU$PSU_EDSU, 
    #    toAdd
    #)
    
    # Set the Assignment back to the process data of the process:
    setProcessMemory(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID, 
        argumentName = "processData", 
        argumentValue = list(AcousticPSU) # We need to list this to make it correspond to the single value of the argumentName parameter.
    )
    
    # Revert the active process ID to the previous process:
    resetModel(projectPath, modelName, processID = processID, modified = TRUE)
    
    # Return the active process:
    activeProcess <- getActiveProcess(projectPath, modelName = modelName)
    return(list(activeProcess = activeProcess))
}
#' 
#' @export
#' @rdname AcousticPSU
#' 
removeEDSU <- function(EDSU, projectPath, modelName, processID) {
    
    # Get the process data of the process, a table of PSU, Layer, AssignmentID, Haul and HaulWeight:
    AcousticPSU <- getProcessData(projectPath, modelName, processID)
    
    # Set the PSU column to empty string for the given EDSUs:
    atEDSUs <- AcousticPSU$EDSU_PSU$EDSU %in% EDSU
    AcousticPSU$EDSU_PSU[atEDSUs, PSU := ""]
    
    ## Add the acsoutic PSU:
    #EDSUsToKeep <- !AcousticPSU$PSU_EDSU$EDSU %in% EDSU
    #
    #AcousticPSU$PSU_EDSU <- AcousticPSU$PSU_EDSU[EDSUsToKeep, ]
    
    # Set the Assignment back to the process data of the process:
    setProcessMemory(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID, 
        argumentName = "processData", 
        argumentValue = list(AcousticPSU) # We need to list this to make it correspond to the single value of the argumentName parameter.
    )
    
    # Revert the active process ID to the previous process:
    resetModel(projectPath, modelName, processID = processID, modified = TRUE)
    
    # Return the active process:
    activeProcess <- getActiveProcess(projectPath, modelName = modelName)
    return(list(activeProcess = activeProcess))
}






############################################################
############################################################
#' Stratum manipulation
#' 
#' The functions \code{addStations} and \code{removeStations} adds or removes biotic stations from the Assignment process data of the specified process.
#' 
#' @details 
#' The assignment IDs are refreshed for every change, after sorting the assignemnts by the PSU column.
#' 
#' @inheritParams getProcessOutput
#' @name Stratum
#' 
NULL
#' 
#' @export
#' @rdname Stratum
#' 
addStratum <- function(stratum, projectPath, modelName, processID) {
    
    # Get the process data of the process, a table of PSU, Layer, AssignmentID, Haul and HaulWeight:
    StratumPolygon <- getProcessData(projectPath, modelName, processID)
    
    # If given as a GeoJSON string, parse to a SpatialPolygonsDataFrame object:
    if(is.character(stratum)) {
        #stratum <- geojsonio::geojson_sp(geojsonio::as.json(stratum))
        stratum <- rgdal::readOGR(stratum, stringsAsFactors = FALSE)
        stratum <- copyPolygonNameToID(stratum)
        # Add "x", "y" as column names of the coords, since readOGR() does not do this:
        stratum <- addCoordsNames(stratum)
        # added by aasmund: Set projection to empty by default, rbind will then work.
        stratum@proj4string <- sp::CRS()
    }
    
    # Add the new strata, but check that the stratum names are not in use:
    usedStratumNames <- intersect(
        RstoxBase::getStratumNames(stratum), 
        RstoxBase::getStratumNames(StratumPolygon$StratumPolygon)
    )
    if(length(usedStratumNames)) {
        stop("The stratum name ", usedStratumNames, " already exist. Choose a different name")
    }
    if(length(RstoxBase::getStratumNames(stratum)) == 0) {
        stop("The new stratum must have a name")
    }
    
    #toAdd <- list(Polygons(list(Polygon(coordinates)), ID = stratumName))
    #StratumPolygon$StratumPolygon@polygons <- c(
    #    StratumPolygon$StratumPolygon@polygons, 
    #    stratum@polygons
    #)
    StratumPolygon$StratumPolygon <- rbind(
        StratumPolygon$StratumPolygon, 
        stratum
    )
    
    # Set the Assignment back to the process data of the process:
    setProcessMemory(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID, 
        argumentName = "processData", 
        argumentValue = list(StratumPolygon) # We need to list this to make it correspond to the single value of the argumentName parameter.
    )
    
    # Revert the active process ID to the previous process:
    resetModel(projectPath, modelName, processID = processID, modified = TRUE)
    
    # Return the active process:
    activeProcess <- getActiveProcess(projectPath, modelName = modelName)
    return(list(activeProcess = activeProcess))
}
#' 
#' @export
#' @rdname Stratum
#' 
removeStratum <- function(stratumName, projectPath, modelName, processID) {
    
    # Get the process data of the process, a table of PSU, Layer, AssignmentID, Haul and HaulWeight:
    StratumPolygon <- getProcessData(projectPath, modelName, processID)
    
    # Add the coordinates:
    # Modify the coordinates:
    atModify <- match( 
        stratumName, 
        RstoxBase::getStratumNames(StratumPolygon$StratumPolygon)
    )
    if(!any(is.na(atRemove))) {
        StratumPolygon$StratumPolygon@polygons <- StratumPolygon$StratumPolygon@polygons[-atRemove]
    }
    
    # Set the Assignment back to the process data of the process:
    setProcessMemory(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID, 
        argumentName = "processData", 
        argumentValue = list(StratumPolygon) # We need to list this to make it correspond to the single value of the argumentName parameter.
    )
    
    # Revert the active process ID to the previous process:
    resetModel(projectPath, modelName, processID = processID, modified = TRUE)
    
    # Return the active process:
    activeProcess <- getActiveProcess(projectPath, modelName = modelName)
    return(list(activeProcess = activeProcess))
}
#' 
#' @export
#' @rdname Stratum
#' 
modifyStratum <- function(stratum, projectPath, modelName, processID) {
    
    # Get the process data of the process, a table of PSU, Layer, AssignmentID, Haul and HaulWeight:
    StratumPolygon <- getProcessData(projectPath, modelName, processID)
    
    # If given as a GeoJSON string, parse to a SpatialPolygonsDataFrame object:
    if(is.character(stratum)) {
        #stratum <- geojsonio::geojson_sp(geojsonio::as.json(stratum))
        stratum <- rgdal::readOGR(stratum)
        # Add "x", "y" as column names of the coords, since readOGR() does not do this:
        stratum <- addCoordsNames(stratum)
    }
    
    # Modify the coordinates:
    atModify <- match( 
        RstoxBase::getStratumNames(stratum), 
        RstoxBase::getStratumNames(StratumPolygon$StratumPolygon)
    )
    if(length(atModify)) {
    #if(!any(is.na(atModify))) {
            StratumPolygon$StratumPolygon@polygons[atModify] <- stratum@polygons
    }
    
    # Set the Assignment back to the process data of the process:
    setProcessMemory(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID, 
        argumentName = "processData", 
        argumentValue = list(StratumPolygon) # We need to list this to make it correspond to the single value of the argumentName parameter.
    )
    
    # Revert the active process ID to the previous process:
    resetModel(projectPath, modelName, processID = processID, modified = TRUE)
    
    # Return the active process:
    activeProcess <- getActiveProcess(projectPath, modelName = modelName)
    return(list(activeProcess = activeProcess))
}

# Function to add colnames to the coords slot of a SpatialPolygonsDataFrame:
addCoordsNames <- function(stratum, names = c("x", "y")) {
    # Hack to change the names of the coords to "x" and "y":
    for(i in seq_along(stratum@polygons)) {
        for(j in seq_along(stratum@polygons[[i]]@Polygons)) {
            colnames(stratum@polygons[[i]]@Polygons[[j]]@coords) <- names
        }
    }
    
    return(stratum)
}

# Function to rename the IDs to the column polygonNames of a SpatialPolygonsDataFrame:
copyPolygonNameToID <- function(stratum) {
    
    # Get the polygon names and IDs:
    polygonName <- stratum$polygonName
    
    # Rename all IDs to the polygon names:
    for (ind in seq_along(stratum@polygons)) {
        stratum@polygons[[ind]]@ID <- polygonName[ind]
    }
    
    # Update rownames of the data slot:
    rownames(slot(stratum, "data")) <- polygonName
    
    return(stratum)
}

# Function to rename the IDs to the column polygonNames of a SpatialPolygonsDataFrame:
setEmptyID <- function(stratum) {
    
    # Get the polygon names and IDs:
    polygonName <- stratum$polygonName
    
    # Rename all IDs to the polygon names:
    for (ind in seq_along(stratum@polygons)) {
        stratum@polygons[[ind]]@ID <- ""
    }
    
    return(stratum)
}

