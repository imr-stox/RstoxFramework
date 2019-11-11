# StoX GUI will ask for interactive functions after every process has been run.

#' 
#' @export
#' 


# BioticStationAssignment	
addStations <- function(PSU, Layer, Stations, projectPath, modelName, processID) {
    
    # Get the process data of the process, a table of PSU, Layer, AssignmentID, Station and StationWeight:
    processData <- getProcessData(projectPath, modelName, processID)
    
    # Add the stations:
    processData <- rbind(
        processData, 
        data.table::data.table(
            PSU = PSU, 
            Layer = Layer, 
            AssignmentID = NA, 
            Stations = Stations, 
            StationWeight = 1
        )
    )
    
    # Get unique assignment IDs:
    # processData
    
    
    
    
}

# removeStations(Layers, PSUs, Stations)
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
