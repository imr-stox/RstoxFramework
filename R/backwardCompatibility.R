# Backwards compatibility:

# 1. Re-organize models
# 	Baseline report and Report into report, R renamed to analysis

# 2. Distribute process data into processes
# 2.0. Convert process data to appropriate tables and sp objects
# 2.1. Copy stratumpolygon to all functions DefineStrata
# 2.2. Copy edsupsu and psustratum to all functions DefineAcousticPSU
# 2.3. Copy bioticassignment, suassignment and assignmentresolution to all functions BioStationAssignment and BioStationWeighting
# 2.4. Copy temporal to DefineTemporal, gearfactor to DefineGearFactor, spatial to DefineSpatial, platformfactor to DefinePlatform, ageerror to DefineAgeErrorMatrix and stratumneighbour to DefineStratumNeighbour, and treat covparam somehow, since this is a table that is appended by each of DefineTemporal, DefineGearFactor, DefineSpatial and DefinePlatform.


#
#Generate process data processes. DefineAccoustic
#
#
#- Removed function
#- Added function
#- Renamed function
#- Split function
#
#- Renamed parameter
#- Removed parameter
#- Added parameter
#
#
#


# 1. Remove processes:
# 1.1. Remove ReadProcessData
# 2.2. Remove WriteProcessData
# 
# 2. Copy process data into processes:
# 2.0. Convert process data to appropriate tables and sp objects
# 2.1. Copy stratumpolygon to all functions DefineStrata
# 2.2. Copy edsupsu and psustratum to a data.table for all functions DefineAcousticPSU
# 2.3. Copy bioticassignment to all functions BioStationAssignment and BioStationWeighting
# 2.4. Copy stratumpolygon to all functions DefineStrata
# 2.5. Copy stratumpolygon to all functions DefineStrata
# 2.6. Copy stratumpolygon to all functions DefineStrata
# 
# 
# - Split ReadBioticXML into RstoxData::ReadBiotic and RstoxData::StoxBiotic
# - Split ReadAcousticXML into RstoxData::ReadAcoustic and RstoxData::StoxAcoustic
# - Split ReadLandingXML into RstoxData::ReadLanding and RstoxData::StoxLanding
# 
# - Convert FilterBiotic to RstoxData::AddToStoxBiotic with all variables detected in the FilterBiotic and RstoxData::FilterStoxBiotic # with a filter expression built based on the Java JEXL expression of FilterBiotic
# - Convert FilterAcoustic to RstoxData::AddToAcoustic with all variables detected in the FilterAcoustic and RstoxData::FilterStoxAcou# stic with a filter expression built based on the Java JEXL expression of FilterAcoustic
# - Convert FilterLanding to RstoxData::FilterStoxLanding with a filter expression built based on the Java JEXL expression of FilterLan# ding
# 
# - Read biostationassignment global process data and map to BioticAssignment process data
# 
# - Read edsupsu and psustratum global process data and map to AcousticPSU process data
# 
# - Split AcousticDensity into RstoxBase::DefineAcousticTargetStrength and RstoxBase::AcousticDensity
# 
# - Replace StationLengthDist with RstoxBase::LengthDistribution
# 
# - Replace RegroupLengthDist with RstoxBase::RegroupLengthDistribution
# 
# - Replace DefineStrata with RstoxBase::DefineStratumPolygon
# 
# - Replace BioStationAssignment with RstoxBase::DefineBioticAssignment
# 
# - Replace BioStationWeighting with RstoxBase::BioticAssignmentWeighting
# 
# - Replace TotalLengthDist with RstoxBase::MeanLengthDistribution in swept-area models and RstoxBase::AssignmentLengthDistribution
# 
# - Delete SumDensity
# 
# - Replace IndividualDataStations and IndividualData with RstoxBase::Individuals
# 
# - Replace SuperIndAbundance with RstoxBase::SuperIndividuals
# 
# - Move imputeByAge to RstoxBase::ImputeSuperIndividuals
# 
# - Replace runBootstrap with RstoxFramework::Bootstrap
# 
# 
# 
# Backwards compatibility actions:
#     
# - Delete process
# - Replace process by new process
# - Replace process by several process


readProjectXMLToList <- function(projectPath) {
    projectXMLFile <- getProjectPaths(projectPath, "projectXMLFile")
    # Read the project.xml file into a list:
    doc = XML::xmlParse(projectXMLFile)
    projectList <- XML::xmlToList(doc)
    return(projectList)
}


readProjectXMLToProjectDescription2.7 <- function(projectPath) {
    # Read the project.xml file into a list:
    projectList <- readProjectXMLToList(projectPath)
    
    # For convenience separate into models, processdata and attrs:
    models <- projectList[names(projectList) == "model"]
    processdata <- projectList$processdata
    attrs <- projectList$.attrs
    
    # Extract project attributes:
    list(
        TimeSaved = strftime(as.POSIXlt(attrs[["lastmodified"]], "UTC", "%Y/%m/%d %H:%M") , "%Y-%m-%dT%H:%M:%OS3Z"), 
        FileVersion = "", 
        RVersion = attrs[["rversion"]], 
        RstoxFrameworkVersion = "",
        RstoxFrameworkDependencies = data.table::data.table()
        #Template = attrs[["template"]]
    )
    
    # Get the model names and group baseline
    modelNames2.7 <- sapply(models, function(x) x$.attrs[["name"]])
    names(models) <- modelNames2.7
    
    # Create new models:
    modelNameMapping2.7To3 <- getRstoxFrameworkDefinitions("modelNameMapping2.7To3")
    models <- split(models, modelNameMapping2.7To3[names(models)])
    # Order the models:
    models <- models[getRstoxFrameworkDefinitions("stoxModelNames")]
    
    
}


getProjectAttributes <- function(FileVersion = "", Template = "") {
    list(
        TimeSaved = strftime(as.POSIXlt(Sys.time(), "UTC", "%Y-%m-%dT%H:%M:%S") , "%Y-%m-%dT%H:%M:%OS3Z"), 
        FileVersion = FileVersion, 
        RVersion = R.version.string, 
        RstoxFrameworkVersion = as.character(utils::packageVersion("RstoxFramework")),
        RstoxFrameworkDependencies = getDependenciesWithVersion("RstoxFramework", dependencies = c("Depends", "Imports", "LinkingTo")), 
        Template = Template
    )
}




getDependenciesWithVersion <- function(packageName, dependencies = c("Depends", "Imports", "LinkingTo")) {
    dep <- gtools::getDependencies(packageName, dependencies = dependencies)
    ver <- lapply(dep, packageVersion)
    data.table::data.table(dep = dep, ver = sapply(ver, as.character))
}







# Rename DefineStratumPolygon to DefineStratum:
#rename_DefineStratumPolygon_to_DefineStratum <- function(projectDescription) {
#    if(StoxVersion == 2.7) {
#        # Get the function names:
#        functionNames <- sapply(projectDescription$baseline, "[[", "functionName")
#        # Get the position of the process using DefineStratumPolygon():
#        atDefineStratumPolygon <- which("DefineStratumPolygon" %in% functionNames)
#        if(length(atDefineStratumPolygon)) {
#            # Split the process:
#            projectDescription$baseline[[atDefineStratumPolygon]]$functionName <- "DefineStratum"
#        }
#    }
#    
#    return(projectDescription)
#}


checkResourceVersion1.92 <- function(projectDescription) {
    resourceVersion <- attr(projectDescription, "resourceversion")
    if(!length(resourceVersion) || (length(resourceVersion) && resourceVersion != "1.92")) {
        stop("StoX: Backwards compatibility only supported for StoX 2.7 projects")
    }
}


# Split ReadBioticXML into ReadBiotic and StoxBiotic:
split_ReadBioticXML_to_ReadBiotic_and_StoxBiotic <- function(projectDescription) {
    
    # Do not support backwards compatibility for versoins prior to StoX 2.7:
    checkResourceVersion1.92(projectDescription)
    
    
    
    # Get the function names:
    functionNames <- sapply(projectDescription$baseline, "[[", "functionName")
    
    # Get the positions of all processes using ReadBioticXML():
    atReadBioticXML <- which("ReadBioticXML" %in% functionNames)
    
    # Apply the conversion:
    for(at in atReadBioticXML) {
        projectDescription <- split_ReadBioticXML_to_ReadBiotic_and_StoxBioticOne(
            projectDescription, 
            atReadBioticXML = at
        )
    }
    
    
    split_ReadBioticXML_to_ReadBiotic_and_StoxBioticOne <- function(projectDescription, atReadBioticXML) {
        # Trick to make it possible to split the process:
        projectDescription$baseline <- lapply(projectDescription$baseline, list)
        
        # Split the process:
        projectDescription$baseline[[atReadBioticXML]] <- list(
            projectDescription$baseline[[atReadBioticXML]], 
            projectDescription$baseline[[atReadBioticXML]]
        )
        
        # ... into ReadBiotic:
        projectDescription$baseline[[atReadBioticXML]]$processName = "ReadBiotic"
        projectDescription$baseline[[atReadBioticXML]]$functionName = "RstoxBase::ReadBiotic"
        projectDescription$baseline[[atReadBioticXML]]$functionParameters = list(
            FileNames = unlist(projectDescription$baseline[[atReadBioticXML]]$functionParameters)
        )
        
        # ... and StoxBiotic:
        projectDescription$baseline[[atReadBioticXML]]$processName = "StoxBiotic"
        projectDescription$baseline[[atReadBioticXML]]$functionName = "RstoxBase::StoxBiotic"
        projectDescription$baseline[[atReadBioticXML]]$functionParameters$SpeciesCategory = "commonname"
        projectDescription$baseline[[atReadBioticXML]]$functionInputs$BioticData = "ReadBiotic"
        
        # Flatten the list again:
        projectDescription$baseline <- unlist(projectDescription$baseline, recursive = TRUE)
    }
    
    
    
    
    
    
    
    if(length(atReadBioticXML)) {
        # Trick to make it possible to split the process:
        projectDescription$baseline <- lapply(projectDescription$baseline, list)
        
        # Split the process:
        projectDescription$baseline[[atReadBioticXML]] <- list(
            projectDescription$baseline[[atReadBioticXML]], 
            projectDescription$baseline[[atReadBioticXML]]
        )
        
        # ... into ReadBiotic:
        projectDescription$baseline[[atReadBioticXML]]$processName = "ReadBiotic"
        projectDescription$baseline[[atReadBioticXML]]$functionName = "RstoxBase::ReadBiotic"
        projectDescription$baseline[[atReadBioticXML]]$functionParameters = list(
            FileNames = unlist(projectDescription$baseline[[atReadBioticXML]]$functionParameters)
        )
        
        # ... and StoxBiotic:
        projectDescription$baseline[[atReadBioticXML]]$processName = "StoxBiotic"
        projectDescription$baseline[[atReadBioticXML]]$functionName = "RstoxBase::StoxBiotic"
        projectDescription$baseline[[atReadBioticXML]]$functionParameters$SpeciesCategory = "commonname"
        projectDescription$baseline[[atReadBioticXML]]$functionInputs$BioticData = "ReadBiotic"
        
        # Flatten the list again:
        projectDescription$baseline <- unlist(projectDescription$baseline, recursive = TRUE)
    }

    return(projectDescription)
}

remove_ReadProcessData <- function(projectDescription) {
    # Get the StoxVersion from the attributes:
    resourceVersion <- attr(projectDescription, "resourceversion")
    
    if(length(resourceVersion) && resourceVersion == "1.92") {
        # Get the function names:
        functionNames <- sapply(projectDescription$baseline, "[[", "functionName")
        # Get the position of the process using ReadBioticXML():
        atReadProcessData <- which("ReadProcessData" %in% functionNames)
        if(length(atReadProcessData)) {
            
            # Remove the process:
            projectDescription$baseline[[atReadProcessData]] <- NULL
        }
    }
    
    return(projectDescription)
}


modifyFilterBiotic <- function(projectDescription) {
    
    # Get the StoxVersion from the attributes:
    resourceVersion <- attr(projectDescription, "resourceversion")
    
    # Run only for StoX 2.7:
    if(length(resourceVersion) && resourceVersion == "1.92") {
        # Find the process using DefineStratumPolygon():
        atProcess <- findProcessFromFunctionName(
            functionName = "FilterBiotic", 
            projectDescription =projectDescription, 
            modelName = "baseline"
        )
        if(length(atProcess)) {
            # Get the filters:
            filterNames <- c("FishStationExpr", "CatchExpr", "SampleExpr", "IndExpr")
            
            # Get the filters:
            FilterExpression <- lapply(filterNames, function(x) projectDescription$baseline[[atProcess]][[x]])
            
            # Convert to R syntax:s
            FilterExpression <- lapply(FilterExpression, JavaJEXL2R)
            
            
            #FilterExpression <- list(
            #    fishstation = projectDescription$baseline[[atProcess]]$
            #)
            
            
            # Backwards compatibility must happen in a separate package, maybe RstoxAPI, or possibly in RstoxFramework!
            
            # General conversions:
            # 1. Get function inputs, by a list of data types in old StoX, except process data, which must be inserted from the last process returning the process data specified in the function inputs of the new function (moev from global to local process data).
            # 2. Get function parameters, and use individual backwards compatibility functions to map to the new parameters.
            # 3. Get function name
            # 4. Get process name
            # 5. Get process parameters
            # 6. Get process data
            
            
            
            
            
            #FishStationExpr	fs.getLengthSampleCount('SILDG03') > 9
            #CatchExpr	species == '161722.G03'
            #SampleExpr	
            #IndExpr	
            #
            #
            #JavaJEXL2R
            
                
        }
    }
    
    return(projectDescription)
}


modify_DefineStratumPolygon <- function(projectDescription) {
    
    # Get the StoxVersion from the attributes:
    resourceVersion <- attr(projectDescription, "resourceversion")
    
    # Run only for StoX 2.7:
    if(length(resourceVersion) && resourceVersion == "1.92") {
        # Find the process using DefineStratumPolygon():
        atProcess <- findProcessFromFunctionName(
            functionName = "DefineStratumPolygon", 
            projectDescription =projectDescription, 
            modelName = "baseline"
        )
        if(length(atProcess)) {
            # Get the stratum multypolygon WKT table, and ocnvert to SpatialPolygonsDataFrame, and then to JSON
            stratumpolygon_WKT <- projectDescription$processdata$stratumpolygon
            stratumpolygon_sp <- dataTable2SpatialPolygonsDataFrame(stratumpolygon_WKT)
            # Add the SpatialPolygonsDataFrame to the process data of the process:
            projectDescription$baseline[[atProcess]]$processData <- stratumpolygon_sp
        }
    }
    
    return(projectDescription)
}

findProcessFromFunctionName <- function(functionName, projectDescription, modelName = "baseline") {
    # Get the function names:
    functionNames <- sapply(projectDescription[[modelName]], "[[", "functionName")
    # Get the position of the process:
    atProcess <- which(functionName %in% functionNames)
    return(atProcess)
}






# A list of functions performing conversions of the projectDescription to ensure backward compatibility:
#' 
#' @export
#' 
backwardCompatibility <- list(
    # rename_DefineStratumPolygon_to_DefineStratum, 
    split_ReadBioticXML_to_ReadBiotic_and_StoxBiotic
)






stratumpolygon2.7ToTable <- function(stratumpolygon) {
    # Get polygon keys:
    polygonkey <- sapply(stratumpolygon, function(x) x$.attrs["polygonkey"])
    
    # Convert to a list with one list per polygon:
    stratumpolygonList <- split(stratumpolygon, polygonkey)
    # ... and extract the includeintotal and polygon:
    stratumpolygonList <- lapply(stratumpolygonList, function(x) lapply(x, function(y) y$text))
    
    # Rbind to a data.table and add names:
    stratumpolygonTable <- data.table::rbindlist(stratumpolygonList)
    stratumpolygonTable <- cbind(names(stratumpolygonList), stratumpolygonTable)
    names(stratumpolygonTable) <- c("polygonkey", "includeintotal", "polygon")
    
    return(stratumpolygonTable)
}


saveStoXMultipolygonWKT <- function(stratumpolygonTable, projectPath, stratumPolygonFileName = "stratumPolygon.txt") {
    stratumpolygonFilePath <- file.path(
        getProjectPaths(projectPath, "Input"), 
        stratumPolygonFileName
    )
    
    data.table::fwrite(
        stratumpolygonTable[, c("polygonkey", "polygon")], 
        stratumpolygonFilePath, 
        col.names = FALSE
    )
    
    return(stratumpolygonFilePath)
}


copyStoXMultipolygonWKTFrom2.7 <- function(projectPath, stratumPolygonFileName = "stratumPolygon.txt") {
    
    # Read the project.xml file into a list:
    projectList <- readProjectXMLToList(projectPath)
    
    # Convert the stratumpolygon to a table:
    stratumpolygonTable <- stratumpolygon2.7ToTable(projectList$processdata$stratumpolygon)
    # ... and write to file:
    stratumpolygonFilePath <- file.path(
        getProjectPaths(projectPath, "Input"), 
        stratumPolygonFileName
    )
    data.table::fwrite(
        stratumpolygonTable[, c("polygonkey", "polygon")], 
        stratumpolygonFilePath, 
        col.names = FALSE, 
        sep = "\t"
    )
    
    return(stratumpolygonFilePath)
}



