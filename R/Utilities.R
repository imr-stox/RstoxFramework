initiateRstox <- function(){
    
    # NMD and StoX defines different data types (StoX has the more general category "acoustic"):
    NMD_data_sources <- c(acoustic = "echosounder", biotic = "biotic", landing = "landing")
    # The implemented NMD APIs for the NMD_data_sources:
    NMD_API_versions <- list(
        biotic = c(1, 2, 3), 
        echosounder = 1, 
        reference = c(1, 2), 
        landing = NULL
    )
    
    # The format used by NMD for shapshot time:
    dateTimeNMDAPIFormat <- "%Y-%m-%dT%H.%M.%OSZ"
    
    # The current API and datasource formats:
    ver <- list(
        API = list(
            biotic = "3", 
            echosounder = "1", 
            reference = "2", 
            landing = NA
        ),
        reference = "2.0", 
        biotic = "3.0",
        echosounder = NA, 
        landing = NA
    )
    
    # Define project types:
    project_types <- c("AcousticTrawl", "SweptAreaLength", "SweptAreaTotal")
    
    # Define the process levels for the presicion estimate:
    processLevels <- c("bootstrap", "bootstrapImpute")
    
    # Define ECA covariates:
    ECACovariates <- data.frame(
        Name = c(
            "temporal", 
            "gearfactor", 
            "spatial", 
            "platformfactor"
        ), 
        Processe = c(
            "DefineTemporalLanding", 
            "DefineGearLanding", 
            "DefineSpatialLanding", 
            "DefinePlatformLanding"
        ), 
        Description = c(
            "The temporal covariate", 
            "The gear covariate given as groups of gear codes", 
            "The spatial covariate giving polygons or locations", 
            "The platform covariate (vessels)"
        ), 
        stringsAsFactors = FALSE
    )
    
    # Define the dependencies to include in the version names of the automated testing:
    internal_dependencies <- c("Reca")
    
    # Assign to RstoxEnv and return the definitions:
    Definitions <- list(
        StoX_data_sources = StoX_data_sources, 
        StoX_data_type_keys = StoX_data_type_keys, 
        StoX_reading_processes = StoX_reading_processes, 
        dateTimeNMDAPIFormat = dateTimeNMDAPIFormat, 
        NMD_data_sources = NMD_data_sources, 
        ver = ver, 
        project_types = project_types, 
        processLevels = processLevels, 
        ECACovariates = ECACovariates, 
        internal_dependencies = internal_dependencies
    )
    
    # Create the RstoxFrameworkEnv environment, holding definitions on folder structure and all the projects. This environment cna be accesses using RstoxFramework:::RstoxFrameworkEnv:
    utils::globalVariables("RstoxEnv")
    assign("RstoxEnv", new.env(), parent.env(environment()))
    
    assign("Definitions", Definitions, envir=get("RstoxEnv"))
    assign("Projects", list(), envir=get("RstoxEnv"))
    
    # Return the definitions:
    Definitions
}


initiateRstoxFramework <- function(){
    
    # The folders in a StoX project:
    StoXFolders <- c("input", "output", "process")
    StoX_data_sources <- c(echosounder = "acoustic", biotic = "biotic", landing = "landing")
    StoXFoldersRecursive <- list(
        input = file.path("input", StoX_data_sources), 
        output = file.path("output", outer(c("baseline", "r"), c("data", "report"), file.path)), 
        process = "process"
    ) # [NOTE] We should revise whether to restructure this to only one level, and hopefully the folders "baseline", "statistics" and "reports"
    StoXFoldersRecursiveSansOutput <- unlist(StoXFoldersRecursive[names(StoXFoldersRecursive) != "output"])
    
    # Assign to RstoxEnv and return the definitions:
    Definitions <- list(
        StoXFolders = StoXFolders, 
        StoXFoldersRecursive = StoXFoldersRecursive, 
        StoXFoldersRecursiveSansOutput = StoXFoldersRecursiveSansOutput
    )
    
    # Create the RstoxFrameworkEnv environment, holding definitions on folder structure and all the projects. This environment cna be accesses using RstoxFramework:::RstoxFrameworkEnv:
    utils::globalVariables("RstoxFrameworkEnv")
    assign("RstoxFrameworkEnv", new.env(), parent.env(environment()))
    
    assign("Definitions", Definitions, envir=get("RstoxFrameworkEnv"))
    assign("Projects", list(), envir=get("RstoxFrameworkEnv"))
   
    # Return the definitions:
    Definitions
}
