




initiateRstoxEnv <- function(){
    
# Function used for extracting defaults and possible values of functions:
getDefaultsAndPredefinedValues <- function(data){
        
        getDefaultsAndPredefinedValuesOne <- function(data){
            fun_formals <- formals(data$Name)
            # Remove the first the projectName:
            fun_formals <- fun_formals[names(fun_formals) != "projectName"]
            # Keep only the specified arguments:
            fun_formals <- subset(fun_formals, names(fun_formals) %in% data$Parameters$Name)
            # Evaluate the arguments:
            fun_formalsEval <- lapply(fun_formals, eval)
            
            
            data$Parameters$DataType <- sapply(fun_formalsEval, function(x) if(is.numeric(x) && x==round(x)) "Integer" else if(is.numeric(x) && x!=round(x)) "Double" else "String")
            
            # Convert to string, by converting numeric and character using as.character() and all other using deparse():
            data$Parameters$Values <- lapply(fun_formalsEval, function(x) if(is.numeric(x) || is.character(x)) as.character(x) else deparse(x))
            # Assume that all the parameters of functions passed to StoX have length 1, and thus that if there are more than one value given as default, these are the possible values, and the first of these is the true default.
            data$Parameters$DefaultValue <- sapply(data$Parameters$Values, function(x) head(x, 1))
            
            data
        }
        
        lapply(data, getDefaultsAndPredefinedValuesOne)
    }
    
    # Create a Rstox environment in which the baseline objects of the various projects are placed. This allows for a check for previously run baseline models and avoids memory leakage:", 
    assign("RstoxEnv", new.env(), envir=.GlobalEnv)
    
    ##### Define fundamental variables Rstox: #####
    # The folders in a StoX project:
    StoXFolders <- c("input", "output", "process")
    StoX_data_sources <- c(echosounder = "acoustic", biotic = "biotic", landing = "landing")
    StoXFoldersRecursive <- list(
        input = file.path("input", StoX_data_sources), 
        output = file.path("output", outer(c("baseline", "r"), c("data", "report"), file.path)), 
        process = "process"
    )
    StoXFoldersRecursiveSansOutput <- unlist(StoXFoldersRecursive[names(StoXFoldersRecursive) != "output"])
    
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
        StoXFolders = StoXFolders, 
        StoXFoldersRecursive = StoXFoldersRecursive, 
        StoXFoldersRecursiveSansOutput = StoXFoldersRecursiveSansOutput, 
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
    assign("Definitions", Definitions, envir=get("RstoxEnv"))
    assign("Projects", list(), envir=get("RstoxEnv"))
    return(Definitions)
}

