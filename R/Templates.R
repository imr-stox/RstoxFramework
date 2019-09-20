#### Templates: ####





stoxTemplates <- list(
    
    #### Empty template: ####
    emptyTemplate = list(
        description <- "Empty template (user defined)"
        
        processes <- list()
    )
    
    #### Template to calculate the length distribution per station: ####
    stationLengthDistributionTemplate = list(
        
        description <- "Bla bla"
        
        processes <- list(
            # Read the biotic data:
            ReadBiotic = list(
                ProcessName = "ReadBiotic", 
                FunctionName = "ReadBiotic", 
                ProcessParameters = list(
                    FileOutput = FALSE
                )
            ), 
            # Convert to StoxBiotic:
            StoxBiotic = list(
                ProcessName = "StoxBiotic", 
                FunctionName = "StoxBiotic", 
                FunctionInputs = list(
                    BioticData = "ReadBiotic"
                )
            ),
            # Filter StoxBiotic:
            FilterStoxBiotic = list(
                ProcessName = "FilterStoxBiotic", 
                FunctionName = "FilterStoxBiotic", 
                FunctionInputs = list(
                    StoxBioticData = "StoxBiotic"
                )
            ),
            # Get the length distribution per station:
            StationLengthDist = list(
                ProcessName = "StationLengthDist", 
                FunctionName = "StationLengthDist", 
                FunctionInputs = list(
                    StoxBioticData = "FilterStoxBiotic"
                ), 
                FunctionParameters = list(
                    LengthDistType = "PercentLengthDist"
                )
            )
        )
        
        
    ), 
    
    #### Simple template to read biotic data: ####
    readBioticDataTemplate = list(
        # Read the biotic data:
        ReadBiotic = list(
            ProcessName = "ReadBiotic", 
            FunctionName = "ReadBiotic", 
            ProcessParameters = list(
                FileOutput = FALSE
            )
        )
    )
    
)