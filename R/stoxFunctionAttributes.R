# A list of 
stoxFunctionAttributes <- list(
    # Read input biotic data:
    ReadBiotic = list(
        FunctionCategory = "Baseline", 
        FunctionOutputDataType = "BioticData", 
        FunctionParameterHierarchy = list(
            FileNames = list()
        )
    ), 
    # Read strata polygons:
    DefineStrata = list(
        FunctionCategory = "Baseline", 
        FunctionOutputDataType = "StratumPolygon", 
        FunctionParameterHierarchy = list(
            FileName = list(
                UseProcessData = FALSE
            ), 
            UseProcessData = list()
        )
    ), 
    # Calculate areas of strata polygons:
    StratumArea = list(
        FunctionCategory = "Baseline", 
        FunctionOutputDataType = "StratumArea", 
        FunctionParameterHierarchy = list(
            StratumPolygon = list(), 
            AreaMethod = list()
        )
    )
)


