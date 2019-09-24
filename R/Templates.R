stoxTemplates <- list(
    #### AcousticAbundanceTransectTemplate: Acoustic abundance by transect and r-model with uncertainty
    AcousticAbundanceTransectTemplate <- list(
        description <- "Acoustic abundance by transect and r-model with uncertainty",
        Baseline <- list(
            ReadProcessData <- list(
                ProcessName = "ReadProcessData",
                FunctionName = "ReadProcessData",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                )
            ),
            ReadAcousticXML <- list(
                ProcessName = "ReadAcousticXML",
                FunctionName = "ReadAcousticXML",
                ProcessParameters <- list(
                    FileOutput = FALSE,
                    BreakInGUI = FALSE
                )
            ),
            FilterAcoustic <- list(
                ProcessName = "FilterAcoustic",
                FunctionName = "FilterAcoustic",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    AcousticData = "ReadAcousticXML"
                ),
                FunctionParameters <- list(
                    FreqExpr = "frequency == 38000 and transceiver == 2",
                    NASCExpr = "acocat == 12 and chtype == 'P'"
                )
            ),
            SumNASC <- list(
                ProcessName = "SumNASC",
                FunctionName = "SumNASC",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    AcousticData = "FilterAcoustic"
                ),
                FunctionParameters <- list(
                    LayerType = "WaterColumn"
                )
            ),
            ReadBioticXML <- list(
                ProcessName = "ReadBioticXML",
                FunctionName = "ReadBioticXML",
                ProcessParameters <- list(
                    FileOutput = FALSE,
                    BreakInGUI = FALSE
                )
            ),
            FilterBiotic <- list(
                ProcessName = "FilterBiotic",
                FunctionName = "FilterBiotic",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    BioticData = "RedefineSpecCat"
                )
            ),
            StationLengthDist <- list(
                ProcessName = "StationLengthDist",
                FunctionName = "StationLengthDist",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    BioticData = "FilterBiotic"
                ),
                FunctionParameters <- list(
                    LengthDistType = "PercentLengthDist"
                )
            ),
            RegroupLengthDist <- list(
                ProcessName = "RegroupLengthDist",
                FunctionName = "RegroupLengthDist",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    LengthDist = "StationLengthDist"
                ),
                FunctionParameters <- list(
                    LengthInterval = 1.0
                )
            ),
            DefineStrata <- list(
                ProcessName = "DefineStrata",
                FunctionName = "DefineStrata",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = TRUE
                ),
                FunctionInputs <- list(
                    ProcessData = "ReadProcessData"
                ),
                FunctionParameters <- list(
                    UseProcessData = FALSE
                )
            ),
            StratumArea <- list(
                ProcessName = "StratumArea",
                FunctionName = "StratumArea",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    ProcessData = "ReadProcessData"
                ),
                FunctionParameters <- list(
                    AreaMethod = "Simple"
                )
            ),
            DefineAcousticPSU <- list(
                ProcessName = "DefineAcousticPSU",
                FunctionName = "DefineAcousticPSU",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = TRUE
                ),
                FunctionInputs <- list(
                    ProcessData = "ReadProcessData",
                    AcousticData = "FilterAcoustic"
                ),
                FunctionParameters <- list(
                    DefinitionMethod = "UseProcessData"
                )
            ),
            MeanNASC <- list(
                ProcessName = "MeanNASC",
                FunctionName = "MeanNASC",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    ProcessData = "ReadProcessData",
                    NASC = "SumNASC"
                ),
                FunctionParameters <- list(
                    SampleUnitType = "PSU"
                )
            ),
            BioStationAssignment <- list(
                ProcessName = "BioStationAssignment",
                FunctionName = "BioStationAssignment",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = TRUE
                ),
                FunctionInputs <- list(
                    ProcessData = "ReadProcessData",
                    BioticData = "FilterBiotic",
                    AcousticData = "FilterAcoustic"
                ),
                FunctionParameters <- list(
                    AssignmentMethod = "Stratum",
                    Radius = 15.0,
                    EstLayers = "1~PELBOT"
                )
            ),
            BioStationWeighting <- list(
                ProcessName = "BioStationWeighting",
                FunctionName = "BioStationWeighting",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    ProcessData = "ReadProcessData",
                    BioticData = "FilterBiotic",
                    NASC = "SumNASC",
                    AcousticData = "FilterAcoustic",
                    LengthDist = "RegroupLengthDist"
                ),
                FunctionParameters <- list(
                    WeightingMethod = "Equal",
                    m = 20,
                    MaxNumLengthSamples = 100
                )
            ),
            TotalLengthDist <- list(
                ProcessName = "TotalLengthDist",
                FunctionName = "TotalLengthDist",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    ProcessData = "ReadProcessData",
                    LengthDist = "RegroupLengthDist"
                )
            ),
            AcousticDensity <- list(
                ProcessName = "AcousticDensity",
                FunctionName = "AcousticDensity",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    ProcessData = "ReadProcessData",
                    LengthDist = "TotalLengthDist",
                    NASC = "MeanNASC"
                ),
                FunctionParameters <- list(
                    m = 20
                )
            ),
            MeanDensity_Stratum <- list(
                ProcessName = "MeanDensity_Stratum",
                FunctionName = "MeanDensity",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    ProcessData = "ReadProcessData",
                    Density = "AcousticDensity"
                ),
                FunctionParameters <- list(
                    SampleUnitType = "Stratum"
                )
            ),
            SumDensity_Stratum <- list(
                ProcessName = "SumDensity_Stratum",
                FunctionName = "SumDensity",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    Density = "MeanDensity_Stratum"
                )
            ),
            Abundance <- list(
                ProcessName = "Abundance",
                FunctionName = "Abundance",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    Density = "SumDensity_Stratum",
                    PolygonArea = "StratumArea"
                )
            ),
            IndividualDataStations <- list(
                ProcessName = "IndividualDataStations",
                FunctionName = "IndividualDataStations",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    ProcessData = "ReadProcessData",
                    Abundance = "Abundance"
                )
            ),
            IndividualData <- list(
                ProcessName = "IndividualData",
                FunctionName = "IndividualData",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    BioticData = "FilterBiotic",
                    IndividualDataStations = "IndividualDataStations"
                )
            ),
            SuperIndAbundance <- list(
                ProcessName = "SuperIndAbundance",
                FunctionName = "SuperIndAbundance",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    Abundance = "Abundance",
                    IndividualData = "IndividualData",
                    ProcessData = "ReadProcessData",
                    LengthDist = "RegroupLengthDist"
                ),
                FunctionParameters <- list(
                    AbundWeightMethod = "Equal"
                )
            ),
            WriteProcessData <- list(
                ProcessName = "WriteProcessData",
                FunctionName = "WriteProcessData",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                )
            )
        ),
        Statistics <- list(
            runBootstrap <- list(
                ProcessName = "runBootstrap",
                FunctionName = "runBootstrap",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionParameters <- list(
                    bootstrapMethod = "AcousticTrawl",
                    acousticMethod = "PSU~Stratum",
                    bioticMethod = "PSU~Stratum",
                    startProcess = "Process(TotalLengthDist)",
                    endProcess = "Process(SuperIndAbundance)",
                    nboot = 5,
                    seed = 1,
                    cores = 1
                )
            ),
            imputeByAge <- list(
                ProcessName = "imputeByAge",
                FunctionName = "imputeByAge",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionParameters <- list(
                    seed = 1,
                    cores = 1
                )
            ),
            saveProjectData <- list(
                ProcessName = "saveProjectData",
                FunctionName = "saveProjectData",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                )
            )
        ),
        Report <- list(
            FillMissingData <- list(
                ProcessName = "FillMissingData",
                FunctionName = "FillMissingData",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    SuperIndividuals = "SuperIndAbundance"
                ),
                FunctionParameters <- list(
                    FillVariables = "ImputeByAge",
                    Seed = 1,
                    FillWeight = "Mean",
                    a = "0.01",
                    b = "3.0"
                )
            ),
            EstimateByPopulationCategory <- list(
                ProcessName = "EstimateByPopulationCategory",
                FunctionName = "EstimateByPopulationCategory",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    SuperIndividuals = "FillMissingData"
                ),
                FunctionParameters <- list(
                    LengthInterval = 1.0,
                    Scale = 1000,
                    Dim1 = "LenGrp",
                    Dim2 = "age",
                    Dim3 = "SpecCat",
                    Dim4 = "none",
                    Dim5 = "none"
                )
            ),
            getReports <- list(
                ProcessName = "getReports",
                FunctionName = "getReports",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionParameters <- list(
                    out = "all"
                )
            ),
            getPlots <- list(
                ProcessName = "getPlots",
                FunctionName = "getPlots",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionParameters <- list(
                    out = "all"
                )
            )
        )
    ),
    #### SweptAreaTemplate: Swept area (length dependent)
    SweptAreaTemplate <- list(
        description <- "Swept area (length dependent)",
        Baseline <- list(
            ReadProcessData <- list(
                ProcessName = "ReadProcessData",
                FunctionName = "ReadProcessData",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                )
            ),
            ReadBioticXML <- list(
                ProcessName = "ReadBioticXML",
                FunctionName = "ReadBioticXML",
                ProcessParameters <- list(
                    FileOutput = FALSE,
                    BreakInGUI = FALSE
                )
            ),
            FilterBiotic <- list(
                ProcessName = "FilterBiotic",
                FunctionName = "FilterBiotic",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    BioticData = "RedefineSpecCat"
                )
            ),
            StationLengthDist <- list(
                ProcessName = "StationLengthDist",
                FunctionName = "StationLengthDist",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    BioticData = "FilterBiotic"
                ),
                FunctionParameters <- list(
                    LengthDistType = "NormLengthDist"
                )
            ),
            RegroupLengthDist <- list(
                ProcessName = "RegroupLengthDist",
                FunctionName = "RegroupLengthDist",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    LengthDist = "StationLengthDist"
                ),
                FunctionParameters <- list(
                    LengthInterval = 1.0
                )
            ),
            DefineStrata <- list(
                ProcessName = "DefineStrata",
                FunctionName = "DefineStrata",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = TRUE
                ),
                FunctionInputs <- list(
                    ProcessData = "ReadProcessData"
                ),
                FunctionParameters <- list(
                    UseProcessData = FALSE
                )
            ),
            StratumArea <- list(
                ProcessName = "StratumArea",
                FunctionName = "StratumArea",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    ProcessData = "ReadProcessData"
                ),
                FunctionParameters <- list(
                    AreaMethod = "Simple"
                )
            ),
            DefineSweptAreaPSU <- list(
                ProcessName = "DefineSweptAreaPSU",
                FunctionName = "DefineSweptAreaPSU",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    ProcessData = "ReadProcessData",
                    BioticData = "FilterBiotic"
                ),
                FunctionParameters <- list(
                    Method = "Station"
                )
            ),
            TotalLengthDist <- list(
                ProcessName = "TotalLengthDist",
                FunctionName = "TotalLengthDist",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    ProcessData = "ReadProcessData",
                    LengthDist = "RegroupLengthDist"
                )
            ),
            SweptAreaDensity <- list(
                ProcessName = "SweptAreaDensity",
                FunctionName = "SweptAreaDensity",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    ProcessData = "ReadProcessData",
                    BioticData = "FilterBiotic",
                    LengthDist = "TotalLengthDist"
                ),
                FunctionParameters <- list(
                    SweptAreaMethod = "LengthDependent",
                    CatchVariable = "Weight",
                    DistanceMethod = "FullDistance",
                    SweepWidthMethod = "Constant",
                    SweepWidth = 25
                )
            ),
            MeanDensity_Stratum <- list(
                ProcessName = "MeanDensity_Stratum",
                FunctionName = "MeanDensity",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    ProcessData = "ReadProcessData",
                    Density = "SweptAreaDensity"
                ),
                FunctionParameters <- list(
                    SampleUnitType = "Stratum"
                )
            ),
            Abundance <- list(
                ProcessName = "Abundance",
                FunctionName = "Abundance",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    Density = "MeanDensity_Stratum",
                    PolygonArea = "StratumArea"
                )
            ),
            IndividualDataStations <- list(
                ProcessName = "IndividualDataStations",
                FunctionName = "IndividualDataStations",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    ProcessData = "ReadProcessData",
                    Abundance = "Abundance"
                )
            ),
            IndividualData <- list(
                ProcessName = "IndividualData",
                FunctionName = "IndividualData",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    BioticData = "FilterBiotic",
                    IndividualDataStations = "IndividualDataStations"
                )
            ),
            SuperIndAbundance <- list(
                ProcessName = "SuperIndAbundance",
                FunctionName = "SuperIndAbundance",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    Abundance = "Abundance",
                    IndividualData = "IndividualData",
                    ProcessData = "ReadProcessData",
                    LengthDist = "RegroupLengthDist"
                ),
                FunctionParameters <- list(
                    AbundWeightMethod = "StationDensity"
                )
            ),
            WriteProcessData <- list(
                ProcessName = "WriteProcessData",
                FunctionName = "WriteProcessData",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                )
            )
        ),
        Statistics <- list(
            runBootstrap <- list(
                ProcessName = "runBootstrap",
                FunctionName = "runBootstrap",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionParameters <- list(
                    bootstrapMethod = "SweptAreaLength",
                    acousticMethod = "",
                    bioticMethod = "PSU~Stratum",
                    startProcess = "Process(TotalLengthDist)",
                    endProcess = "Process(SuperIndAbundance)",
                    nboot = 5,
                    seed = 1,
                    cores = 1
                )
            ),
            imputeByAge <- list(
                ProcessName = "imputeByAge",
                FunctionName = "imputeByAge",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionParameters <- list(
                    seed = 1,
                    cores = 1
                )
            ),
            saveProjectData <- list(
                ProcessName = "saveProjectData",
                FunctionName = "saveProjectData",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                )
            )
        ),
        Report <- list(
            FillMissingData <- list(
                ProcessName = "FillMissingData",
                FunctionName = "FillMissingData",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    SuperIndividuals = "SuperIndAbundance"
                ),
                FunctionParameters <- list(
                    FillVariables = "ImputeByAge",
                    Seed = 1,
                    FillWeight = "Mean",
                    a = "0.01",
                    b = "3.0"
                )
            ),
            EstimateByPopulationCategory <- list(
                ProcessName = "EstimateByPopulationCategory",
                FunctionName = "EstimateByPopulationCategory",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    SuperIndividuals = "FillMissingData"
                ),
                FunctionParameters <- list(
                    LengthInterval = 1.0,
                    Scale = 1000,
                    Dim1 = "LenGrp",
                    Dim2 = "age",
                    Dim3 = "SpecCat",
                    Dim4 = "none",
                    Dim5 = "none"
                )
            ),
            getReports <- list(
                ProcessName = "getReports",
                FunctionName = "getReports",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionParameters <- list(
                    out = "all"
                )
            ),
            getPlots <- list(
                ProcessName = "getPlots",
                FunctionName = "getPlots",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionParameters <- list(
                    out = "all"
                )
            )
        )
    ),
    #### SweptAreaTotalTemplate: Swept area (total catch)
    SweptAreaTotalTemplate <- list(
        description <- "Swept area (total catch)",
        Baseline <- list(
            ReadProcessData <- list(
                ProcessName = "ReadProcessData",
                FunctionName = "ReadProcessData",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                )
            ),
            ReadBioticXML <- list(
                ProcessName = "ReadBioticXML",
                FunctionName = "ReadBioticXML",
                ProcessParameters <- list(
                    FileOutput = FALSE,
                    BreakInGUI = FALSE
                )
            ),
            FilterBiotic <- list(
                ProcessName = "FilterBiotic",
                FunctionName = "FilterBiotic",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    BioticData = "RedefineSpecCat"
                )
            ),
            DefineStrata <- list(
                ProcessName = "DefineStrata",
                FunctionName = "DefineStrata",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = TRUE
                ),
                FunctionInputs <- list(
                    ProcessData = "ReadProcessData"
                ),
                FunctionParameters <- list(
                    UseProcessData = FALSE
                )
            ),
            DefineSweptAreaPSU <- list(
                ProcessName = "DefineSweptAreaPSU",
                FunctionName = "DefineSweptAreaPSU",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    ProcessData = "ReadProcessData",
                    BioticData = "FilterBiotic"
                ),
                FunctionParameters <- list(
                    Method = "Station"
                )
            ),
            SweptAreaCountDensity <- list(
                ProcessName = "SweptAreaCountDensity",
                FunctionName = "SweptAreaDensity",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    ProcessData = "ReadProcessData",
                    BioticData = "FilterBiotic"
                ),
                FunctionParameters <- list(
                    SweptAreaMethod = "TotalCatch",
                    CatchVariable = "Count",
                    DistanceMethod = "FullDistance",
                    SweepWidthMethod = "Constant",
                    SweepWidth = 25
                )
            ),
            MeanCountDensity_Stratum <- list(
                ProcessName = "MeanCountDensity_Stratum",
                FunctionName = "MeanDensity",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    ProcessData = "ReadProcessData",
                    Density = "SweptAreaCountDensity"
                ),
                FunctionParameters <- list(
                    SampleUnitType = "Stratum"
                )
            ),
            AbundanceCount_Stratum <- list(
                ProcessName = "AbundanceCount_Stratum",
                FunctionName = "Abundance",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    Density = "MeanCountDensity_Stratum",
                    PolygonArea = "StratumArea"
                )
            ),
            SweptAreaWeightDensity <- list(
                ProcessName = "SweptAreaWeightDensity",
                FunctionName = "SweptAreaDensity",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    ProcessData = "ReadProcessData",
                    BioticData = "FilterBiotic"
                ),
                FunctionParameters <- list(
                    SweptAreaMethod = "TotalCatch",
                    CatchVariable = "Weight",
                    DistanceMethod = "FullDistance",
                    SweepWidthMethod = "Constant",
                    SweepWidth = 25
                )
            ),
            MeanWeightDensity_Stratum <- list(
                ProcessName = "MeanWeightDensity_Stratum",
                FunctionName = "MeanDensity",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    ProcessData = "ReadProcessData",
                    Density = "SweptAreaWeightDensity"
                ),
                FunctionParameters <- list(
                    SampleUnitType = "Stratum"
                )
            ),
            AbundanceWeight_Stratum <- list(
                ProcessName = "AbundanceWeight_Stratum",
                FunctionName = "Abundance",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    Density = "MeanWeightDensity_Stratum",
                    PolygonArea = "StratumArea"
                )
            ),
            WriteProcessData <- list(
                ProcessName = "WriteProcessData",
                FunctionName = "WriteProcessData",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                )
            )
        ),
        Statistics <- list(
            runBootstrap <- list(
                ProcessName = "runBootstrap",
                FunctionName = "runBootstrap",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionParameters <- list(
                    bootstrapMethod = "SweptAreaTotal",
                    acousticMethod = "",
                    bioticMethod = "PSU~Stratum",
                    startProcess = "Process(SweptAreaCountDensity)",
                    endProcess = "Process(SweptAreaCountDensity)",
                    nboot = 5,
                    seed = 1,
                    cores = 1
                )
            ),
            saveProjectData <- list(
                ProcessName = "saveProjectData",
                FunctionName = "saveProjectData",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                )
            )
        ),
        Report <- list(
            TotalAbundance <- list(
                ProcessName = "TotalAbundance",
                FunctionName = "TotalAbundance",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    WeightAbundance = "AbundanceWeight_Stratum",
                    CountAbundance = "AbundanceCount_Stratum"
                ),
                FunctionParameters <- list(
                    Scale = 1000
                )
            ),
            getReports <- list(
                ProcessName = "getReports",
                FunctionName = "getReports",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionParameters <- list(
                    out = "all",
                    options = "bootstrapMethod='SweptAreaTotal'"
                )
            ),
            getPlots <- list(
                ProcessName = "getPlots",
                FunctionName = "getPlots",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionParameters <- list(
                    out = "all",
                    options = "bootstrapMethod='SweptAreaTotal'"
                )
            )
        )
    ),
    #### SweptAreaTotalSpecCatTemplate: Station species category density
    SweptAreaTotalSpecCatTemplate <- list(
        description <- "Station species category density",
        Baseline <- list(
            ReadProcessData <- list(
                ProcessName = "ReadProcessData",
                FunctionName = "ReadProcessData",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                )
            ),
            ReadBioticXML <- list(
                ProcessName = "ReadBioticXML",
                FunctionName = "ReadBioticXML",
                ProcessParameters <- list(
                    FileOutput = FALSE,
                    BreakInGUI = FALSE
                )
            ),
            FilterBiotic <- list(
                ProcessName = "FilterBiotic",
                FunctionName = "FilterBiotic",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    BioticData = "RedefineSpecCat"
                )
            ),
            DefineStrata <- list(
                ProcessName = "DefineStrata",
                FunctionName = "DefineStrata",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = TRUE
                ),
                FunctionInputs <- list(
                    ProcessData = "ReadProcessData"
                ),
                FunctionParameters <- list(
                    UseProcessData = FALSE
                )
            ),
            DefineSweptAreaPSU <- list(
                ProcessName = "DefineSweptAreaPSU",
                FunctionName = "DefineSweptAreaPSU",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    ProcessData = "ReadProcessData",
                    BioticData = "FilterBiotic"
                ),
                FunctionParameters <- list(
                    Method = "Station"
                )
            ),
            SweptAreaCountDensity <- list(
                ProcessName = "SweptAreaCountDensity",
                FunctionName = "SweptAreaDensity",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    ProcessData = "ReadProcessData",
                    BioticData = "FilterBiotic"
                ),
                FunctionParameters <- list(
                    SweptAreaMethod = "TotalCatch",
                    CatchVariable = "Count",
                    DistanceMethod = "FullDistance",
                    SweepWidthMethod = "Constant",
                    SweepWidth = 25
                )
            ),
            StationSpecCatDensity_Count <- list(
                ProcessName = "StationSpecCatDensity_Count",
                FunctionName = "StationSpecCatDensity",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    BioticData = "FilterBiotic",
                    ProcessData = "ReadProcessData",
                    Density = "SweptAreaCountDensity"
                )
            ),
            SweptAreaWeightDensity <- list(
                ProcessName = "SweptAreaWeightDensity",
                FunctionName = "SweptAreaDensity",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    ProcessData = "ReadProcessData",
                    BioticData = "FilterBiotic"
                ),
                FunctionParameters <- list(
                    SweptAreaMethod = "TotalCatch",
                    CatchVariable = "Weight",
                    DistanceMethod = "FullDistance",
                    SweepWidthMethod = "Constant",
                    SweepWidth = 25
                )
            ),
            StationSpecCatDensity_Weight <- list(
                ProcessName = "StationSpecCatDensity_Weight",
                FunctionName = "StationSpecCatDensity",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    BioticData = "FilterBiotic",
                    ProcessData = "ReadProcessData",
                    Density = "SweptAreaWeightDensity"
                )
            ),
            WriteProcessData <- list(
                ProcessName = "WriteProcessData",
                FunctionName = "WriteProcessData",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                )
            )
        )
    ),
    #### SplitNASCTemplate: Split NASC
    SplitNASCTemplate <- list(
        description <- "Split NASC",
        Baseline <- list(
            ReadAcousticXML <- list(
                ProcessName = "ReadAcousticXML",
                FunctionName = "ReadAcousticXML",
                ProcessParameters <- list(
                    FileOutput = FALSE,
                    BreakInGUI = FALSE
                )
            ),
            FilterAcoustic <- list(
                ProcessName = "FilterAcoustic",
                FunctionName = "FilterAcoustic",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    AcousticData = "ReadAcousticXML"
                ),
                FunctionParameters <- list(
                    FreqExpr = "frequency == 38000 and transceiver == 2"
                )
            ),
            SumNASC <- list(
                ProcessName = "SumNASC",
                FunctionName = "SumNASC",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    AcousticData = "FilterAcoustic"
                ),
                FunctionParameters <- list(
                    LayerType = "PChannel"
                )
            ),
            ReadBioticXML <- list(
                ProcessName = "ReadBioticXML",
                FunctionName = "ReadBioticXML",
                ProcessParameters <- list(
                    FileOutput = FALSE,
                    BreakInGUI = FALSE
                )
            ),
            FilterBiotic <- list(
                ProcessName = "FilterBiotic",
                FunctionName = "FilterBiotic",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    BioticData = "RedefineSpecCat"
                )
            ),
            StationLengthDist <- list(
                ProcessName = "StationLengthDist",
                FunctionName = "StationLengthDist",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    BioticData = "FilterBiotic"
                ),
                FunctionParameters <- list(
                    LengthDistType = "NormLengthDist"
                )
            ),
            RegroupLengthDist <- list(
                ProcessName = "RegroupLengthDist",
                FunctionName = "RegroupLengthDist",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    LengthDist = "StationLengthDist"
                ),
                FunctionParameters <- list(
                    LengthInterval = 1.0
                )
            ),
            SplitNASCAssignment <- list(
                ProcessName = "SplitNASCAssignment",
                FunctionName = "SplitNASCAssignment",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    AcousticData = "FilterAcoustic",
                    BioticData = "FilterBiotic"
                ),
                FunctionParameters <- list(
                    Radius = 50.0
                )
            ),
            TotalLengthDist <- list(
                ProcessName = "TotalLengthDist",
                FunctionName = "TotalLengthDist",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    ProcessData = "SplitNASCAssignment",
                    LengthDist = "RegroupLengthDist"
                )
            ),
            SplitNASC_BUNN <- list(
                ProcessName = "SplitNASC_BUNN",
                FunctionName = "SplitNASC",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    ProcessData = "SplitNASCAssignment",
                    AcousticData = "FilterAcoustic",
                    LengthDist = "TotalLengthDist",
                    NASC = "SumNASC"
                ),
                FunctionParameters <- list(
                    SpeciesTS = "2;22;SEI;20.0;-67.0;0.0/2;18;HVITTING;20.0;-67.0;0.0/2;31;TORSK;20.0;-66.0;0.0/2;30;HYSE;20.0;-65.0;0.0/2;28;ØYEPÅL;20.0;-67.0;0.0/2;24;KOLMULE;20.0;-67.0;0.0"
                )
            ),
            SplitNASC_BUNN2 <- list(
                ProcessName = "SplitNASC_BUNN2",
                FunctionName = "SplitNASC",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    ProcessData = "SplitNASCAssignment",
                    AcousticData = "FilterAcoustic",
                    LengthDist = "TotalLengthDist",
                    NASC = "SumNASC"
                ),
                FunctionParameters <- list(
                    SpeciesTS = "52;22;SEI;20.0;-67.0;0.0/52;18;HVITTING;20.0;-67.0;0.0/52;31;TORSK;20.0;-66.0;0.0/52;30;HYSE;20.0;-65.0;0.0/52;28;ØYEPÅL;20.0;-67.0;0.0"
                )
            ),
            CombineNASC_SEI <- list(
                ProcessName = "CombineNASC_SEI",
                FunctionName = "CombineNASC",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    NASC1 = "SplitNASC_BUNN",
                    NASC2 = "SplitNASC_BUNN2",
                    NASC3 = "SumNASC"
                ),
                FunctionParameters <- list(
                    TargetAcoCat = "22"
                )
            ),
            CombineNASC_TORSK <- list(
                ProcessName = "CombineNASC_TORSK",
                FunctionName = "CombineNASC",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    NASC1 = "SplitNASC_BUNN",
                    NASC2 = "SplitNASC_BUNN2",
                    NASC3 = "SumNASC"
                ),
                FunctionParameters <- list(
                    TargetAcoCat = "31"
                )
            ),
            CombineNASC <- list(
                ProcessName = "CombineNASC",
                FunctionName = "CombineNASC",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    NASC1 = "CombineNASC_SEI",
                    NASC2 = "CombineNASC_TORSK"
                )
            ),
            NASCToAcousticData <- list(
                ProcessName = "NASCToAcousticData",
                FunctionName = "NASCToAcousticData",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    AcousticData = "FilterAcoustic",
                    NASC = "CombineNASC"
                )
            ),
            WriteAcousticDataToXML <- list(
                ProcessName = "WriteAcousticDataToXML",
                FunctionName = "WriteAcousticDataToXML",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    AcousticData = "NASCToAcousticData"
                ),
                FunctionParameters <- list(
                    Directory = "output/baseline"
                )
            )
        )
    ),
    #### LengthWeightRelationShipTemplate: Length Weight relationship
    LengthWeightRelationShipTemplate <- list(
        description <- "Length Weight relationship",
        Baseline <- list(
            ReadProcessData <- list(
                ProcessName = "ReadProcessData",
                FunctionName = "ReadProcessData",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                )
            ),
            ReadBioticXML <- list(
                ProcessName = "ReadBioticXML",
                FunctionName = "ReadBioticXML",
                ProcessParameters <- list(
                    FileOutput = FALSE,
                    BreakInGUI = FALSE
                )
            ),
            FilterBiotic <- list(
                ProcessName = "FilterBiotic",
                FunctionName = "FilterBiotic",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    BioticData = "RedefineSpecCat"
                )
            ),
            DefineStrata <- list(
                ProcessName = "DefineStrata",
                FunctionName = "DefineStrata",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    ProcessData = "ReadProcessData"
                ),
                FunctionParameters <- list(
                    UseProcessData = FALSE
                )
            ),
            DefineSweptAreaPSU <- list(
                ProcessName = "DefineSweptAreaPSU",
                FunctionName = "DefineSweptAreaPSU",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    ProcessData = "ReadProcessData",
                    BioticData = "FilterBiotic"
                ),
                FunctionParameters <- list(
                    Method = "Station"
                )
            ),
            LengthWeightRelationship <- list(
                ProcessName = "LengthWeightRelationship",
                FunctionName = "LengthWeightRelationship",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    ProcessData = "ReadProcessData",
                    BioticData = "FilterBiotic"
                )
            ),
            WriteProcessData <- list(
                ProcessName = "WriteProcessData",
                FunctionName = "WriteProcessData",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                )
            )
        )
    ),
    #### StationLengthDistTemplate: Station length distribution
    StationLengthDistTemplate <- list(
        description <- "Station length distribution",
        Baseline <- list(
            ReadBioticXML <- list(
                ProcessName = "ReadBioticXML",
                FunctionName = "ReadBioticXML",
                ProcessParameters <- list(
                    FileOutput = FALSE,
                    BreakInGUI = FALSE
                )
            ),
            FilterBiotic <- list(
                ProcessName = "FilterBiotic",
                FunctionName = "FilterBiotic",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    BioticData = "RedefineSpecCat"
                )
            ),
            StationLengthDist <- list(
                ProcessName = "StationLengthDist",
                FunctionName = "StationLengthDist",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    BioticData = "FilterBiotic"
                ),
                FunctionParameters <- list(
                    LengthDistType = "PercentLengthDist"
                )
            ),
            RegroupLengthDist <- list(
                ProcessName = "RegroupLengthDist",
                FunctionName = "RegroupLengthDist",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    LengthDist = "StationLengthDist"
                ),
                FunctionParameters <- list(
                    LengthInterval = 1.0
                )
            )
        )
    ),
    #### DATRASTemplate: DATRAS conversion
    DATRASTemplate <- list(
        description <- "DATRAS conversion",
        Baseline <- list(
            ReadBioticXML <- list(
                ProcessName = "ReadBioticXML",
                FunctionName = "ReadBioticXML",
                ProcessParameters <- list(
                    FileOutput = FALSE,
                    BreakInGUI = FALSE
                )
            ),
            FilterBiotic <- list(
                ProcessName = "FilterBiotic",
                FunctionName = "FilterBiotic",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    BioticData = "RedefineSpecCat"
                )
            ),
            DATRASConvert <- list(
                ProcessName = "DATRASConvert",
                FunctionName = "DATRASConvert",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                ),
                FunctionInputs <- list(
                    BioticData = "FilterBiotic"
                )
            )
        ),
        Statistics <- list(
            prepareDATRAS <- list(
                ProcessName = "prepareDATRAS",
                FunctionName = "prepareDATRAS",
                ProcessParameters <- list(
                    FileOutput = TRUE,
                    BreakInGUI = FALSE
                )
            )
        )
    ),
    #### UserDefinedTemplate: User defined (empty models)
    UserDefinedTemplate <- list(
        description <- "User defined (empty models)"
    )
)