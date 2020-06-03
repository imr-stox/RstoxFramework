stoxTemplates <- list(
	#### AcousticAbundance Template: Acoustic abundance by transect and r-model with uncertainty: ####
	AcousticAbundanceTransect = structure(
		list(
			Baseline = list(
				ReadProcessData = list(
					processName = "ReadProcessData",
					functionName = "ReadProcessData",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					)
				),
				ReadAcousticXML = list(
					processName = "ReadAcousticXML",
					functionName = "ReadAcousticXML",
					processParameters = list(
						FileOutput = FALSE,
						BreakInGUI = FALSE
					)
				),
				FilterAcoustic = list(
					processName = "FilterAcoustic",
					functionName = "FilterAcoustic",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						AcousticData = "ReadAcousticXML"
					),
					functionParameters = list(
						FreqExpr = "frequency == 38000 and transceiver == 2",
						NASCExpr = "acocat == 12 and chtype == 'P'"
					)
				),
				SumNASC = list(
					processName = "SumNASC",
					functionName = "SumNASC",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						AcousticData = "FilterAcoustic"
					),
					functionParameters = list(
						LayerType = "WaterColumn"
					)
				),
				ReadBioticXML = list(
					processName = "ReadBioticXML",
					functionName = "ReadBioticXML",
					processParameters = list(
						FileOutput = FALSE,
						BreakInGUI = FALSE
					)
				),
				FilterBiotic = list(
					processName = "FilterBiotic",
					functionName = "FilterBiotic",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						BioticData = "RedefineSpecCat"
					)
				),
				StationLengthDist = list(
					processName = "StationLengthDist",
					functionName = "StationLengthDist",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						BioticData = "FilterBiotic"
					),
					functionParameters = list(
						LengthDistType = "PercentLengthDist"
					)
				),
				RegroupLengthDist = list(
					processName = "RegroupLengthDist",
					functionName = "RegroupLengthDist",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						LengthDist = "StationLengthDist"
					),
					functionParameters = list(
						LengthInterval = 1.0
					)
				),
				DefineStrata = list(
					processName = "DefineStrata",
					functionName = "DefineStrata",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = TRUE
					),
					functionInputs = list(
						ProcessData = "ReadProcessData"
					),
					functionParameters = list(
						UseProcessData = FALSE
					)
				),
				StratumArea = list(
					processName = "StratumArea",
					functionName = "StratumArea",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						ProcessData = "ReadProcessData"
					),
					functionParameters = list(
						AreaMethod = "Simple"
					)
				),
				DefineAcousticPSU = list(
					processName = "DefineAcousticPSU",
					functionName = "DefineAcousticPSU",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = TRUE
					),
					functionInputs = list(
						ProcessData = "ReadProcessData",
						AcousticData = "FilterAcoustic"
					),
					functionParameters = list(
						DefinitionMethod = "UseProcessData"
					)
				),
				MeanNASC = list(
					processName = "MeanNASC",
					functionName = "MeanNASC",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						ProcessData = "ReadProcessData",
						NASC = "SumNASC"
					),
					functionParameters = list(
						SampleUnitType = "PSU"
					)
				),
				BioStationAssignment = list(
					processName = "BioStationAssignment",
					functionName = "BioStationAssignment",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = TRUE
					),
					functionInputs = list(
						ProcessData = "ReadProcessData",
						BioticData = "FilterBiotic",
						AcousticData = "FilterAcoustic"
					),
					functionParameters = list(
						AssignmentMethod = "Stratum",
						Radius = 15.0,
						EstLayers = "1~PELBOT"
					)
				),
				BioStationWeighting = list(
					processName = "BioStationWeighting",
					functionName = "BioStationWeighting",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						ProcessData = "ReadProcessData",
						BioticData = "FilterBiotic",
						NASC = "SumNASC",
						AcousticData = "FilterAcoustic",
						LengthDist = "RegroupLengthDist"
					),
					functionParameters = list(
						WeightingMethod = "Equal",
						m = 20,
						MaxNumLengthSamples = 100
					)
				),
				TotalLengthDist = list(
					processName = "TotalLengthDist",
					functionName = "TotalLengthDist",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						ProcessData = "ReadProcessData",
						LengthDist = "RegroupLengthDist"
					)
				),
				AcousticDensity = list(
					processName = "AcousticDensity",
					functionName = "AcousticDensity",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						ProcessData = "ReadProcessData",
						LengthDist = "TotalLengthDist",
						NASC = "MeanNASC"
					),
					functionParameters = list(
						m = 20
					)
				),
				MeanDensity_Stratum = list(
					processName = "MeanDensity_Stratum",
					functionName = "MeanDensity",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						ProcessData = "ReadProcessData",
						Density = "AcousticDensity"
					),
					functionParameters = list(
						SampleUnitType = "Stratum"
					)
				),
				SumDensity_Stratum = list(
					processName = "SumDensity_Stratum",
					functionName = "SumDensity",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						Density = "MeanDensity_Stratum"
					)
				),
				Abundance = list(
					processName = "Abundance",
					functionName = "Abundance",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						Density = "SumDensity_Stratum",
						PolygonArea = "StratumArea"
					)
				),
				IndividualDataStations = list(
					processName = "IndividualDataStations",
					functionName = "IndividualDataStations",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						ProcessData = "ReadProcessData",
						Abundance = "Abundance"
					)
				),
				IndividualData = list(
					processName = "IndividualData",
					functionName = "IndividualData",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						BioticData = "FilterBiotic",
						IndividualDataStations = "IndividualDataStations"
					)
				),
				SuperIndAbundance = list(
					processName = "SuperIndAbundance",
					functionName = "SuperIndAbundance",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						Abundance = "Abundance",
						IndividualData = "IndividualData",
						ProcessData = "ReadProcessData",
						LengthDist = "RegroupLengthDist"
					),
					functionParameters = list(
						AbundWeightMethod = "Equal"
					)
				),
				WriteProcessData = list(
					processName = "WriteProcessData",
					functionName = "WriteProcessData",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					)
				)
			),
			Analysis = list(
				runBootstrap = list(
					processName = "runBootstrap",
					functionName = "runBootstrap",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionParameters = list(
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
				imputeByAge = list(
					processName = "imputeByAge",
					functionName = "imputeByAge",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionParameters = list(
						seed = 1,
						cores = 1
					)
				),
				saveProjectData = list(
					processName = "saveProjectData",
					functionName = "saveProjectData",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					)
				)
			),
			Report = list(
				FillMissingData = list(
					processName = "FillMissingData",
					functionName = "FillMissingData",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						SuperIndividuals = "SuperIndAbundance"
					),
					functionParameters = list(
						FillVariables = "ImputeByAge",
						Seed = 1,
						FillWeight = "Mean",
						a = "0.01",
						b = "3.0"
					)
				),
				EstimateByPopulationCategory = list(
					processName = "EstimateByPopulationCategory",
					functionName = "EstimateByPopulationCategory",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						SuperIndividuals = "FillMissingData"
					),
					functionParameters = list(
						LengthInterval = 1.0,
						Scale = 1000,
						Dim1 = "LenGrp",
						Dim2 = "age",
						Dim3 = "SpecCat",
						Dim4 = "none",
						Dim5 = "none"
					)
				),
				getReports = list(
					processName = "getReports",
					functionName = "getReports",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionParameters = list(
						out = "all"
					)
				),
				getPlots = list(
					processName = "getPlots",
					functionName = "getPlots",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionParameters = list(
						out = "all"
					)
				)
			)
		), 
		description = "Acoustic abundance by transect and r-model with uncertainty"
	),
	#### SweptArea Template: Swept area (length dependent): ####
	SweptArea = structure(
		list(
			Baseline = list(
				ReadProcessData = list(
					processName = "ReadProcessData",
					functionName = "ReadProcessData",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					)
				),
				ReadBioticXML = list(
					processName = "ReadBioticXML",
					functionName = "ReadBioticXML",
					processParameters = list(
						FileOutput = FALSE,
						BreakInGUI = FALSE
					)
				),
				FilterBiotic = list(
					processName = "FilterBiotic",
					functionName = "FilterBiotic",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						BioticData = "RedefineSpecCat"
					)
				),
				StationLengthDist = list(
					processName = "StationLengthDist",
					functionName = "StationLengthDist",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						BioticData = "FilterBiotic"
					),
					functionParameters = list(
						LengthDistType = "NormLengthDist"
					)
				),
				RegroupLengthDist = list(
					processName = "RegroupLengthDist",
					functionName = "RegroupLengthDist",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						LengthDist = "StationLengthDist"
					),
					functionParameters = list(
						LengthInterval = 1.0
					)
				),
				DefineStrata = list(
					processName = "DefineStrata",
					functionName = "DefineStrata",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = TRUE
					),
					functionInputs = list(
						ProcessData = "ReadProcessData"
					),
					functionParameters = list(
						UseProcessData = FALSE
					)
				),
				StratumArea = list(
					processName = "StratumArea",
					functionName = "StratumArea",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						ProcessData = "ReadProcessData"
					),
					functionParameters = list(
						AreaMethod = "Simple"
					)
				),
				DefineSweptAreaPSU = list(
					processName = "DefineSweptAreaPSU",
					functionName = "DefineSweptAreaPSU",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						ProcessData = "ReadProcessData",
						BioticData = "FilterBiotic"
					),
					functionParameters = list(
						Method = "Station"
					)
				),
				TotalLengthDist = list(
					processName = "TotalLengthDist",
					functionName = "TotalLengthDist",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						ProcessData = "ReadProcessData",
						LengthDist = "RegroupLengthDist"
					)
				),
				SweptAreaDensity = list(
					processName = "SweptAreaDensity",
					functionName = "SweptAreaDensity",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						ProcessData = "ReadProcessData",
						BioticData = "FilterBiotic",
						LengthDist = "TotalLengthDist"
					),
					functionParameters = list(
						SweptAreaMethod = "LengthDependent",
						CatchVariable = "Weight",
						DistanceMethod = "FullDistance",
						SweepWidthMethod = "Constant",
						SweepWidth = 25
					)
				),
				MeanDensity_Stratum = list(
					processName = "MeanDensity_Stratum",
					functionName = "MeanDensity",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						ProcessData = "ReadProcessData",
						Density = "SweptAreaDensity"
					),
					functionParameters = list(
						SampleUnitType = "Stratum"
					)
				),
				Abundance = list(
					processName = "Abundance",
					functionName = "Abundance",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						Density = "MeanDensity_Stratum",
						PolygonArea = "StratumArea"
					)
				),
				IndividualDataStations = list(
					processName = "IndividualDataStations",
					functionName = "IndividualDataStations",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						ProcessData = "ReadProcessData",
						Abundance = "Abundance"
					)
				),
				IndividualData = list(
					processName = "IndividualData",
					functionName = "IndividualData",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						BioticData = "FilterBiotic",
						IndividualDataStations = "IndividualDataStations"
					)
				),
				SuperIndAbundance = list(
					processName = "SuperIndAbundance",
					functionName = "SuperIndAbundance",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						Abundance = "Abundance",
						IndividualData = "IndividualData",
						ProcessData = "ReadProcessData",
						LengthDist = "RegroupLengthDist"
					),
					functionParameters = list(
						AbundWeightMethod = "StationDensity"
					)
				),
				WriteProcessData = list(
					processName = "WriteProcessData",
					functionName = "WriteProcessData",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					)
				)
			),
			Analysis = list(
				runBootstrap = list(
					processName = "runBootstrap",
					functionName = "runBootstrap",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionParameters = list(
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
				imputeByAge = list(
					processName = "imputeByAge",
					functionName = "imputeByAge",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionParameters = list(
						seed = 1,
						cores = 1
					)
				),
				saveProjectData = list(
					processName = "saveProjectData",
					functionName = "saveProjectData",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					)
				)
			),
			Report = list(
				FillMissingData = list(
					processName = "FillMissingData",
					functionName = "FillMissingData",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						SuperIndividuals = "SuperIndAbundance"
					),
					functionParameters = list(
						FillVariables = "ImputeByAge",
						Seed = 1,
						FillWeight = "Mean",
						a = "0.01",
						b = "3.0"
					)
				),
				EstimateByPopulationCategory = list(
					processName = "EstimateByPopulationCategory",
					functionName = "EstimateByPopulationCategory",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						SuperIndividuals = "FillMissingData"
					),
					functionParameters = list(
						LengthInterval = 1.0,
						Scale = 1000,
						Dim1 = "LenGrp",
						Dim2 = "age",
						Dim3 = "SpecCat",
						Dim4 = "none",
						Dim5 = "none"
					)
				),
				getReports = list(
					processName = "getReports",
					functionName = "getReports",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionParameters = list(
						out = "all"
					)
				),
				getPlots = list(
					processName = "getPlots",
					functionName = "getPlots",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionParameters = list(
						out = "all"
					)
				)
			)
		), 
		description = "Swept area (length dependent)"
	),
	#### SweptAreaTotal Template: Swept area (total catch): ####
	SweptAreaTotal = structure(
		list(
			Baseline = list(
				ReadProcessData = list(
					processName = "ReadProcessData",
					functionName = "ReadProcessData",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					)
				),
				ReadBioticXML = list(
					processName = "ReadBioticXML",
					functionName = "ReadBioticXML",
					processParameters = list(
						FileOutput = FALSE,
						BreakInGUI = FALSE
					)
				),
				FilterBiotic = list(
					processName = "FilterBiotic",
					functionName = "FilterBiotic",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						BioticData = "RedefineSpecCat"
					)
				),
				DefineStrata = list(
					processName = "DefineStrata",
					functionName = "DefineStrata",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = TRUE
					),
					functionInputs = list(
						ProcessData = "ReadProcessData"
					),
					functionParameters = list(
						UseProcessData = FALSE
					)
				),
				DefineSweptAreaPSU = list(
					processName = "DefineSweptAreaPSU",
					functionName = "DefineSweptAreaPSU",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						ProcessData = "ReadProcessData",
						BioticData = "FilterBiotic"
					),
					functionParameters = list(
						Method = "Station"
					)
				),
				SweptAreaCountDensity = list(
					processName = "SweptAreaCountDensity",
					functionName = "SweptAreaDensity",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						ProcessData = "ReadProcessData",
						BioticData = "FilterBiotic"
					),
					functionParameters = list(
						SweptAreaMethod = "TotalCatch",
						CatchVariable = "Count",
						DistanceMethod = "FullDistance",
						SweepWidthMethod = "Constant",
						SweepWidth = 25
					)
				),
				MeanCountDensity_Stratum = list(
					processName = "MeanCountDensity_Stratum",
					functionName = "MeanDensity",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						ProcessData = "ReadProcessData",
						Density = "SweptAreaCountDensity"
					),
					functionParameters = list(
						SampleUnitType = "Stratum"
					)
				),
				AbundanceCount_Stratum = list(
					processName = "AbundanceCount_Stratum",
					functionName = "Abundance",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						Density = "MeanCountDensity_Stratum",
						PolygonArea = "StratumArea"
					)
				),
				SweptAreaWeightDensity = list(
					processName = "SweptAreaWeightDensity",
					functionName = "SweptAreaDensity",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						ProcessData = "ReadProcessData",
						BioticData = "FilterBiotic"
					),
					functionParameters = list(
						SweptAreaMethod = "TotalCatch",
						CatchVariable = "Weight",
						DistanceMethod = "FullDistance",
						SweepWidthMethod = "Constant",
						SweepWidth = 25
					)
				),
				MeanWeightDensity_Stratum = list(
					processName = "MeanWeightDensity_Stratum",
					functionName = "MeanDensity",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						ProcessData = "ReadProcessData",
						Density = "SweptAreaWeightDensity"
					),
					functionParameters = list(
						SampleUnitType = "Stratum"
					)
				),
				AbundanceWeight_Stratum = list(
					processName = "AbundanceWeight_Stratum",
					functionName = "Abundance",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						Density = "MeanWeightDensity_Stratum",
						PolygonArea = "StratumArea"
					)
				),
				WriteProcessData = list(
					processName = "WriteProcessData",
					functionName = "WriteProcessData",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					)
				)
			),
			Analysis = list(
				runBootstrap = list(
					processName = "runBootstrap",
					functionName = "runBootstrap",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionParameters = list(
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
				saveProjectData = list(
					processName = "saveProjectData",
					functionName = "saveProjectData",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					)
				)
			),
			Report = list(
				TotalAbundance = list(
					processName = "TotalAbundance",
					functionName = "TotalAbundance",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						WeightAbundance = "AbundanceWeight_Stratum",
						CountAbundance = "AbundanceCount_Stratum"
					),
					functionParameters = list(
						Scale = 1000
					)
				),
				getReports = list(
					processName = "getReports",
					functionName = "getReports",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionParameters = list(
						out = "all",
						options = "bootstrapMethod='SweptAreaTotal'"
					)
				),
				getPlots = list(
					processName = "getPlots",
					functionName = "getPlots",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionParameters = list(
						out = "all",
						options = "bootstrapMethod='SweptAreaTotal'"
					)
				)
			)
		), 
		description = "Swept area (total catch)"
	),
	#### SweptAreaTotalSpecCat Template: Station species category density: ####
	SweptAreaTotalSpecCat = structure(
		list(
			Baseline = list(
				ReadProcessData = list(
					processName = "ReadProcessData",
					functionName = "ReadProcessData",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					)
				),
				ReadBioticXML = list(
					processName = "ReadBioticXML",
					functionName = "ReadBioticXML",
					processParameters = list(
						FileOutput = FALSE,
						BreakInGUI = FALSE
					)
				),
				FilterBiotic = list(
					processName = "FilterBiotic",
					functionName = "FilterBiotic",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						BioticData = "RedefineSpecCat"
					)
				),
				DefineStrata = list(
					processName = "DefineStrata",
					functionName = "DefineStrata",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = TRUE
					),
					functionInputs = list(
						ProcessData = "ReadProcessData"
					),
					functionParameters = list(
						UseProcessData = FALSE
					)
				),
				DefineSweptAreaPSU = list(
					processName = "DefineSweptAreaPSU",
					functionName = "DefineSweptAreaPSU",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						ProcessData = "ReadProcessData",
						BioticData = "FilterBiotic"
					),
					functionParameters = list(
						Method = "Station"
					)
				),
				SweptAreaCountDensity = list(
					processName = "SweptAreaCountDensity",
					functionName = "SweptAreaDensity",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						ProcessData = "ReadProcessData",
						BioticData = "FilterBiotic"
					),
					functionParameters = list(
						SweptAreaMethod = "TotalCatch",
						CatchVariable = "Count",
						DistanceMethod = "FullDistance",
						SweepWidthMethod = "Constant",
						SweepWidth = 25
					)
				),
				StationSpecCatDensity_Count = list(
					processName = "StationSpecCatDensity_Count",
					functionName = "StationSpecCatDensity",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						BioticData = "FilterBiotic",
						ProcessData = "ReadProcessData",
						Density = "SweptAreaCountDensity"
					)
				),
				SweptAreaWeightDensity = list(
					processName = "SweptAreaWeightDensity",
					functionName = "SweptAreaDensity",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						ProcessData = "ReadProcessData",
						BioticData = "FilterBiotic"
					),
					functionParameters = list(
						SweptAreaMethod = "TotalCatch",
						CatchVariable = "Weight",
						DistanceMethod = "FullDistance",
						SweepWidthMethod = "Constant",
						SweepWidth = 25
					)
				),
				StationSpecCatDensity_Weight = list(
					processName = "StationSpecCatDensity_Weight",
					functionName = "StationSpecCatDensity",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						BioticData = "FilterBiotic",
						ProcessData = "ReadProcessData",
						Density = "SweptAreaWeightDensity"
					)
				),
				WriteProcessData = list(
					processName = "WriteProcessData",
					functionName = "WriteProcessData",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					)
				)
			)
		), 
		description = "Station species category density"
	),
	#### SplitNASC Template: Split NASC: ####
	SplitNASC = structure(
		list(
			Baseline = list(
				ReadAcousticXML = list(
					processName = "ReadAcousticXML",
					functionName = "ReadAcousticXML",
					processParameters = list(
						FileOutput = FALSE,
						BreakInGUI = FALSE
					)
				),
				FilterAcoustic = list(
					processName = "FilterAcoustic",
					functionName = "FilterAcoustic",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						AcousticData = "ReadAcousticXML"
					),
					functionParameters = list(
						FreqExpr = "frequency == 38000 and transceiver == 2"
					)
				),
				SumNASC = list(
					processName = "SumNASC",
					functionName = "SumNASC",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						AcousticData = "FilterAcoustic"
					),
					functionParameters = list(
						LayerType = "PChannel"
					)
				),
				ReadBioticXML = list(
					processName = "ReadBioticXML",
					functionName = "ReadBioticXML",
					processParameters = list(
						FileOutput = FALSE,
						BreakInGUI = FALSE
					)
				),
				FilterBiotic = list(
					processName = "FilterBiotic",
					functionName = "FilterBiotic",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						BioticData = "RedefineSpecCat"
					)
				),
				StationLengthDist = list(
					processName = "StationLengthDist",
					functionName = "StationLengthDist",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						BioticData = "FilterBiotic"
					),
					functionParameters = list(
						LengthDistType = "NormLengthDist"
					)
				),
				RegroupLengthDist = list(
					processName = "RegroupLengthDist",
					functionName = "RegroupLengthDist",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						LengthDist = "StationLengthDist"
					),
					functionParameters = list(
						LengthInterval = 1.0
					)
				),
				SplitNASCAssignment = list(
					processName = "SplitNASCAssignment",
					functionName = "SplitNASCAssignment",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						AcousticData = "FilterAcoustic",
						BioticData = "FilterBiotic"
					),
					functionParameters = list(
						Radius = 50.0
					)
				),
				TotalLengthDist = list(
					processName = "TotalLengthDist",
					functionName = "TotalLengthDist",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						ProcessData = "SplitNASCAssignment",
						LengthDist = "RegroupLengthDist"
					)
				),
				SplitNASC_BUNN = list(
					processName = "SplitNASC_BUNN",
					functionName = "SplitNASC",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						ProcessData = "SplitNASCAssignment",
						AcousticData = "FilterAcoustic",
						LengthDist = "TotalLengthDist",
						NASC = "SumNASC"
					),
					functionParameters = list(
						SpeciesTS = "2;22;SEI;20.0;-67.0;0.0/2;18;HVITTING;20.0;-67.0;0.0/2;31;TORSK;20.0;-66.0;0.0/2;30;HYSE;20.0;-65.0;0.0/2;28;;20.0;-67.0;0.0/2;24;KOLMULE;20.0;-67.0;0.0"
					)
				),
				SplitNASC_BUNN2 = list(
					processName = "SplitNASC_BUNN2",
					functionName = "SplitNASC",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						ProcessData = "SplitNASCAssignment",
						AcousticData = "FilterAcoustic",
						LengthDist = "TotalLengthDist",
						NASC = "SumNASC"
					),
					functionParameters = list(
						SpeciesTS = "52;22;SEI;20.0;-67.0;0.0/52;18;HVITTING;20.0;-67.0;0.0/52;31;TORSK;20.0;-66.0;0.0/52;30;HYSE;20.0;-65.0;0.0/52;28;;20.0;-67.0;0.0"
					)
				),
				CombineNASC_SEI = list(
					processName = "CombineNASC_SEI",
					functionName = "CombineNASC",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						NASC1 = "SplitNASC_BUNN",
						NASC2 = "SplitNASC_BUNN2",
						NASC3 = "SumNASC"
					),
					functionParameters = list(
						TargetAcoCat = "22"
					)
				),
				CombineNASC_TORSK = list(
					processName = "CombineNASC_TORSK",
					functionName = "CombineNASC",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						NASC1 = "SplitNASC_BUNN",
						NASC2 = "SplitNASC_BUNN2",
						NASC3 = "SumNASC"
					),
					functionParameters = list(
						TargetAcoCat = "31"
					)
				),
				CombineNASC = list(
					processName = "CombineNASC",
					functionName = "CombineNASC",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						NASC1 = "CombineNASC_SEI",
						NASC2 = "CombineNASC_TORSK"
					)
				),
				NASCToAcousticData = list(
					processName = "NASCToAcousticData",
					functionName = "NASCToAcousticData",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						AcousticData = "FilterAcoustic",
						NASC = "CombineNASC"
					)
				),
				WriteAcousticDataToXML = list(
					processName = "WriteAcousticDataToXML",
					functionName = "WriteAcousticDataToXML",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						AcousticData = "NASCToAcousticData"
					),
					functionParameters = list(
						Directory = "output/baseline"
					)
				)
			)
		), 
		description = "Split NASC"
	),
	#### LengthWeightRelationShip Template: Length Weight relationship: ####
	LengthWeightRelationShip = structure(
		list(
			Baseline = list(
				ReadProcessData = list(
					processName = "ReadProcessData",
					functionName = "ReadProcessData",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					)
				),
				ReadBioticXML = list(
					processName = "ReadBioticXML",
					functionName = "ReadBioticXML",
					processParameters = list(
						FileOutput = FALSE,
						BreakInGUI = FALSE
					)
				),
				FilterBiotic = list(
					processName = "FilterBiotic",
					functionName = "FilterBiotic",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						BioticData = "RedefineSpecCat"
					)
				),
				DefineStrata = list(
					processName = "DefineStrata",
					functionName = "DefineStrata",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						ProcessData = "ReadProcessData"
					),
					functionParameters = list(
						UseProcessData = FALSE
					)
				),
				DefineSweptAreaPSU = list(
					processName = "DefineSweptAreaPSU",
					functionName = "DefineSweptAreaPSU",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						ProcessData = "ReadProcessData",
						BioticData = "FilterBiotic"
					),
					functionParameters = list(
						Method = "Station"
					)
				),
				LengthWeightRelationship = list(
					processName = "LengthWeightRelationship",
					functionName = "LengthWeightRelationship",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						ProcessData = "ReadProcessData",
						BioticData = "FilterBiotic"
					)
				),
				WriteProcessData = list(
					processName = "WriteProcessData",
					functionName = "WriteProcessData",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					)
				)
			)
		), 
		description = "Length Weight relationship"
	),
	#### StationLengthDist Template: Station length distribution: ####
	StationLengthDist = structure(
		list(
			Baseline = list(
				ReadBioticXML = list(
					processName = "ReadBioticXML",
					functionName = "ReadBioticXML",
					processParameters = list(
						FileOutput = FALSE,
						BreakInGUI = FALSE
					)
				),
				FilterBiotic = list(
					processName = "FilterBiotic",
					functionName = "FilterBiotic",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						BioticData = "RedefineSpecCat"
					)
				),
				StationLengthDist = list(
					processName = "StationLengthDist",
					functionName = "StationLengthDist",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						BioticData = "FilterBiotic"
					),
					functionParameters = list(
						LengthDistType = "PercentLengthDist"
					)
				),
				RegroupLengthDist = list(
					processName = "RegroupLengthDist",
					functionName = "RegroupLengthDist",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						LengthDist = "StationLengthDist"
					),
					functionParameters = list(
						LengthInterval = 1.0
					)
				)
			)
		), 
		description = "Station length distribution"
	),
	#### DATRAS Template: DATRAS conversion: ####
	DATRAS = structure(
		list(
			Baseline = list(
				ReadBioticXML = list(
					processName = "ReadBioticXML",
					functionName = "ReadBioticXML",
					processParameters = list(
						FileOutput = FALSE,
						BreakInGUI = FALSE
					)
				),
				FilterBiotic = list(
					processName = "FilterBiotic",
					functionName = "FilterBiotic",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						BioticData = "RedefineSpecCat"
					)
				),
				DATRASConvert = list(
					processName = "DATRASConvert",
					functionName = "DATRASConvert",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					),
					functionInputs = list(
						BioticData = "FilterBiotic"
					)
				)
			),
			Analysis = list(
				prepareDATRAS = list(
					processName = "prepareDATRAS",
					functionName = "prepareDATRAS",
					processParameters = list(
						FileOutput = TRUE,
						BreakInGUI = FALSE
					)
				)
			)
		), 
		description = "DATRAS conversion"
	),
	#### UserDefined Template: User defined (empty models): ####
	UserDefined = structure(
		list(), 
		description = "User defined (empty models)"
	), 
	#### Test template for StoX 3.0: ####
	Test3.0 = structure(
		list(
			Baseline = list(
				ReadBiotic = list(
					processName = "ReadBiotic",
					functionName = "RstoxData::ReadBiotic"
				),
				DefineStratum = list(
					processName = "DefineStrata",
					functionName = "RstoxBase::DefineStrata", 
					processParameters = list(
					    showInMap = TRUE
					)
				),
				StratumArea = list(
					processName = "StratumArea",
					functionName = "RstoxBase::StratumArea",
					functionInputs = list(
						StratumPolygon = "DefineStratum"
					)
				)
			)
		), 
		description = "Test template for StoX 3.0"
	)
)