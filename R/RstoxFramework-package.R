#' The Engine of StoX
#'
#' The framwork of StoX >= 3.0.
#'
#' The RstoxFramework package is the engine of the stock assesment utility StoX, which is an open source application fo acoustic-trawl and swept-area survey estimation.
#'
#' The package creates an evironment containing the StoX project(s) in separate environments. Each StoX project consists of a list of data tables holding e.g. biotic and acoustic data, filtered versions of the data, strata system, definitios of primary sampling units, accompanied by a list of specifications of the StoX processes comprising the StoX project. A StoX process is an R function taking as input the project name, the input data and parameters used by the function.
#'
#' The package replaces the old Java library in StoX versions prior to StoX 4.0.
#' @docType package
#' @name RstoxFramework
#'
"_PACKAGE"

# Global variables
utils::globalVariables(c(
	 "RstoxFrameworkEnv", ":=", ".", "..PSU", "..activeProcessID", "..clickPointNames",
	 "..coordinateNames", "..functionInputs", "..functionName", "..functionParameters",
	 "..infoToKeep", "..processDirty", "..newProcessName", "CruiseKey", "Latitude", "Latitude2",
	 "LogOrigin", "LogOrigin2", "Longitude", "Longitude2", "PSU", "atRemove", "canShowInMap",
	 "filePahts", "functionName", "functionOutputDataType", "hasBeenRun", "hasProcessData",
	 "modelName", "processDirty", "name", "possibleValues", "processID", "projectPath", "value",
	 "..EDSUInfoToKeep", "..haulInfoToKeep", "..ind", "..propertyDirty", "..stationInfoToKeep",
	 "..validInd", "BootstrapID", "JavaJEXL2R", "ProcessName", "ResampleFunction", "Stratum",
	 "col2rgb", "colorRampPalette", "dataTable2SpatialPolygonsDataFrame", "dataType",
	 "functionArguments", "packageVersion", "processIndex", "processName",
	 "readProjectDescriptionXML", "resampledCountWithUniqueName", "writeProjectXML"))

.onLoad <- function(libname, pkgname) {
	# Initiate the RstoxFramework environment:
	initiateRstoxFramework()
} 

# Packages to import to NAMESPACE (typically packages which are used extensively or packcages with special syntax that requires import, e.g, data.table)
#' @import data.table
NULL

