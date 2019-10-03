##################################################
##################################################
#' Some title
#' 
#' Some description
#' 
#' @param parameterName Parameter descrption.
#' 
#' @details
#' This function is awesome and does excellent stuff.
#' 
#' @return
#' A data.table is returned with awesome stuff.
#' 
#' @examples
#' x <- 1
#' 
#' @seealso \code{\link[roxygen2]{roxygenize}} is used to generate the documentation.
#' 
#' @export
#' 
DATRASConvert <- function() {
	# Use @noRd to prevent rd-files, and @inheritParams runBaseline to inherit parameters (those in common that are not documented) from e.g. getBaseline. Use @section to start a section in e.g. the details. Use @inheritParams runBaseline to inherit parameters from e.g. runBaseline(). Remove the @import data.table for functions that do not use the data.table package, and add @importFrom packageName functionName anotherFunctionName for importing specific functions from packages. Also use the packageName::functionName convention for the specifically imported functions.
}


##################################################
##################################################
#' Some title
#' 
#' Some description
#' 
#' @param parameterName Parameter descrption.
#' 
#' @details
#' This function is awesome and does excellent stuff.
#' 
#' @return
#' A data.table is returned with awesome stuff.
#' 
#' @examples
#' x <- 1
#' 
#' @seealso \code{\link[roxygen2]{roxygenize}} is used to generate the documentation.
#' 
#' @export
#' 
FilterAcoustic <- function() {
	# Use @noRd to prevent rd-files, and @inheritParams runBaseline to inherit parameters (those in common that are not documented) from e.g. getBaseline. Use @section to start a section in e.g. the details. Use @inheritParams runBaseline to inherit parameters from e.g. runBaseline(). Remove the @import data.table for functions that do not use the data.table package, and add @importFrom packageName functionName anotherFunctionName for importing specific functions from packages. Also use the packageName::functionName convention for the specifically imported functions.
}


##################################################
##################################################
#' Some title
#' 
#' Some description
#' 
#' @param parameterName Parameter descrption.
#' 
#' @details
#' This function is awesome and does excellent stuff.
#' 
#' @return
#' A data.table is returned with awesome stuff.
#' 
#' @examples
#' x <- 1
#' 
#' @seealso \code{\link[roxygen2]{roxygenize}} is used to generate the documentation.
#' 
#' @export
#' 
FilterBiotic <- function() {
	# Use @noRd to prevent rd-files, and @inheritParams runBaseline to inherit parameters (those in common that are not documented) from e.g. getBaseline. Use @section to start a section in e.g. the details. Use @inheritParams runBaseline to inherit parameters from e.g. runBaseline(). Remove the @import data.table for functions that do not use the data.table package, and add @importFrom packageName functionName anotherFunctionName for importing specific functions from packages. Also use the packageName::functionName convention for the specifically imported functions.
}


##################################################
##################################################
#' Some title
#' 
#' Some description
#' 
#' @param parameterName Parameter descrption.
#' 
#' @details
#' This function is awesome and does excellent stuff.
#' 
#' @return
#' A data.table is returned with awesome stuff.
#' 
#' @examples
#' x <- 1
#' 
#' @seealso \code{\link[roxygen2]{roxygenize}} is used to generate the documentation.
#' 
#' @export
#' 
FilterLanding <- function() {
	# Use @noRd to prevent rd-files, and @inheritParams runBaseline to inherit parameters (those in common that are not documented) from e.g. getBaseline. Use @section to start a section in e.g. the details. Use @inheritParams runBaseline to inherit parameters from e.g. runBaseline(). Remove the @import data.table for functions that do not use the data.table package, and add @importFrom packageName functionName anotherFunctionName for importing specific functions from packages. Also use the packageName::functionName convention for the specifically imported functions.
}


##################################################
##################################################
#' Read acoustic XML files
#' 
#' This function reads multiple acoustic XML files.
#' 
#' @param acousticFormat    The format of the acoustic data, referring to the List User File (LUF) naming used by the Large Scale Survey System (LSSS) ************** refer to ICES instead!!!!!!!!!!!!!!
#' @inheritParams readNMDxmlFiles
#' 
#' @details
#' This function is awesome and does excellent stuff.
#' 
#' @return
#' A list of data.tables in the specified format.
#' 
#' @examples
#' x <- 1
#' 
#' @seealso \code{\link[RstoxData]{readNMDxmlFile}}.
#' 
#' @export
#' 
ReadAcousticXML <- function(
    FileNames = NA, 
    acousticFormat = "25") {
    
    # Read the acoustic files:
    acousticFormat <- paste("acoustic", acousticFormat, sep = "_")
    out <- readNMDxmlFiles(FileNames, format = acousticFormat)
    
    # Return the acoustic data:
    out
}


##################################################
##################################################
#' Some title
#' 
#' Some description
#' 
#' @param FileNames One or more file paths given either as (1) a vector, (2) a JSON string representing a vector, or (3) a comma or semicolon separated string of file paths.
#' 
#' @details
#' This function is awesome and does excellent stuff.
#' 
#' @return
#' A data.table is returned with awesome stuff.
#' 
#' @examples
#' x <- 1
#' 
#' @seealso \code{\link[roxygen2]{roxygenize}} is used to generate the documentation.
#' 
#' @export
#' 
ReadBiotic <- function(FileNames) {
    # NOTE: Fix the xsdobjects problem and remove the following line:
    library(RstoxData)
    out <- lapply(FileNames, RstoxData::readXmlFile)
    names(out) <- basename(FileNames)
    out
}


getFileName <- function(FileName) {
    
}




##################################################
##################################################
#' Read biotic XML files
#' 
#' This function reads multiple biotic XML files and adds the columns "LengthCentimeter" and "IndividualWeightGram", merges age readings to the individual data.table, and adds the SpecCat as specified by the user.
#' 
#' @param bioticFormat  The format given as a string or decimal number such as "3.0" or 3.
#' @param SpecCatMethod The method used for defining the SpecCat variable, which is the species variable used by StoX. The \code{SpecCatMethod} parameter has the following three possible values: (1) "SelectVar", which copies the column \code{SpecVarBiotic} of the "catchsample" table to the SpecCat column of the tables "catchsample", "individual" and all non-empty tables at lower levels. (2) "Expression", which ***************. (3) "ResourceFile", which requires the parameters \code{FileName}, \code{SpecVarBiotic}, \code{SpecVarRef} and \code{SpecCatRef} to be set. See Details.
#' @param SpecCat       An expression indicating how to create the SpecCat********************.
#' @param FileName      The name of the file holding a table of at least two columns, (1) the species variable in a column named by \code{SpecVarRef}, corresponding to the field named by \code{SpecVarBiotic} in the biotic data, and (2) a column named by \code{SpecCatRef} defining the SpecCat variable.
#' @param SpecVarBiotic The name of the field on the biotic data to match with the column named by \code{SpecVarRef} in \code{FileName}.
#' @param SpecVarRef    The name of the column of \code{FileName} which should be matched with the field named by \code{SpecVarBiotic} in the biotic data.
#' @param SpecCatRef    The name of the column of \code{FileName} defining the SpecCat.
#' @inheritParams readNMDxmlFiles
#' 
#' @details
#' This function is awesome and does excellent stuff.
#' 
#' @return
#' A list of data.tables in the specified format.
#' 
#' @examples
#' x <- 1
#' 
#' @seealso \code{\link[RstoxData]{readNMDxmlFile}}.
#' 
#' @export
#' 
ReadBioticXML <- function(
    FileNames = NA, 
    SpecCatMethod = "ResourceFile", 
    SpecCat = NULL, 
    SpecVarBiotic = "commonname", 
    SpecVarRef = NULL, 
    SpecCatRef = NULL, 
    bioticFormat = "3.0") {
    
    
    
    # Read the biotic files:
    bioticFormat <- paste("biotic", bioticFormat, sep = "_")
    out <- readNMDxmlFiles(FileNames, format = bioticFormat)
    
    # Add required columns:
    out <- DefineLengthCentimeter(out)
    out <- DefineIndividualWeightGram(out)
    out <- MergeAgeDeterminationToIndividual(out)
    out <- DefineSpecCat(
        out, 
        SpecCatMethod = SpecCatMethod, 
        SpecCat = SpecCat, 
        SpecVarBiotic = SpecVarBiotic, 
        SpecVarRef = SpecVarRef, 
        SpecCatRef = SpecCatRef
    )
    
    # Return the biotic data:
    out
}


##################################################
##################################################
#' Read landing XML files
#' 
#' This function reads multiple landing XML files.
#' 
#' @param landingFormat    The format of the landing data.
#' @inheritParams readNMDxmlFiles
#' 
#' @details
#' This function is awesome and does excellent stuff.
#' 
#' @return
#' A list of data.tables in the specified format.
#' 
#' @examples
#' x <- 1
#' 
#' @seealso \code{\link[RstoxData]{readNMDxmlFile}}.
#' 
#' @export
#' 
ReadLandingXML <- function(
    FileNames = NA, 
    landingFormat = NULL) { # Add the landing format!!!!!!!!!!!!!!!!!!!!!!
    
    # Read the landing files:
    landingFormat <- paste("landing", landingFormat, sep = "_")
    out <- readNMDxmlFiles(FileNames, format = landingFormat)
    
    # Return the landing data:
    out
}


##################################################
##################################################
#' Some title
#' 
#' Some description
#' 
#' @param parameterName Parameter descrption.
#' 
#' @details
#' This function is awesome and does excellent stuff.
#' 
#' @return
#' A data.table is returned with awesome stuff.
#' 
#' @examples
#' x <- 1
#' 
#' @seealso \code{\link[roxygen2]{roxygenize}} is used to generate the documentation.
#' 
#' @export
#' 
WriteAcousticDataToXML <- function() {
	# Use @noRd to prevent rd-files, and @inheritParams runBaseline to inherit parameters (those in common that are not documented) from e.g. getBaseline. Use @section to start a section in e.g. the details. Use @inheritParams runBaseline to inherit parameters from e.g. runBaseline(). Remove the @import data.table for functions that do not use the data.table package, and add @importFrom packageName functionName anotherFunctionName for importing specific functions from packages. Also use the packageName::functionName convention for the specifically imported functions.
}


