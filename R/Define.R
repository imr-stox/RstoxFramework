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
#' @import data.table
#' 
DefineIndividualWeightGram <- function(BioticData, individualName = "individual", ...) {
    if(any(length(BioticData[[individualName]]) == 0)) {
        stop("'individual' is not present in the data.")
    }   

    temp <- BioticData[[individualName]]

    temp$IndividualWeightGram <- temp$individualweight * 1000

    BioticData[[individualName]] <- temp

    BioticData	
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
#' @import data.table
#' 
DefineLengthCentimeter <- function(BioticData, individualName = "individual", ...) {
    if(any(length(BioticData[[individualName]]) == 0)) {
        stop("'individual' is not present in the data.")
    }    

    temp <- BioticData[[individualName]]

    temp$LengthCentimeter <- temp$length * 100

    BioticData[[individualName]] <- temp

    BioticData
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
#' @import data.table
#' 
DefineSpecCat <- function() {
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
#' @import data.table
#' 
MergeAgeDeterminationToIndividual <- function(BioticData, 
    individualName = "individual",
	ageDeterminationName = "agedetermination",
    ...) {

    if(any(length(BioticData[[individualName]]) == 0, length(BioticData[[ageDeterminationName]]) == 0)) {
        stop("'individual' and/or 'agedetermination' are not present in the data.")
    }
    
    # Merge individual and agedetermination:
    temp <- merge(BioticData[[individualName]], BioticData[[ageDeterminationName]], by = commonVar, all = TRUE)
    
    # Warning if there are more tmhan one age reading and preferredagereading is NA:
    temp$NumberOfAgeReadings <- table(apply(temp[, commonVar], 1, paste, collapse="_"))
    missing <- temp$NumberOfAgeReadings > 1 & is.na(temp$preferredagereading)
    if(any(missing)) {
        missingInfo <- paste(commonVar, BioticData[[individualName]][, commonVar][which(missing),], collapse=", ", sep=" = ")
        warning("The following individuals had several age readings but no preferred age reading. The first was chosen:\n", missingInfo)
    }
    
    # Insert 1 for missing preferredagereading:
    temp$PreferredAgeReadingTemp <- replace(temp$preferredagereading, is.na(temp$preferredagereading), values=1)
    
    # Pick out the preffered age readings:
    isPreferred <- temp$PreferredAgeReadingTemp == temp$agedeterminationid
    # Do not remove the individuals wich do not have age determination:
    isPreferred <- replace(isPreferred, is.na(isPreferred), values=TRUE)
    # Remove the non-preferred age readings:
    temp <- subset(temp, isPreferred)
    
    # Remove the temporary preferredagereading (since we inserted ones above):
    temp$NumberOfAgeReadings <- NULL
    temp$PreferredAgeReadingTemp <- NULL
    
    # Replace the individual table by the merged individual and agedetermination table, and return the biotic data:
    BioticData[[individualName]] <- temp
    BioticData
}


