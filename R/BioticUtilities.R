#*********************************************
#*********************************************
#' Merge two data tables with all=TRUE and the specified columns and keys.
#' 
#' Merges two data tables (see \code{\link[data.table]{data.table}}) with all=TRUE, while keeping only columns of the data tables with names intersecting \code{var}, and using the intersect of \code{keys} and the names of the data tables as the 'by' argument.
#' 
#' @param x,y		Two data tables to be merged.
#' @param var		A character vector of names of the columns to keep while merging.
#' @param keys		A character vector of names of the columns to merge by (see the \code{by} argument in \code{\link[data.table]{merge}}).
#' @param keys.out	Logical: If TRUE return a list with the keys used and the merged data.
#'
#' @return A merged data table.
#' 
#' @noRd
#'
#' @import data.table
#' 
merge2 <- function(x, y, var=c("distance", "weight", "lengthsampleweight", "length", "lengthresolution"), keys=c("cruise", "serialnumber", "samplenumber", "SpecCat"), keys.out=FALSE) {
    # Get the keys common for the two data tables:
    commonVar <- intersect(names(x), names(y))
    thisKeys <- intersect(keys, commonVar)
    # Get the variables requested for x and y:
    xvar <- intersect(names(x), c(var, keys))
    yvar <- intersect(names(y), c(var, keys))
    # Remove variables named identically ('weight' in biotic v1.4):
    yvar <- setdiff(yvar, xvar)
    # Add the keys:
    xvar <- unique(c(thisKeys, xvar))
    yvar <- unique(c(thisKeys, yvar))
    browser()
    # Merge the data.tables:
    out <- merge(x[,xvar, with=FALSE], y[,yvar, with=FALSE], all=TRUE, by=thisKeys)
    
    if(keys.out) {
        list(data=out, keys=thisKeys)
    }
    else {
        out
    }
}


#*********************************************
#*********************************************
#' Get the keys of a biotic XSD
#' 
#' This function gets all keys of a biotic XSD used by StoX.
#' 
#' @param xsd   A string identifying the XSD
#'
#' @return A merged data table.
#' 
#' @noRd
#' 
getBioticKeys <- function(xsd = "3.0") {
    unlist(Rstox::readHIXSD(xsd = xsd, xsdtype = "biotic")$attrs_required)
}


#*********************************************
#*********************************************
#' Get the levels of a biotic XSD
#' 
#' This function gets all keys of a biotic XSD used by StoX.
#' 
#' @param xsd   A string identifying the XSD
#'
#' @return A merged data table.
#' 
#' @noRd
#' 
getBioticLevels <- function(xsd = "3.0") {
    out <- unlist(Rstox::readHIXSD(xsd = xsd, xsdtype = "biotic")$levels)
    if(length(out) == 0) {
        warning("Re-write the readHIXSD() to return the levels as an element")
        out <- unique(unlist(Rstox::readHIXSD(xsd = xsd, xsdtype = "biotic")$level))
    }
    out
}

