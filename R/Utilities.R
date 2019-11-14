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
    
    # Merge the data.tables:
    out <- merge(x[,xvar, with=FALSE], y[,yvar, with=FALSE], all=TRUE, by=thisKeys)
    
    if(keys.out) {
        list(data=out, keys=thisKeys)
    }
    else {
        out
    }
}

# Function to get the first element of class(x):
firstClass <- function(x) {
    class(x)[1]
}

# Function to select valid elements by name
selectValidElements <- function(x, names) {
    validNames <- intersect(names, names(x))
    x[validNames]
}

# Function to check whether a data table is rugged:
isDataTableRugged <- function(x) {
    lens <- sapply(x, lengths)
    all(lens == lens[1])
}

# Function to remove all empty elements of a data.table:
replaceEmptyInDataTable = function(DT, replace = NA) {
    for (i in names(DT)) {
        DT[lengths(get(i)) == 0, (i):= replace]
    }
    DT     
}

# Function to expand a data table so that the cells that are vectors are transposed and the rest repeated to fill the gaps:
expandDT <- function(DT, toExpand = NULL) {
    # Set the columns to expand:
    if(length(toExpand) == 0) {
        lens <- lapply(DT, lengths)
        lensLargerThan1 <- sapply(lens, function(l) any(l > 1))
        toExpand <- names(DT)[lensLargerThan1]
    }

    if(length(toExpand)) {
        DT <- do.call(
            cbind, 
            c(
                list(
                    DT[rep(1:.N, lengths(get(toExpand[1]))), !toExpand, with = FALSE]
                ), 
                lapply(toExpand, function(x) DT[, unlist(get(x))])
            )
        )
    }
    
    DT
}

# Function to create a rectangular data table from a data table which may contain empty cells and vectors in cells (all vectors must have equal length for one row):
flattenDataTable <- function(x, replace = NA) {
    
    # Replace all empty with NA
    x <- replaceEmptyInDataTable(x, replace = replace)

    x <- expandDT(x)
    # AcousticPSU:
    #     Stratum_PSU [Stratum, PSUID (vector), PSUName (vector)]
    #     PSU_EDSU [PSUID, EDSU (vector)]
    # SweptAreaPSU:
    #     Stratum_PSU [Stratum, PSUID (vector), PSUName (vector)]
    #     PSU_Station [PSUID, Station (vector)]
    # Assignment:
    #     Assignment [PSUID, LayerID, Station (vector), StationWeight (vector)]
    # AcousticLayer:
    #     AcousticLayer [LayerID, LayerName, MinRange, MaxRange]
    # SweptAreaLayer:
    #     SweptAreaLayer [LayerID, LayerName, MinDepth, MaxDepth]
}

# Function to convert data.table to fixed width:
fixedWidthDataTable <- function(x) {
    # First convert all columns to character:
    x <- x[, (colnames(x)) := lapply(.SD, as.character), .SDcols = colnames(x)]
    # Get the maximum number of characters of the columns:
    suppressWarnings(maxNcharColumns <- sapply(x, function(x) max(nchar(x), na.rm = TRUE)))
    # Get the number of characters of the column names:
    ncharColumnNames <- nchar(names(x))
    # Get maximum of the two:
    maxNchar <- pmax(maxNcharColumns, ncharColumnNames, na.rm = TRUE)
    # Create the code to fixed width the columns:
    maxNcharString <- paste0("%", maxNchar, "s")
    # Fixed width:
    out <- data.table::as.data.table(mapply(sprintf, maxNcharString, x))
    names(out) <- names(x)
    out
}
