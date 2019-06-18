##################################################
##################################################
#' Read XML files
#' 
#' This function reads multiple XML files and converts to the specified format.
#' 
#' @param FileNames A vector of paths to XML files.
#' @param format    A string specifying the format to read the files in (converted if possible).
#' 
#' @details
#' The function \code{\link[RNMDAPI]{readNMDxmlFile}} detects the format and reads a biotic, acoustic or landing XML file, and is used to read multiple files of the same type. The output is converted to the specified format, and data from all files are rbinded (stacked vertically).
#' 
#' @return
#' A list of data.tables in the specified format.
#' 
#' @examples
#' x <- 1
#' 
#' @seealso \code{\link[RNMDAPI]{readNMDxmlFile}}.
#' 
#' @import data.table
#' @importFrom RNMDAPI readNMDxmlFile
#'
readNMDxmlFiles <- function(
    FileNames, 
    format = "biotic_3.0") {
    
    # Define a function that rbinds the ind'th element of each dataset:
    rbindlist_oneElement <- function(ind, x) {
        # Get the ind'th element of each list returned from readNMDxmlFile:
        temp <- lapply(x, "[[", ind)
        # Fast rbind using data.table:
        data.table::rbindlist(temp)
    }
    
    # Read the XML files in a loop:
    out <- lapply(FileNames, RNMDAPI::readNMDxmlFile)
    
    # Convert each file to the specified data format, biotic or acoustic (this function must detect the format that was read):
    out <- lapply(out, convertBiotic, format = format)
    
    # Rbind the levels across the files:
    if(length(out) > 1){
        # Get the names of the first element:
        names_out <- names(out[[1]])
        
        # Rbind all fish stations, catch samples etc across files:
        ind <- seq_along(out[[1]])
        out <- lapply(ind, rbindlist_oneElement, x=out)
        
        # Set the names of the output:
        names(out) <- names_out
    }
    else{
        out <- out[[1]]
    }
    
    return(out)
}

# Small function to check whether the first letter is upper case, used to identify StoX functions and parameters:
isUpperFirstLetter <- function(x) {
    grepl("^[[:upper:]]+", x)
}

# Function for checking the meta information stored in a function:
checkStoXmetaData <- function(functionName) {
    # Get StoX parameter names from the formals:
    parameters <- formals(functionName)
    parameterNames = names(parameters)
    StoXparameterNames = parameterNames[isUpperFirstLetter(parameterNames)]
    
    # Get StoX parameters from the return from the function:
    meta <- do.call(functionName, list())
    
    # Check that the StoX functions specified in the meta information match the actual formals:
    onlyInMeta <- setdiff(meta$StoXparameterNames, StoXparameterNames)
    onlyInFormals <- setdiff(meta$StoXparameterNames, StoXparameterNames)
    
    if(length(onlyInMeta)) {
        message("The following parameters are not used by the function ", functionName, ": ", paste(onlyInMeta, sep=", "), ".")
        return(FALSE)
    }
    if(length(onlyInFormals)) {
        message("The following parameters are missing in the meta information of the function ", functionName, ": ", paste(onlyInFormals, sep=", "), ".")
        return(FALSE)
    }
    
    # If no errors, return TRUE
    TRUE
}

