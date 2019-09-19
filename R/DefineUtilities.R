
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
SplitCommaSeparatedString <- function(x) {
	trimws(unlist(strsplit(x, ",")))
}
