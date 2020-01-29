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
    # Return immediately if x has length 0:
    if(nrow(x) == 0) {
        return(FALSE)
    }
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
    
    # Return immediately if x has length 0:
    if(length(x) == 0) {
        return(x)
    }
    
    # Replace all empty with NA
    x <- replaceEmptyInDataTable(x, replace = replace)

    x <- expandDT(x)
}

# Function to convert data.table to fixed width:
fixedWidthDataTable <- function(x, columnSeparator = " ", lineSeparator = NULL, na = "-", list.pretty = FALSE) {
    # Return immediately if x has length 0:
    if(length(x) == 0) {
        return(x)
    }
    
    # First convert all columns to character:
    x <- x[, (colnames(x)) := lapply(.SD, as.character), .SDcols = names(x)]
    
    # Replace all NA with the user specified na:
    x[is.na(x)] <- na
    
    # Add the column names:
    out <- rbindlist(list(structure(as.list(names(x)), names = names(x)), x))
    # Left pad with space:
    out <- out[, lapply(.SD, function(y) stringr::str_pad(y, max(nchar(y)), pad = " "))]
    
    # Collapse to lines:
    out <- out[, do.call(paste, c(.SD, sep = columnSeparator)), .SDcols = names(x)]
    
    # Collapse the lines if requested:
    if(length(lineSeparator)) {
        out <- paste(out, collapse = lineSeparator)
    }
    else if(list.pretty) {
        out <- list(out)
    }
    
    out
}

# Function to extract the trailing integer of a string (vector):
getTrailingInteger <- function(x, integer = TRUE) {
    # Get the trailing numerics:
    trailing <- stringr::str_extract(x, "[^[a-z]]*$")
    
    # Convert to numeric if specified:
    if(integer) {
        as.integer(trailing)
    }
    else {
        trailing
    }
}

# Function to generate a new name in the sequence of names starting with the prefix:
getNewDefaultName <- function(names, prefix) {
    
    # Find the names starting with the prefix:
    startsWithProcess_Prefix <- which(startsWith(names, prefix))
    # Get the trailing integers of the names starting with the prefix:
    trailingIntegerString <- getTrailingInteger(names[startsWithProcess_Prefix], integer = FALSE)
    trailingInteger <- getTrailingInteger(names[startsWithProcess_Prefix])
    # Verify that the number of characters equals the sum of the prefix and the number of characters of the numeric string:
    hasCorrectNumberOfCharacters <- nchar(names[startsWithProcess_Prefix]) == nchar(prefix) + nchar(trailingIntegerString)
    
    if(length(hasCorrectNumberOfCharacters) == 0) {
        newInteger <- 1
    }
    else {
        # Get new integer as one more than the maximum integer:
        newInteger <- max(trailingInteger[hasCorrectNumberOfCharacters]) + 1
    }
    
    # Add 1 to the latest integer:
    newName <- paste0(prefix, newInteger)
    
    newName
    #currentMax <- 1
    #detectedTheLast <- TRUE
    #
    #while(detectedTheLast) {
    #    # Create a vector of potential names, pasting the prefix and a sequence of integer:
    #    sequence <- currentMax - 1 + seq_len(length)
    #    potentialNames <- paste0(prefix, sequence)
    #    # Fin the latest name:
    #    latest <- max(which(names %in% potentialNames))
    #    # Break the loop 
    #}
    #
    ## Create a vector of potential names, pasting the prefix and a sequence of integer:
    #potentialNames <- paste0(prefix, seq_len(length(names) * lengthFact))
    ## Fin the latest name:
    #latest <- max(which(names %in% potentialNames))
    #
    ## Return the new default name, as the next in the sequence:
    #potentialNames[latest + 1]
}


json_str <- '{
  "negate": true,
  "linkoperator": "&",
  "group": [
    {
      "linkoperator": "|",
      "group": [
        {
          "negate": false,
          "field": "age",
          "operator": "!=",
          "value": 1
        },
        {
          "negate": false,
          "field": "sttype",
          "operator": "<",
          "value": 5
        }
      ]
    },
    {
      "negate": false,
      "field": "name",
      "operator": "%in%",
      "value": [1,2,3]
    }
  ]
}'

j2expr = function(json_str) {
  #convert from json to list
  json_l <- jsonlite::fromJSON(json_str, simplifyVector = F)
  # convert from list to expression 
  l2expr = function(l) {
    ret <- NULL
    negate <- isTRUE(l$negate)
    needpar <- negate
    if(any(names(l) == "linkoperator")) { 
      # group found
      needpar <- TRUE
      link <- paste('', l$linkoperator, '')
      ret <- paste(lapply(l$group, l2expr), collapse=link)
    } else { 
      # expression found
      val <- l$value
      needQuote <- is.character(val)
      if(needQuote) {
        val <- paste0('\'', val, '\'')
      }
      if(is.list(val)) {
        val = paste0('c(', paste(val, collapse=','), ')')
      }
      ret <- paste(l$field, l$operator, val)
    }
    if(needpar) {
      ret <- paste0('(', ret, ')')
    }
    if(negate) {
      ret <- paste0('!', ret)
    }
    return(ret)
  }
  l2expr(json_l)
}

#expr <- j2expr(json_str)

#setRefClass("Log", fields=list(entries="list"))
#log <- new("Log",entries=list())
#writeToLog = function(level, s, lg) {
#  lg$entries[[length(lg$entries) + 1]] <- paste(level, s);
#}
#writeToLog(1, 'start', l)

#convert from expression to list 
expr2j = function(expr) {
  expr2l = function(expr, parent, recLevel, log) {
    res <- NULL;
    if(is.null(parent)) {
      parent <- list(listOperator = '&', rules = list())
      res <- parent;
    }
    orFoundAtLevel0 <- FALSE
    andFoundAtLevel0 <- FALSE
    level <- 0
    for(c in unlist(strsplit(expr, ''))) {
      andFoundAtLevel0 <- andFoundAtLevel0 | level == 0 & c == '&'
      orFoundAtLevel0 <- orFoundAtLevel0 | level == 0 & c == '|'
      if (c == '(') {
        level <- level + 1;
      } else if (c == ')') {
        level <- level -1;
      }
    }
    splitOperator <- NULL
    groupsList <- NULL
    if(orFoundAtLevel0) {
      splitOperator <- '|'
    } else if(andFoundAtLevel0) {
      splitOperator <- '&'
    }
    expr <- trimws(expr)
  #  writeToLog(recLevel, paste("afterparse-1:", "class:", class(expr), "expr:", expr), log)
    if(!is.null(splitOperator)) {
      res$linkoperator = splitOperator
      groupsList <- unlist(strsplit(expr, splitOperator, fixed=T))
      #res$group <- lapply(groupsList, expr2l, group, recLevel + 1)
      res$group <- list()
      for(grp in groupsList) {
        res$group[[length(res$group) + 1]] <- expr2l(grp, parent, recLevel + 1, log) 
      }
    } else {
      negate <- startsWith(expr, '!')
      if(isTRUE(negate)) {
        expr <- substr(expr, 2, nchar(expr))
      }
      expr <- trimws(expr)
      if(startsWith(expr, '(') & endsWith(expr, ')')) {
        # rid off surrounding parenthesis (..)
        expr <- substr(expr, 2, nchar(expr) - 1)
        res <- expr2l(expr, parent, recLevel + 1, log)
        res$negate <- negate
      } else {
        # expression field op value
        s <- unlist(strsplit(gsub('([.^s]*)\\s*(!=|==|<|<=|>|>=|%in%)\\s*([.^s]*)', '\\1;\\2;\\3', expr), ';'))
        if(length(s) == 3) {
          #valid syntax: field op value
          val <- eval(parse(text=s[[3]]))
          res <- list(field=s[[1]], operator=s[[2]], value=val)
          if(!is.null(parent)) {
            parent$group[[length(parent$group) + 1]] <- res
          }
        }   
      }
    }
    res
  }
  json_l <- expr2l(expr, NULL, 0, log)
  jsonlite::toJSON(json_l, auto_unbox = T, pretty=T)
}
j2expr(expr2j(expr))


