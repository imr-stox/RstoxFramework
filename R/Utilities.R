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



# Function to create a rectangular data table from a data table which may contain empty cells and vectors in cells (all vectors must have equal length for one row):
flattenDataTable <- function(x, replace = NA) {
    
    # Return immediately if x has length 0:
    if(length(x) == 0) {
        return(x)
    }
    
    # Replace all empty with NA
    x <- replaceEmptyInDataTable(x, replace = replace)

    x <- RstoxBase::expandDT(x)
}

#' Function to convert data.table to fixed width:
#' 
#' @param x The table to modify.
#' @param columnSeparator The string to separate columns by, defaulted to a single space. 
#' @param lineSeparator The string to separate lines by, defaulted to a NULL, which keeps the output as a vector of strings.
#' @param na The string to replace NAs by, defaulted to "-".
#' @param list.pretty Logical: If TRUE wrap the output in a list in the case that \code{pretty} is TRUE.
#' 
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
    #trailing <- stringr::str_extract(x, "[^[a-z]]*$")
    trailing <- stringr::str_extract(x, "\\-*\\d+\\.*\\d*")
    
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
    
    if(length(names)) {
        # Find the names starting with the prefix:
        startsWithProcess_Prefix <- which(startsWith(names, prefix))
        # Get the trailing integers of the names starting with the prefix:
        trailingIntegerString <- getTrailingInteger(names[startsWithProcess_Prefix], integer = FALSE)
        trailingInteger <- getTrailingInteger(names[startsWithProcess_Prefix])
        # Verify that the number of characters equals the sum of the prefix and the number of characters of the numeric string:
        hasCorrectNumberOfCharacters <- nchar(names[startsWithProcess_Prefix]) == nchar(prefix) + nchar(trailingIntegerString)
    }
    else {
        hasCorrectNumberOfCharacters <- NULL
    }
    
    if(length(hasCorrectNumberOfCharacters) == 0) {
        newInteger <- 1
    }
    else {
        # Get new integer as one more than the maximum integer:
        newInteger <- max(trailingInteger[hasCorrectNumberOfCharacters]) + 1
    }
    
    # Add 1 to the latest integer:
    newName <- paste0(prefix, newInteger)
    
    return(newName)
}

# Function to convert from json to R expression:
#' 
#' @export
#' 
json2expression <- function(json) {
    l <- parseParameter(json, simplifyVector = FALSE)
    #l <- jsonlite::fromJSON(json, simplifyVector = FALSE)
    list2expression(l)
}


# Function to convert from R list to R expression:
#' 
#' @export
#' 
list2expression <- function(l, parentHasSiblings=FALSE) {
    # Declare the resulting expression
    result <- NULL
    # If the current rules or expression should be negated, we need to enclose the expression in paretheses:
    negate <- isTRUE(l$negate)
    needParentheses <- negate
    
    # Identify rules by the condition:
    if(any(names(l) == "condition")) { 
        # Rules need parentheses, and the link is padded by spaces for readability:
        needParentheses <- needParentheses || parentHasSiblings
        link <- paste('', l$condition, '')
        
        # Recurse into the children:
        result <- paste(lapply(l$rules, list2expression, parentHasSiblings=length(l$rules) > 1), collapse = link)
    } 
    # Otherwise build the expression:
    else {
        # Extract the value for some processing:
        value <- l$value
        
        # If the value is a character, pad with quotation marks:
        if(length(value) && is.character(value[[1]])) {
            # Replace the string "NA" with NA:
            areNAString <- sapply(value, "%in%", "NA")
            if(any(areNAString)) {
                value[areNAString] <- rep(list(NA), sum(areNAString))
            }
            
            # Add quotes:
            value[!areNAString] <- lapply(value[!areNAString], function(x) paste0("\"", x, "\""))
        }
        
        # If there is one single value, and this is NA, change operator to %in%, with a warning:
        if(length(value) == 1 && is.na(value[[1]])) {
            if(l$operator %in% "==") {
                warning("StoX: Operator cannot be == when extracting NAs. Changed from == to %in% for ", l$field, l$operator, value)
                l$operator <- "%in%"
            }
            else if(l$operator %in% "!=") {
                warning("StoX: Operator cannot be != when excluding NAs. Changed from != to %notin% for ", l$field, l$operator, value)
                l$operator <- "%notin%"
            }
        }
        if(length(value) > 1 && l$operator %in% "==") {
            warning("StoX: The operator == cannot be used with multiple reference values, and was replaced by %in%")
            l$operator <- "%in%"
        }
        if(length(value) > 1 && l$operator %in% "!=") {
            warning("StoX: The operator != cannot be used with multiple reference values, and was replaced by %notin%")
            l$operator <- "%notin%"
        }
        
        # Then collapse to a vector:
        #value <- unlist(value)
        
        #value <- lapply(value, function(x) if (is.character(x)) if(x == "NA") NA else paste0("\"", x, "\"") else x)
        # If more than one value, obtain the c(...) notation: 
        if(l$operator %in% c('%in%', '%notin%') && length(value) > 1) {
            value = paste0('c(', paste(value, collapse=', '), ')')
        }
        
        # Paste field, operator and value:
        result <- paste(l$field, l$operator, value)
    }
    
    # Enclose in parentheses if there was a link operator or a negation:
    if(needParentheses) {
        result <- paste0('(', result, ')')
    }
    
    # Add the exclamation mark to apply negation:
    if(negate) {
        result <- paste0('!', result)
    }
    
    return(result)
}



splitStrByOpAtLevel0 = function(expr, splitOperator){
    resArr <- list()
    currentArr <- list()
    res <- list()
    exprArr <- unlist(strsplit(expr, ''))
    exprSeq <- seq_along(exprArr)
    
    level <- 0
    for(i in exprSeq) {
        c <- exprArr[i]
        if (c == '(') {
            level <- level + 1
        } else if (c == ')') {
            level <- level - 1
        } 
        if(c != splitOperator || level > 0) {
            currentArr[[length(currentArr) + 1]] <- c
        }
        if (level == 0) {
            if(length(currentArr) > 0 && (c == splitOperator || i == nchar(expr))) {
                # flush the currentArr to a string result vector
                res[[length(res) + 1]] <- paste(currentArr, collapse='')
                currentArr <- list()
            }
        }
    }
    unlist(res)
}


#' Parse an R expression to a nested list:
#' 
#' @param expr An R expression string such as "a == 3 && !(b > 0)".
#' @param generateRuleset Logical: If TRUE output the expressions as rules, which are lists of condition and rules.
#' 
#' @export
#' 
expression2list <- function(expr, generateRuleset = TRUE) {
    res <- NULL
    expr <- trimws(expr)
    negate <- startsWith(expr, '!') 

    orFoundAtLevel0 <- FALSE
    andFoundAtLevel0 <- FALSE
    
    level <- 0
    for(c in unlist(strsplit(expr, ''))) {
        andFoundAtLevel0 <- andFoundAtLevel0 | level == 0 & c == '&'
        orFoundAtLevel0 <- orFoundAtLevel0 | level == 0 & c == '|'
        if (c == '(') {
            level <- level + 1
        } else if (c == ')') {
            level <- level -1
        }
    }
    splitOperator <- NULL
    rulesList <- NULL
    if(orFoundAtLevel0) {
        splitOperator <- '|'
    } else if(andFoundAtLevel0) {
        splitOperator <- '&'
    }
    
    # If a splitOperator (| or &) is found, split into a list of rules:
    if(!is.null(splitOperator)) {
        res = list()
        res$condition = splitOperator
        rulesList <- splitStrByOpAtLevel0(expr, splitOperator)
        #unlist(strsplit(expr, splitOperator, fixed = TRUE))
        
        res$rules <- list()
        for(grp in rulesList) {
          res$rules[[length(res$rules) + 1]] <- expression2list(grp, FALSE) 
        }
    } else if(isTRUE(negate)) { 
        # Handle negate both outside and inside parenthesis by recursing
        res <- expression2list(substr(expr, 2, nchar(expr)), generateRuleset)
        if(!is.null(res)) {
              if('rules' %in% names(res) && length(res$rules) == 1) {
                if('negate' %in% res$rules[[1]]) {
                    res$rules[[1]]$negate = !res$rules[[1]]$negate
                } else {
                  res$rules[[1]] <- c(list(negate = TRUE), res$rules[[1]])
                }
            } else {
                # keep the negate outside at the ruleset
                if('negate' %in% names(res)) {
                    res$negate = !res$negate
                } else {
                  res <- c(list(negate = TRUE), res)
                }
            }

        }
    }
    else if(startsWith(expr, '(') & endsWith(expr, ')')) {
        # rid off surrounding parenthesis (..)
        res <- expression2list(substr(expr, 2, nchar(expr) - 1), generateRuleset)
    } 
    # rule expression field=value:
    else {
        
        # expression field op value
        allPossibleOperators <- unique(unlist(getRstoxFrameworkDefinitions("filterOperators")))
        space <- "\\s*"
        regularExpression <- paste0(
            "([[:alnum:]^\\\\s]+)", 
            space, 
            paste0("(", paste(allPossibleOperators, collapse = "|"), ")"), 
            space, 
            "(.+)"
        )
        safeSeparator <- ";"
        groupingKey <- paste0("\\", 1:3, collapse = safeSeparator)
        code <- gsub(
            regularExpression, 
            groupingKey, 
            expr
        )
        splittedCode <- strsplit(code, safeSeparator)[[1]]
        if(length(splittedCode) != 3) {
            stop(paste("Syntax error in expression: ", expr))
        }
        s <- c(
            splittedCode[1], 
            splittedCode[2], 
            paste(splittedCode[-c(1,2)], collapse = safeSeparator)
        )

        if(length(s) == 3) {
            #valid syntax: field op value
            val <- eval(parse(text = s[[3]]))
            rule <- list(
                field = s[[1]], 
                operator = s[[2]], 
                value = val)
            if(rule$operator %in% c('%in%', '%notin%') && length(rule$value) == 1) {
                rule$value <- list(rule$value)
            }
            if(generateRuleset) {
                res <- list(
                    condition = '&', 
                    rules = list(rule))
            } else {
                res <- rule
            }
        }	 
    }
    res
}








#convert from expression to list 
#expr2j = function(expr) {
#    
#    json_l <- expr2l(expr, NULL, 0, log)
#    jsonlite::toJSON(json_l, auto_unbox = T, pretty=T)
#}
#j2expr(expr2j(expr))
    
verifyPaths <- function(x) {
    valid <- file.exists(x)
    if(any(!valid)) {
        warning("StoX: The following files do not exist: ", paste(x[!valid], collapse = ", "), ".")
    }
    return(x[valid])
}


getMemoryFileFormat <- function(x) {
    
    if(length(x) == 0) {
        memoryFileFormat <- getRstoxFrameworkDefinitions("memoryFileFormat_Empty")
    }
    else if(data.table::is.data.table(x)) {
        memoryFileFormat <- getRstoxFrameworkDefinitions("memoryFileFormat_Table")
    }
    else if("SpatialPolygonsDataFrame" %in% class(x)) {
        memoryFileFormat <- getRstoxFrameworkDefinitions("memoryFileFormat_Spatial")
    }
    else if("SpatialPointsDataFrame" %in% class(x)) {
        memoryFileFormat <- getRstoxFrameworkDefinitions("memoryFileFormat_Spatial")
    }
    else if(is.list(x)) {
        memoryFileFormat <- getRstoxFrameworkDefinitions("memoryFileFormat_List")
    }
    else {
        stop("Wrong memory file class ", class(x)[1])
    }
    return(memoryFileFormat)
}


writeMemoryFile <- function(x, filePathSansExt, ext = NULL) {
    
    if(length(ext) == 0) {
        # Get the memory file format and append this as a file extension to the file to write:
        ext <- getMemoryFileFormat(x)
    }
    
    filePath <- paste(filePathSansExt, ext, sep = ".")
    
    # Create the directory if missing:
    dir <- dirname(filePath)
    if(!file.exists(dir)) {
        dir.create(dir, showWarnings = FALSE, recursive = TRUE)
    }
    
    # Write the file:
    if(ext == "fst") {
        fst::write_fst(as.data.frame(x), path = filePath)
    }
    else if(ext == "rds") {
        saveRDS(x, file = filePath)
    }
    else {
        stop("Wrong memoryFileFormat")
    }
    
    return(filePath)
}

writeMemoryFiles <- function(objects, filePathsSansExt, writeOrderFile = TRUE) {
    # Write the files:
    filePaths <- mapply(writeMemoryFile, objects, filePathsSansExt)
    # Write the orderfile:
    if(writeOrderFile) {
        orderFileName <- file.path(dirname(filePathsSansExt[1]), "tableOrder.txt")
        write(filePaths, orderFileName)
    }
}

readMemoryFile <- function(filePath) {
    
    # Get the file extension:
    ext <- tools::file_ext(filePath)
    
    # Read the file:
    if(grepl("fst", ext, ignore.case = TRUE)) {
        output <- data.table::as.data.table(fst::read_fst(path = filePath))
    }
    else if(grepl("rds", ext, ignore.case = TRUE)) {
        output <- readRDS(file = filePath)
    }
    else {
        stop("Unsupported file format")
    }
    
    return(output)
}

# Small function to expand a logical to possible values starting with the the givevn value:
expandLogical <- function(x) {
    c(x, !x)
}



do.call_robust <- function(what, args, quote = FALSE, envir = parent.frame()) {
    # Get the formals of the function:
    f <-  formals(what)
    # Keep only the relevant args:
    if(is.list(args)) {
        args <- args[intersect(names(args), names(f))]
    }
    # Run the function:
    do.call(what, args, quote = quote, envir = envir)
}



