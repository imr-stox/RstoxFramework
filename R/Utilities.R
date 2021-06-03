##############################################
##############################################
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
    out <- class(x)[1]
    if(out == "double") {
        out <- "numeric"
    }
    return(out)
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

    x <- expandDT(x)
    
    return(x)
}

#' Function to convert data.table to fixed width:
#' 
#' @param x The table to modify.
#' @param columnSeparator The string to separate columns by, defaulted to a single space. 
#' @param lineSeparator The string to separate lines by, defaulted to a NULL, which keeps the output as a vector of strings.
#' @param na The string to replace NAs by, defaulted to "-".
#' @param enable.auto_unbox Logical: If TRUE wrap the output in a list if  \code{pretty} is TRUE and the output is of length 1. This keeps the array when converting to JSON also for length 1.
#' 
fixedWidthTable <- function(x, columnSeparator = " ", lineSeparator = NULL, na = "-", enable.auto_unbox = TRUE) {
    # Return immediately if x has length 0:
    if(length(x) == 0) {
        return(x)
    }
    
    # Hack to make it possible to print matrices:
    if(is.matrix(x)) {
        # Replace all NA with the user specified na:
        x[is.na(x)] <- na
        
        # Add the column names:
        if(length(colnames(x))) {
            x <- rbind(colnames(x), x)
        }
        
        # Right pad with spaecs:
        x <- apply(x, 2, function(y) stringr::str_pad(y, max(nchar(y)), pad = " "))
        
        # Collapse to lines:
        x <- apply(x, 1, paste, collapse = columnSeparator)
    }
    else if(data.table::is.data.table(x)) {
        # First convert all columns to character:
        x <- x[, (colnames(x)) := lapply(.SD, as.character), .SDcols = names(x)]
        
        # Replace all NA with the user specified na:
        x[is.na(x)] <- na
        
        # Add the column names:
        x <- rbindlist(list(structure(as.list(names(x)), names = names(x)), x))
        # Right pad with spaecs:
        x <- x[, lapply(.SD, function(y) stringr::str_pad(y, max(nchar(y)), pad = " "))]
        
        #for(name in names(x)) {
        #    x[, eval(name) := lapply(get(name), function(y) paste0(y, paste(rep(" ", max(nchar(get(name))) - nchar(y)), collapse = "")))]
        #}
        
        
        # Collapse to lines:
        x <- x[, do.call(paste, c(.SD, sep = columnSeparator)), .SDcols = names(x)]
    }
    
    # Collapse the lines if requested:
    if(length(lineSeparator)) {
        x <- paste(x, collapse = lineSeparator)
    }
    else if(enable.auto_unbox && length(x) == 1) {
        x <- list(x)
    }
    
    return(x)
}

# Function to extract the trailing integer of a string (vector):
getTrailingInteger <- function(x, integer = TRUE) {
    # Get the trailing numerics:
    #trailing <- stringr::str_extract(x, "[^[a-z]]*$")
    #trailing <- stringr::str_extract(x, "\\-*\\d+\\.*\\d*")
    #trailing <- gsub("^\\d.*|[A-Za-z]", "", x) # Kept the underscore
    trailing <- gsub( "^\\d.*|[A-Za-z[:punct:][:space:]]", "", x )
    
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

#' Function to convert from json to R expression:
#' 
#' @param json A JSON string
#' 
#' @export
#' 
json2expression <- function(json) {
    #l <- parseParameter(json, simplifyVector = FALSE)
    #l <- parseParameter(json)
    l <- jsonlite::fromJSON(json, simplifyVector = FALSE)
    #l <- jsonlite::fromJSON(json, simplifyVector = FALSE)
    list2expression(l)
}


#' Function to convert from R list to R expression:
#' 
#' @param l An R list.
#' @param parentHasSiblings Logical: If TRUE there are more than one element in the top level of the list.
#' 
#' @export
#' 
list2expression <- function(l, parentHasSiblings = FALSE) {
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
        result <- paste(lapply(l$rules, list2expression, parentHasSiblings = length(l$rules) > 1), collapse = link)
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
            stop("Syntax error in expression: ", expr)
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
    else if(is.matrix(x)) {
        memoryFileFormat <- getRstoxFrameworkDefinitions("memoryFileFormat_Matrix")
    }
    else if("SpatialPolygonsDataFrame" %in% class(x)) {
        memoryFileFormat <- getRstoxFrameworkDefinitions("memoryFileFormat_Spatial")
    }
    else if("SpatialPointsDataFrame" %in% class(x)) {
        memoryFileFormat <- getRstoxFrameworkDefinitions("memoryFileFormat_Spatial")
    }
    else if(is.character(x)) {
        memoryFileFormat <- getRstoxFrameworkDefinitions("memoryFileFormat_Character")
    }
    else if(is.numeric(x)) {
        memoryFileFormat <- getRstoxFrameworkDefinitions("memoryFileFormat_Numeric")
    }
    else if(is.integer(x)) {
        memoryFileFormat <- getRstoxFrameworkDefinitions("memoryFileFormat_Integer")
    }
    else if(is.logical(x)) {
        memoryFileFormat <- getRstoxFrameworkDefinitions("memoryFileFormat_Logical")
    }
    else if(is.list(x)) {
        memoryFileFormat <- getRstoxFrameworkDefinitions("memoryFileFormat_List")
    }
    else {
        stop("StoX: Wrong memory file class ", class(x)[1])
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
    #if(ext == "fst") {
    #    fst::write_fst(as.data.frame(x), path = filePath)
    #}
    #else 
    if(ext == "rds") {
        saveRDS(x, file = filePath)
    }
    else {
        stop("StoX: Wrong memoryFileFormat")
    }
    
    return(filePath)
}



writeMemoryFiles <- function(objects, filePathsSansExt, writeOrderFile = TRUE) {
    
    # Write the files, in an mapply loop if not a valid class at the top level (for outputDepth 2):
    if(firstClass(objects) %in% getRstoxFrameworkDefinitions("validOutputDataClasses")) {
        filePaths <- writeMemoryFile(objects, filePathsSansExt)
    }
    else {
        filePaths <- mapply(writeMemoryFile, objects, filePathsSansExt)
    }
    
    
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
    #if(grepl("fst", ext, ignore.case = TRUE)) {
    #    output <- data.table::as.data.table(fst::read_fst(path = filePath))
    #}
    #else 
    if(grepl("rds", ext, ignore.case = TRUE)) {
        output <- readRDS(file = filePath)
    }
    else {
        stop("StoX: Unsupported file format")
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



isProcessOutputDataType <- function(processOutput) {
    is.list(processOutput) && 
    length(processOutput) == 1 && 
    names(processOutput) %in% getRstoxFrameworkDefinitions("stoxDataTypes")$functionOutputDataType
}


hasUseOutputData <- function(projectPath, modelName, processID) {
    functionParameters <- getFunctionParameters(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID
    )
    "UseOutputData" %in% names(functionParameters)
}



emptyNamedList <- function() {
    list(a = 1)[0]
}


#' Function to expand a data table so that the cells that are vectors are transposed and the rest repeated to fill the gaps
#' 
#' @param DT A data.table.
#' @param toExpand A vector of names of the tables to expand.
#' 
#' @export
#' 
expandDT <- function(DT, toExpand = NULL) {
    # Set the columns to expand:
    if(length(toExpand) == 0) {
        lens <- lapply(DT, lengths)
        lensLargerThan1 <- sapply(lens, function(l) any(l > 1))
        toExpand <- names(DT)[lensLargerThan1]
    }
    
    if(length(toExpand)) {
        expanded <- lapply(toExpand, function(x) DT[, unlist(get(x))])
        names(expanded) <- toExpand
        DT <- do.call(
            cbind, 
            c(
                list(
                    DT[rep(1:.N, lengths(get(toExpand[1]))), !toExpand, with = FALSE]
                ), 
                #lapply(toExpand, function(x) DT[, unlist(get(x))])
                expanded
            )
        )
    }
    
    DT
}

as.list1 <- function(x) {
    if(!length(x) && !is.list(x) && is.vector(x)) {
        x <- list()
    }
    if(length(x) == 1 && !is.list(x) && is.vector(x)) {
        x <- list(x)
    }
    
    return(x)
}


capitalizeFirstLetter <- function(x) {
    gsub("(^[[:alpha:]])", "\\U\\1", x, perl=TRUE)
}





#' Function for unzipping a zipped StoX project
#' 
#' @param projectPath The project to be run and tested against the existing output files of the project gievn by \code{projectPath_original}.
#' @param exdir The direcory to unzip to, defaulted to the current directory.
#' 
#' @export
#'
unzipProject <- function(projectPath, exdir = ".") {
    if(!isProject(projectPath)) {
        stop("The zip ", projectPath, " does not contain a valid StoX project at the root folder.")
    }
    
    utils::unzip(projectPath, exdir = exdir)
    projectName <- basename(tools::file_path_sans_ext(projectPath))
    unzippedProjectPath <- file.path(exdir, projectName)
    return(unzippedProjectPath)
}



#' Function for comparing existing output files with the memory read using runProject()
#' 
#' @param projectPath The project to be run and tested against the existing output files of the project gievn by \code{projectPath_original}.
#' @param projectPath_original The project holding the existing output files, defaulted to \code{projectPath}.
#' 
#' @export
#'
compareProjectToStoredOutputFiles <- function(projectPath, projectPath_original = projectPath) {
    
    # Unzip if zipped:
    if(tolower(tools::file_ext(projectPath)) == "zip") {
        projectPath <- unzipProject(projectPath, exdir = tempdir())
    }
    if(projectPath_original != projectPath && tolower(tools::file_ext(projectPath_original)) == "zip") {
        projectPath_original <- unzipProject(projectPath_original, exdir = tempdir())
    }
    
    # Run the test project:
    projectPath_copy <- file.path(tempdir(), paste0(basename(projectPath), "_copy"))
    temp <- copyProject(projectPath, projectPath_copy, ow = TRUE)
    
    
    openProject(projectPath_copy)
    dat <- runProject(projectPath_copy, unlist.models = TRUE, drop.datatype = FALSE, unlistDepth2 = TRUE)
    
    # Read the original data:
    dat_orig <- readModelData(projectPath_original, unlist.models = TRUE)
    
    # Compare only those elemens common to the two datasets:
    processNames_present <- all(names(dat_orig) %in% names(dat))
    
    # Expect all column names:
    tableNames_identical <- list()
    columnNames_identical <- list()
    for(name in names(dat_orig)) {
        # Check identical table names: 
        tableNames_identical[[name]] <- identical(sort(names(dat_orig[[name]])), sort(names(dat[[name]])))
        # Check identical column names: 
        columnNames_identical[[name]] <- list()
        for(subname in names(dat_orig[[name]])) {
            columnNames_identical[[name]][[subname]] <- identical(names(dat_orig[[name]][[subname]]), names(dat[[name]][[subname]]))
        }
    }
    
    # Check the actual data:
    data_equal <- list()
    
    for(name in names(dat_orig)) {
        data_equal[[name]] <- list()
        for(subname in names(dat_orig[[name]])) {
            if(data.table::is.data.table(dat_orig[[name]][[subname]])) {
                data_equal[[name]][[subname]] <- compareDataTablesUsingClassOfFirst(dat_orig[[name]][[subname]], dat[[name]][[subname]])
                #data_equal[[name]][[subname]] <- compareDataTablesUsingClassOfSecond(dat_orig[[name]][[subname]], dat[[name]][[subname]])
            }
            else if("SpatialPolygonsDataFrame" %in% class(dat_orig[[name]][[subname]])){
                data_equal[[name]][[subname]] <- compareSPDF(dat_orig[[name]][[subname]], dat[[name]][[subname]])
            }
            else {
                data_equal[[name]][[subname]] <- all.equal(dat_orig[[name]][[subname]], dat[[name]][[subname]])
            }
        }
    }
    
    allTests <- list(
        processNames_present = processNames_present,
        tableNames_identical = tableNames_identical,
        columnNames_identical = columnNames_identical,
        data_equal = data_equal
    )
    
    ok <- all(unlist(allTests) %in% TRUE)
    
    warning(paste(names(unlist(allTests)), unlist(allTests), collapse = ",",sep = "-"))
    if(!ok) {
        warning(paste(names(unlist(allTests)), unlist(allTests), collapse = ",",sep = "-"))
        return(allTests)
    }
    else {
        return(TRUE)
    }
}

# Compare two data.tables while ignoring attributes and coercing classes of the first to classes of the second:
compareDataTablesUsingClassOfFirst <- function(x, y) {
    # Get the classes of the first and second table:
    classes_in_x <- sapply(x, firstClass)
    classes_in_y <- sapply(y, firstClass)
    if(!identical(classes_in_x, classes_in_y)) {
        # Coerce to the class in the memory:
        differ <- names(x)[classes_in_x != classes_in_y]
        for(col in differ){
            data.table::set(x, j = col, value = methods::as(x[[col]], classes_in_y[col]))
        }
    }
    # Check equality:
    all.equal(x, y, check.attributes = FALSE)
}

# Compare two data.tables while ignoring attributes and coercing classes of the first to classes of the second:
compareDataTablesUsingClassOfSecond <- function(x, y) {
    # Get the classes of the first and second table:
    classes_in_x <- sapply(x, firstClass)
    classes_in_y <- sapply(y, firstClass)
    if(!identical(classes_in_x, classes_in_y)) {
        # Coerce to the class in the memory:
        differ <- names(x)[classes_in_x != classes_in_y]
        for(col in differ){
            data.table::set(y, j = col, value = methods::as(y[[col]], classes_in_x[col]))
        }
    }
    # Check equality:
    all.equal(x, y, check.attributes = FALSE)
}

# Get all coordinates of a SpatialPolygonsDataFrame in one data.table:
getAllCoords <- function(x) {
    out <- RstoxBase::getStratumPolygonList(x)
    out <- data.table::rbindlist(lapply(out, unname))
    return(out)
}

# Compare two SpatialPolygonsDataFrames using only the polygons:
compareSPDF <- function(x, y) {
    xc<- getAllCoords(x)
    yc<- getAllCoords(y)
    all.equal(xc, yc)
}
