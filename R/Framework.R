# We will build up the following infrastrukture for RstoxFramework:

# 1. Define an environment RstoxEnv as in the current Rstox
# 2. Define the lists Definitons and Projects in RstoxEnv
# 3. The function initiateRstox() defines the global settings of Rstox, such as folder structure, 




# StoX GUI needs the following:

# View output:
# 1. getProcessOutputTableCount(projectName)
# 2. getProcessOutputTableName(projectName, processName)
# 3. getProcessOutputTable(projectName, processName, tableName)

# List of processes:
# getModelTypes(projectName)
# getModelName(modelType)
# getProcessesByModelType(projectName, modelType)

# getFunctionsByModelType()
# getFunctionParameterNames()
# getFunctionParameterPossibleValues()
# getFunctionParameterDefaultValue()

# open
# save
# runModel(projectName, modelName, startProcess, endProcess)
# 



# # By default, retrun only information about the function:
# 
# # From Åsmund on 2019-05-15:
# 
# StoX GUI needs the following:
#     
#     There should be a function getting the outputFileNames given the outputDataType.
# 
getOutputFileNames <- function(processName, projectName, fileExt="txt") {
    
    # Get the status of the project:
    status <- getProjectStatus(projectName)
    # Get the function name from the status:
    functionName <- status[[processName]]$functionName
    # Get the process index:
    processIndex <- status[[processName]]$processIndex
    
    # Get meta information about of function:
    meta <- do.call(functionName, list())
    # Get the table names of the data type of the function:
    dataType <- meta$outputDataType
    # Get the output table names:
    outputTableNames <- "***********************************************"
        
        # Concatinate the index of the process, the process name, the data type, and the output tables:
        outputFileNames <- paste(processIndex, processName, dataType, outputTableNames, sep="_")
    
    # Append file extension:
    outputFileNames <- paste(outputFileNames, fileExt, sep=".")
    
    outputFileNames
}
# 
# Static:
#     - functionName
# - parametersToShow: A function of the StoX parameters of the function given in a ... argument, returning # the parameters to show in StoXGUI given the values of the inputs. 
# - outputDataType
# - parentModel (should a StoX function only be used in one model?)
# 
# UserDefined (defaults should be stored in the meta information):
#     - processName
# - enabled
# - respondInGUI
# - breakInGUI
# - exportCSV Use fileOutput instead as today!
# - parameterValue
# 
# Status: 
#     - hasError
# - isPerformed
# 
# FromFormals: 
#     - parameterName
# - parameterDefaultValue
# - parameterPossibleValues
# - parameterDescription
# 
# 
# 
# 
# StoXparameterNames = parameterNames[isUpperFirstLetter(parameterNames)]
# 
# 
# metainfo <- list(
#     


#'
#' @importFrom XML xmlParse xmlToList
#' @export
#' 
readStoxXSD <- function(xsd="3.0", discardSimple=FALSE){
    
    # Get all StoX XSDs:
    xsds <- list.files(system.file("formats", package="RstoxFramework"), full.names=TRUE)
    
    # Select the xsd given by the input 'xsd'. HERE WE NEED TO HAVE SOME SORT OF VERSIONING ON THE XSDS, SUCH AS IN THE FILE NAME. FOR NOW WE PICK THE LAST:
    xsd <- xsds[length(xsds)]
    xsd <- "~/Code/Github/RstoxFramework/RstoxFramework/inst/formats/stoxProject.xsd"
    
    # Change this when building the package:
    data <- XML::xmlParse(xsd)
    xml_data <- XML::xmlToList(data)
    
    getField <- function(x, field="name"){
        if(is.list(x)) x$.attrs[field] else x[field]
    }
    
    # Function to extract character levels, either as given in the file or trimmed of the suffix "Type" and put to lowercase:
    extractLevel <- function(x, trim=TRUE){
        types <- sapply(x, "[[", ".attrs")
        # Convert to lower case as in the variable names:
        if(trim){
            types <- tolower(gsub("Type", "", types))
        }
        types
    }
    
    # Function to extract variable types:
    extractType <- function(x){
        extractOne <- function(x){
            unname(sapply(x$sequence, getField, "type"))
        }
        out <- lapply(x, extractOne)
        names(out) <- extractLevel(x, trim=TRUE)
        out
    }
    
    # Function to extract variable types:
    extractDefault <- function(x){
        extractOne <- function(x){
            unname(sapply(x$sequence, getField, "default"))
        }
        out <- lapply(x, extractOne)
        names(out) <- extractLevel(x, trim=TRUE)
        out
    }
    
    # Function to extract attributes to the levels:
    extractAttributes <- function(x, only.required=TRUE){
        extractOne <- function(x){
            getName <- function(y, only.required=TRUE){
                name <- getField(y)
                use <- getField(y, "use")
                if(only.required==TRUE){
                    name <- name[use=="required"]
                }
                name <- name[!is.na(name)]
                name
            }
            
            if("simpleContent" %in% names(x)){
                out <- getName(x$simpleContent$extension$attribute)
            }
            else{
                out <- unname(unlist(lapply(x[names(x) == "attribute"], getName, only.required=only.required)))
            }
        }
        out <- lapply(x, extractOne)
        names(out) <- extractLevel(x, trim=TRUE)
        out
    }
    
    # Function for extracting the numeric levels, where 1 is the top level:
    extractLevelNum <- function(x){
        # Get the data types and the levels:
        type <- extractType(x)
        lev <- extractLevel(x, trim=FALSE)
        
        # Declare the numeric levels to output:
        levelNum <- double(length(type))
        
        # Get the links of levels in the types:
        loc <- rep(NA, length(levelNum))
        for(i in seq_along(levelNum)){
            # Locate the current level in the list of types:
            temp <- which(sapply(type, function(x) lev[i] %in% x))
            if(length(temp)){
                loc[i] <- head(temp, 1)
            }
        }
        
        # Reverse the order of the loop if the lowest level comes first:
        locseq <- seq_along(loc)
        if(sum(locseq[!is.na(loc)]) < sum(loc[!is.na(loc)])){
            locseq <- rev(locseq)
        }
        
        # The value of currentMax is used when the level is not found in the list 'type':
        currentMax <- 0
        
        # Run through 'lev' and find it in 'type':
        for(i in locseq){
            # Locate the current level in the list of types:
            temploc <- which(sapply(type, function(x) lev[i] %in% x))
            # If not found, use the currentMax:
            if(length(temploc)==0){
                levelNum[i] <- currentMax + 1
            }
            # Otherwise use the numeric level of the location:
            else{
                levelNum[i] <- levelNum[min(temploc)] + 1
            }
            currentMax <- max(levelNum)
        }
        levelNum
    }
    
    # Function to extract the variable names:
    extractVariables <- function(x){
        extractOne <- function(x){
            #unname(sapply(x$sequence, function(y) if(is.list(y)) y$.attrs["name"] else y["name"]))
            unname(sapply(x$sequence, getField))
        }
        out <- lapply(x, extractOne)
        names(out) <- extractLevel(x, trim=TRUE)
        out
    }
    
    extractAllVariables <- function(x){
        vars <- extractVariables(x)
        attrs <- extractAttributes(x, only.required=FALSE)
        out <- lapply(seq_along(vars), function(i) c(vars[[i]], attrs[[i]]))
        names(out) <- names(vars)
        out
    }
    
    # Get the xml version:
    ver <- xml_data$.attrs["targetNamespace"]
    
    # Extract only complexType:
    complexType <- xml_data[names(xml_data)=="complexType"]
    # Discard the simpleContent:
    if(discardSimple){
        hasSimpleContent <- sapply(complexType, function(x) "simpleContent" %in% names(x))
        complexType <- complexType[!hasSimpleContent]
    }
    
    # Get the variables:
    vars <- extractAllVariables(complexType)
    defaults <- extractDefault(complexType)
    # Get all possible inputs to a process:
    possibleInputNames <- unname(unlist(vars[names(vars) == "process"]))
    # Add the defaulst if any:
    possibleInputs <- unlist(defaults[names(defaults) == "process"], recursive=FALSE)
    names(possibleInputs) <- possibleInputNames
    
    # Get the types:
    #type <- extractType(complexType)
    
    # Get the attributes:
    attrs <- extractAttributes(complexType, only.required=FALSE)
    attrs_required <- extractAttributes(complexType)
    
    # Get the numeric levels:
    level <- extractLevel(complexType)
    levelNum <- extractLevelNum(complexType)
    
    # Create a data frame with with (1) variable names, ()2 variable names added type, (3) numeric level, (4) attribute/variable type, and (level string) as columns:
    
    # Unlist the variables 'vars':
    varsVec <- unname(unlist(vars))
    nvar <- sapply(vars, length)
    # Create level vectors of the same length as the number of variables:
    levelVec <- rep(level, nvar)
    levelNumVec = rep(levelNum, nvar)
    # Append the level to the var names to obtain unique names:
    vars.levelVec <- paste(varsVec, levelVec, sep=".")
    
    # Create the type vector, which has values "Var", "Attr" and "ReqAttr":
    isFirstInSecondList <- function(x, y){
        unlist(lapply(seq_along(x), function(i) x[[i]] %in% y[[i]]))
    }
    areAttr <- isFirstInSecondList(vars, attrs)
    areAllAttrs_required <- isFirstInSecondList(vars, attrs_required)
    typeVec <- rep("Var", length(levelNumVec))
    typeVec[areAttr] <- "Attr"
    typeVec[areAllAttrs_required] <- "AttrReq"
    
    
    # Order the lists by levelNum:
    vars <- vars[levelNum] 
    attrs <- attrs[levelNum] 
    attrs_required <- attrs_required[levelNum] 
    level <- level[levelNum] 
    levelNum <- seq_along(levelNum)
    
    # The data frame returned:
    l <- list(
        Var = varsVec, 
        Var.Level = vars.levelVec, 
        Level = levelVec, 
        LevelNum = levelNumVec, 
        Type = typeVec, 
        String = paste0("Level", levelNumVec, ".", typeVec)
    )
    
    x <- data.frame(
        lapply(l, head, min(lengths(l))), 
        stringsAsFactors=FALSE
    )
    
    xsplit <- split(x, x$Level)
    
    # Output:
    list(
        possibleInputs = possibleInputs, 
        x = x, 
        xsplit = xsplit, 
        vars = vars, 
        attrs = attrs, 
        attrs_required = attrs_required, 
        level = level, 
        levelNum = levelNum, 
        ver = ver)
}




d <- readStoxXSD()$possibleInputs



StoxXSD <- list(
    
)



#readProjectXsd <- function(xsd) {
#    data <- XML::xmlParse(xsd)
#    xml_data <- XML::xmlToList(data)
#    xml_data
#}


#d <- readProjectXsd(xsds)



createProcess <- function(
    ..., 
    parameters = list()
    ) {
    
    
    defaults <- readStoxXSD()$possibleInputs
    
    
    
    
}

project.xml <- list(
    baseline = list(
        ReadAcousticXML = list(
            functionName = "ReadAcousticXML", 
            enabled = TRUE, 
            skip = TRUE, 
            respondingui = FALSE, 
            breakingui = FALSE, 
            fileoutput = FALSE, 
            inputdata = NULL, 
            output = NULL, 
            parameters = list(
                
            )
        )
    )
)



createProcess <- function(
    processName = NULL, 
    functionName = "ReadAcousticXML", 
    enabled = TRUE, 
    skip = TRUE, 
    respondingui = FALSE, 
    breakingui = FALSE, 
    fileoutput = FALSE, 
    inputdata = NULL, 
    output = NULL, 
    parameters = list()) {
    
    
}


