#library(xml2)
#library(data.table)

#
# parsing XML
#

#' Extracts text value form node with n attributes
#'@keywords internal
#'@noRd
get_simple_content <- function(node){
  if (length(xml2::xml_attrs(node))>0){
    stop("Attempting simple content extraction from node with attributes")
  }
  if (length(xml2::xml_children(node))>0){
    stop("Attempting simple content extraction from node with attributes")
  }
  return(xml2::xml_text(node,trim=T))
}

#' REMOVE ?
# Parses covariate definitions from xml
process_covardef <- function(node){
  
  table <- data.table(covariatesourcetype=character(), covariate=character(), value=character())
  for (cc in xml2::xml_children(node)){
    sourcestype <- xml2::xml_attr(cc, "covariatesourcetype")
    covariate <- xml2::xml_attr(cc, "covariate")
    value <- xml2::xml_text(cc, T)
    table <- rbind(table, data.table(covariatesourcetype=c(sourcestype), covariate=c(covariate), value=c(value)))
  }
  return(table)
}

#' REMOVE ?
#' Parses processdata from xml
#'@keywords internal
#'@noRd
process_processdata <- function(node){
  
  attributes <- xml2::xml_attrs(node)
  for (n in names(attributes)){
    stop(paste("Parsing of attribute", n, "not supported"))
  }

  pd <- list()  
  pd$BioticAssignment <- data.table(assignmentid=character(), station=character(), stationweight=character())
  pd$SuAssignment <- data.table(sampleunit=character(), estlayer=character(), assignmentid=character())
  pd$AssignmentResolution <- list()
  pd$EdsuPsu <- list()
  pd$PsuStratum <- list()
  pd$StratumPolygon <- list()
  pd$Temporal <- data.table(covariatesourcetype=character(), covariate=character(), value=character())
  pd$Gearfactor <- data.table(covariatesourcetype=character(), covariate=character(), value=character())
  pd$Spatial <- data.table(covariatesourcetype=character(), covariate=character(), value=character())
  pd$Platformfactor <- data.table(covariatesourcetype=character(), covariate=character(), value=character())
  pd$CovParam <- list()
  pd$AgeError <-  data.table(readage=character(), realage=character(), probability=character())
  pd$StratumNeighbour <- list()
  
  children <- xml2::xml_children(node)
  for (c in children){
    n <- xml2::xml_name(c)
    if (n=="bioticassignment"){
      for (cc in xml2::xml_children(c)){
        pd$BioticAssignment <- rbind(pd$BioticAssignment, data.table(assignmentid=c(xml2::xml_attr(cc, "assignmentid")), station=c(xml2::xml_attr(cc, "station")), stationweight=c(xml2::xml_text(cc, T))))  
      }
    }
    else if (n=="suassignment"){
      for (cc in xml2::xml_children(c)){
        pd$SuAssignment <- rbind(pd$SuAssignment, data.table(sampleunit=c(xml2::xml_attr(cc, "sampleunit")), estlayer=c(xml2::xml_attr(cc, "estlayer")), assignmentid=c(xml2::xml_text(cc, T))))  
      }
    }
    else if (n=="assignmentresolution"){
      for (cc in xml2::xml_children(c)){
        var <- xml2::xml_attr(cc, "variable")
        value <- xml2::xml_text(cc, T)
        pd$AssignmentResolution[[var]]<-value
      }
    }
    else if (n=="edsupsu"){
      for (cc in xml2::xml_children(c)){
        edsu <- xml2::xml_attr(cc, "edsu")
        value <- xml2::xml_text(cc, T)
        pd$EdsuPsu[[edsu]]<-value
      }
    }
    else if (n=="psustratum"){
      for (cc in xml2::xml_children(c)){
        psu <- xml2::xml_attr(cc, "psu")
        value <- xml2::xml_text(cc, T)
        pd$PsuStratum[[psu]]<-value
      }
    }
    else if (n=="stratumpolygon"){
      for (cc in xml2::xml_children(c)){
        key <- xml2::xml_attr(cc, "polygonkey")
        var <- xml2::xml_attr(cc, "polygonvariable")
        value <- xml2::xml_text(cc, T)
        if (is.null(pd$StratumPolygon[[key]])){
          pd$StratumPolygon[[key]]<-list()          
        }
        pd$StratumPolygon[[key]][[var]]<-value
      }
    }
    else if (n=="temporal"){
      pd$Temporal <- process_covardef(c)
    }
    else if (n=="gearfactor"){
      pd$Gearfactor <- process_covardef(c)
    }
    else if (n=="spatial"){
      pd$Spatial <- process_covardef(c)
    }
    else if (n=="platformfactor"){
      pd$Platformfactor <- process_covardef(c)
    }
    else if (n=="covparam"){
      for (cc in xml2::xml_children(c)){
        covariatetable <- xml2::xml_attr(cc, "covariatetable")
        parameter <- xml2::xml_attr(cc, "parameter")
        value <- xml2::xml_text(cc, T)
        if (is.null(pd$CovParam[[covariatetable]])){
          pd$CovParam[[covariatetable]]<-list()          
        }
        pd$CovParam[[covariatetable]][[parameter]]<-value
      }
    }
    else if (n=="ageerror"){
      for (cc in xml2::xml_children(c)){
        readage <- xml2::xml_attr(cc, "readage")
        realage <- xml2::xml_attr(cc, "realage")
        value <- xml2::xml_text(cc, T)
        pd$AgeError <-rbind(pd$AgeError, data.table(readage=c(readage), realage=c(realage), probability=c(value)))
      }
    }
    else if (n=="stratumneighbour"){
      for (cc in xml2::xml_children(c)){
        stratum <- xml2::xml_attr(cc, "variable")
        neighbour <- xml2::xml_text(cc, T)
        if (!is.null(pd$StratumNeighbour[[stratum]])){
          pd$StratumNeighbour[[stratum]] <- c(pd$StratumNeighbour[[stratum]], neighbour)
        }else{
          pd$StratumNeighbour[[stratum]] <- c(neighbour)
        }
      }
    }
    else{
      stop(paste("Parsing of element", n, "not supported"))
    }
  }
  
  return(pd)
}

#' REMOVE ?
#' Parses function inputs from xml
#'@keywords internal
#'@noRd
process_processparameters <- function(node){
  
  attributes <- xml2::xml_attrs(node)
  for (n in names(attributes)){
      stop(paste("Parsing of attribute", n, "not supported"))
  }
  
  
  fi <- list()
  fi$Enabled <- logical()
  fi$FileOutput <- logical()
  fi$BreakInGUI <- logical()
  
  children <- xml2::xml_children(node)
  for (c in children){
    n <- xml2::xml_name(c)
    if (n=="enabled"){
      fi$Enabled <- as.logical(get_simple_content(c))
    }
    else if (n=="breakingui"){
      fi$BreakInGUI <- as.logical(get_simple_content(c))
    }
    else if (n=="fileoutput"){
      fi$FileOutput <- as.logical(get_simple_content(c))
    }
    else{
      stop(paste("Parsing of element", n, "not supported"))
    }
  }

  return(fi)
}

#' REMOVE ?
#' Parses function parameter
#' @parvalue string representation of value
#' @typename type of value
#' @keywords internal
#' @noRd
parse_function_parameter <- function(parvalue, typename){
  if (typename=="character"){
    return(parvalue)
  }
  else if (typename=="logical"){
    return(as.logical(parvalue))
  }
  else if (typename=="integer"){
    return(as.integer(parvalue))
  }
  else if (typename=="numeric"){
    return(as.numeric(parvalue))
  }
  else{
    stop("Parameter type", typename, "not supported.")
  }
}

#' REMOVE ?
#'Parses process from xml
#'@param node
#'@param baselineprocess indicate whether the process to parse is a baselineprocess (may contain processdata)
#'@keywords internal
#'@noRd
parse_process <- function(node, baselineprocess=F){
  
  process <- list()
  process$ProcessName <- character()
  
  attributes <- xml2::xml_attrs(node)
  for (n in names(attributes)){
    if (n=="processname"){
      process$ProcessName <- attributes[n]
    }
    else{
      stop(paste("Parsing of attribute", n, "not supported"))
    }
  }
  
  process$FunctionName <- character()
  process$ProcessParameters <- list()
  process$FunctionParameters <- list()
  process$FunctionInputs <- list()
  process$Output <- character()
  
  if (baselineprocess){
    process$ProcessData <- list()
  }
  
  children <- xml2::xml_children(node)
  for (c in children){
    n <- xml2::xml_name(c)
    if (n == "functionname"){
      process$FunctionName <- get_simple_content(c)
    }
    else if (n == "processparameters"){
      process$ProcessParameters <- process_processparameters(c)
    }
    else if (n == "functioninput"){
      if (length(xml2::xml_attrs(c))!=2){
        stop(paste("Unexpected number of attributes for element", n))
      }
      if (nchar(xml2::xml_text(c,trim=T))!=0){
        stop(paste("Unexpected content for element", n))
      }
      inputname <- xml2::xml_attr(c, "dataparameter")
      inputprocess <- xml2::xml_attr(c, "inputprocessname")
      process$FunctionInputs[[inputname]] <- inputprocess
    }
    else if (n == "functionparameter"){
      if (length(xml2::xml_attrs(c))!=2){
        stop(paste("Unexpected number of attributes for element", n))
      }
      parname <- xml2::xml_attr(c, "name")
      typename <- xml2::xml_attr(c, "paramtypename")
      parvalue <- xml2::xml_text(c,trim=T)
      
      if (is.null(process$FunctionParameters[[parname]])){
        process$FunctionParameters[[parname]] <- parse_function_parameter(parvalue, typename)
      } else{
        process$FunctionParameters[[parname]] <- c(process$FunctionParameters[[parname]], parse_function_parameter(parvalue, typename))
      }
      
    }
    else if (n == "output"){
      process$Output <- get_simple_content(c)
    }
    else if (n == "processdata"){
      if (!baselineprocess){
        stop("Processdata found when parsing without baselineprocessing options enabled")
      }
      else{
        process$ProcessData <- process_processdata(c)
      }
    }
    
    else{
      stop(paste("Parsing of element", n, "not supported"))
    }
  }
  
  return(process)
}

#' REMOVE ?
#'Parses rstoxdependencies from xml
#'@keywords internal
#'@noRd
process_rstoxdependencies <- function(node){
  
  attributes <- xml2::xml_attrs(node)
  for (n in names(attributes)){
    stop(paste("Parsing of attribute", n, "not supported"))
  }
  
  deps <- list()
  children <- xml2::xml_children(node)
  for (c in children){
    n <- xml2::xml_name(c)
    if (n=="rlibrary"){
      if (length(xml2::xml_attrs(c))!=2){
        stop(paste("Unexpected number of attributes for element", n))
      }
      if (nchar(xml2::xml_text(c,trim=T))!=0){
        stop(paste("Unexpected content for element", n))
      }
      libraryname <- xml2::xml_attr(c, "library")
      version <- xml2::xml_attr(c, "version")
      deps[[libraryname]] <- version
    }
    else{
      stop(paste("Parsing of element", n, "not supported"))
    }
  }
  
  return(deps) 
}

#' REMOVE ?
#'Parses models from xml
#'@keywords internal
#'@noRd
process_model <- function(node){
  attributes <- xml2::xml_attrs(node)
  
  for (n in names(attributes)){
    stop(paste("Parsing of attribute", n, "not supported"))
  }
  
  model <- list()
  children <- xml2::xml_children(node)
  for (c in children){
    n <- xml2::xml_name(c)
    if (n=="process"){
      processname <- xml2::xml_attr(c, "processname")
      if (processname %in% names(model)){
        stop(paste("Recurring processname in model:",processname))
      }
      model[[processname]] <- parse_process(c, F)
    }
    else if (n=="baselineprocess"){
      processname <- xml2::xml_attr(c, "processname")
      if (processname %in% names(model)){
        stop(paste("Recurring processname in model:",processname))
      }
      model[[processname]] <- parse_process(c, T)
    }
    else{
        stop(paste("Parsing of element", n, "not supported"))
    }
  }
  
  return(model) 
}

#' REMOVE ?
#'Parses project from xml
#'@keywords internal
#'@noRd
process_project <- function(node){
  
  projectDescription <- list()
  attributes <- xml2::xml_attrs(node)
  
  #
  # check that namespace is given
  #
  
  if (!("xmlns" %in% names(attributes))){
    stop("No default namespace provided. Namespace prefix not supported.")
  }
  
  
  #
  # handle attributes
  #
  
  projectDescription$Template <- character()
  projectDescription$Description <- character()
  projectDescription$Lastmodified <- .POSIXct(character(0))
  projectDescription$Rstoxversion <- character()
  projectDescription$Stoxversion <- character()
  projectDescription$Rversion <- character()

  for(n in names(attributes)){
    if (n=="template"){
      projectDescription$Template <- attributes[[n]]
    }
    else if (n=="description"){
      projectDescription$Description <- attributes[[n]]
    }
    else if (n=="lastmodified"){
      if (nchar(attributes[[n]])>0){
        projectDescription$Lastmodified <- as.POSIXct(gsub("T", " ", attributes[[n]]))  
      }
    }
    else if (n=="rstoxversion"){
      projectDescription$Rstoxversion <- attributes[[n]]
    }
    else if (n=="stoxversion"){
      projectDescription$Stoxversion <- attributes[[n]]
    }
    else if (n=="rversion"){
      projectDescription$Rversion <- attributes[[n]]
    }
    else if (n=="xmlns"){
      compatibleFormats = c("http://www.imr.no/formats/stox/v3")
      if (!(attributes[n] %in% compatibleFormats)){
        warning(paste("default xml namespace", attributes[n], "is not a compatible format"))
      }
    }
    else{
      stop(paste("Parsing of attribute", n, "not supported"))
    }
  }

  #
  #handle child elements
  #
  
  projectDescription$Baseline <- list()
  projectDescription$Statistics <- list()
  projectDescription$Report <- list()
  projectDescription$RstoxDependencies <- list()
  
  children <- xml2::xml_children(node)
  for (c in children){
    n <- xml2::xml_name(c)
    if (n=="baselinemodel"){
      projectDescription$Baseline <- process_model(c)
    }
    else if (n=="statistics"){
      projectDescription$Statistics <- process_model(c)
    }
    else if (n=="report"){
      projectDescription$Report <- process_model(c)
    }
    else if (n=="rstoxdependencies"){
      projectDescription$RstoxDependencies <- process_rstoxdependencies(c)
    }
    else{
      stop(paste("Parsing of element", n, "not supported"))
    }
  }
  return(projectDescription)
}

#' REMOVE ?
#' Inspects all elements and attributes and processes them.
#'
#' @keywords internal
#' @noRd
process_xml <- function(root){
  return(process_project(root))
}


#
# /parsing XML
#

#
# writing XML
#

#' REMOVE ?
#' Create XML node for processparameters
#' @keywords internal
#' @noRd
getProcessParametersXml <- function(processparameters){
  node <- xml2::xml_new_root("processparameters")
  
  enabled <- xml2::xml_new_root("enabled")
  xml2::xml_text(enabled) <- tolower(as.character(processparameters$Enabled))
  xml2::xml_add_child(node, enabled)
  
  if (!is.null(processparameters$BreakInGUI)){
    bi <- xml2::xml_new_root("breakingui")
    xml2::xml_text(bi) <- tolower(as.character(processparameters$BreakInGUI))
    xml2::xml_add_child(node, bi)
  }
  
  fo <- xml2::xml_new_root("fileoutput")
  xml2::xml_text(fo) <- tolower(as.character(processparameters$FileOutput))
  xml2::xml_add_child(node, fo)
  
  return(node)
}

#' REMOVE ?
#' Get xml for covariate definitions
#' @keywords internal
#' @noRd
getCovarDefXml <- function(dataframe, rootname){
  rootnode <- xml2::xml_new_root(rootname)
  nodename <- names(dataframe)[3]
  att1 <- names(dataframe)[1]
  att2 <- names(dataframe)[2]
  for (i in 1:nrow(dataframe)){
    node <- xml2::xml_new_root(nodename)
    xml2::xml_attr(node, att1) <- dataframe[[att1]][[i]]
    xml2::xml_attr(node, att2) <- dataframe[[att2]][[i]]
    xml2::xml_text(node) <- dataframe[[nodename]][[i]]
    xml2::xml_add_child(rootnode, node)
  }
  return(rootnode)
}

#' REMOVE ?
#' Create XML node for processdata
#' @keywords internal
#' @noRd
getProcessDataXml <- function(processdata){
  node <- xml2::xml_new_root("processdata")
  
  if (!is.null(processdata$BioticAssignment)){
    pd <- xml2::xml_new_root("bioticassignment")
    for (i in 1:nrow(processdata$BioticAssignment)){
      assignment <- xml2::xml_new_root("stationweight")
      xml2::xml_attr(assignment, "assignmentid") <- processdata$BioticAssignment$assignmentid[i]
      xml2::xml_attr(assignment, "station") <- processdata$BioticAssignment$station[i]
      xml2::xml_text(assignment) <- processdata$BioticAssignment$stationweight[i]
      xml2::xml_add_child(pd, assignment)
    }
    xml2::xml_add_child(node, pd)
  }


  if (!is.null(processdata$SuAssignment)){
    pd <- xml2::xml_new_root("suassignment")
    for (i in 1:nrow(processdata$SuAssignment)){
      assignment <- xml2::xml_new_root("assignmentid")
      xml2::xml_attr(assignment, "sampleunit") <- processdata$SuAssignment$sampleunit[i]
      xml2::xml_attr(assignment, "estlayer") <- processdata$SuAssignment$estlayer[i]
      xml2::xml_text(assignment) <- processdata$SuAssignment$assignmentid
      xml2::xml_add_child(pd, assignment)
    }
    xml2::xml_add_child(node, pd)
  }
  
  if (!is.null(processdata$AssignmentResolution)){
    pd <- xml2::xml_new_root("assignmentresolution")
    for (n in names(processdata$AssignmentResolution)){
      assignment <- xml2::xml_new_root("value")
      xml2::xml_attr(assignment, "variable") <- n
      xml2::xml_text(assignment) <- processdata$AssignmentResolution[[n]]
      xml2::xml_add_child(pd, assignment)
    }
    xml2::xml_add_child(node, pd)
  }
  
  if (!is.null(processdata$EdsuPsu)){
    pd <- xml2::xml_new_root("edsupsu")
    for (n in names(processdata$EdsuPsu)){
      psu <- xml2::xml_new_root("psu")
      xml2::xml_attr(psu, "edsu") <- n
      xml2::xml_text(psu) <- processdata$EdsuPsu[[n]]
      xml2::xml_add_child(pd, psu)
    }
    xml2::xml_add_child(node, pd)
  }
  
  if (!is.null(processdata$PsuStratum)){
    pd <- xml2::xml_new_root("psustratum")
    for (n in names(processdata$PsuStratum)){
      stratum <- xml2::xml_new_root("stratum")
      xml2::xml_attr(stratum, "psu") <- n
      xml2::xml_text(stratum) <- processdata$PsuStratum[[n]]
      xml2::xml_add_child(pd, stratum)
    }
    xml2::xml_add_child(node, pd)
  }

  if (!is.null(processdata$StratumPolygon)){
    pd <- xml2::xml_new_root("stratumpolygon")
    for (n in names(processdata$StratumPolygon)){
      for (entry in names(processdata$StratumPolygon[[n]])){
        value <- xml2::xml_new_root("value")
        xml2::xml_attr(value, "polygonkey") <- n
        xml2::xml_attr(value, "polygonvariable") <- entry
        xml2::xml_text(value) <- processdata$StratumPolygon[[n]][[entry]]
        xml2::xml_add_child(pd, value)
      }
    }
    xml2::xml_add_child(node, pd)
  }

  if (!is.null(processdata$Temporal)){
    xml2::xml_add_child(node, getCovarDefXml(processdata$Temporal, "temporal"))  
  }

  if (!is.null(processdata$Gearfactor)){
    xml2::xml_add_child(node, getCovarDefXml(processdata$Gearfactor, "gearfactor"))  
  }
  
  if (!is.null(processdata$Spatial)){
    xml2::xml_add_child(node, getCovarDefXml(processdata$Spatial, "spatial"))  
  }
  
  if (!is.null(processdata$Platformfactor)){
    xml2::xml_add_child(node, getCovarDefXml(processdata$Platformfactor, "platformfactor"))  
  }
  
  
  if (!is.null(processdata$CovParam)){
    pd <- xml2::xml_new_root("covparam")
    for (n in names(processdata$CovParam)){
      for (entry in names(processdata$CovParam[[n]])){
        value <- xml2::xml_new_root("value")
        xml2::xml_attr(value, "covariatetable") <- n
        xml2::xml_attr(value, "parameter") <- entry
        xml2::xml_text(value) <- processdata$CovParam[[n]][[entry]]
        xml2::xml_add_child(pd, value)
      }
    }
    xml2::xml_add_child(node, pd)
  }

  if (!is.null(processdata$AgeError)){
    pd <- xml2::xml_new_root("ageerror")
    for (i in 1:nrow(processdata$AgeError)){
      ageerror <- xml2::xml_new_root("probability")
      xml2::xml_attr(ageerror, "readage") <- processdata$AgeError$readage[i]
      xml2::xml_attr(ageerror, "realage") <- processdata$AgeError$realage[i]
      xml2::xml_text(ageerror) <- processdata$AgeError$probability[i]
      xml2::xml_add_child(pd, ageerror)
    }
    xml2::xml_add_child(node, pd) 
  }
  
  if (!is.null(processdata$StratumNeighbour)){
    pd <- xml2::xml_new_root("stratumneighbour")
    for (n in names(processdata$StratumNeighbour)){
      for (v in processdata$StratumNeighbour[[n]]){
        value <- xml2::xml_new_root("value")
        xml2::xml_attr(value, "variable") <- n
        xml2::xml_text(value) <- v
        xml2::xml_add_child(pd, value)
      }
    }
    xml2::xml_add_child(node, pd)
  }
  
  return(node)
}

#' REMOVE ?
#' Create XML node for function parameter
#' @keywords internal
#' @noRd
getFunctionParameterXml <- function(name, paramvalue, paramclass){
  paramnode <- xml2::xml_new_root("functionparameter")
  xml2::xml_attr(paramnode, "name") <- name
  
  if (paramclass=="character"){
    xml2::xml_attr(paramnode, "paramtypename") <- "character"
    xml2::xml_text(paramnode) <- as.character(paramvalue)    
  }
  else if (paramclass=="integer"){
    xml2::xml_attr(paramnode, "paramtypename") <- "integer"
    xml2::xml_text(paramnode) <- as.character(paramvalue)    
  }
  else if (paramclass=="numeric"){
    xml2::xml_attr(paramnode, "paramtypename") <- "numeric"
    xml2::xml_text(paramnode) <- as.character(paramvalue)    
  }
  else if (paramclass=="logical"){
    xml2::xml_attr(paramnode, "paramtypename") <- "logical"
    xml2::xml_text(paramnode) <- as.character(paramvalue)    
  }
  else{
    stop(paste("Function parameter of class", paramclass, "is not supported."))
  }
  
  return(paramnode)
}

#' REMOVE ?
#' Create XML node for process
#' @param model nested list representation of model
#' @param baselineprocess logical, whether to write baseline process (with processdata)
#' @keywords internal
#' @noRd
getProcessXml <- function(process, baselineprocess=F){
  
  if (baselineprocess){
    node <- xml2::xml_new_root("baselineprocess")
  }
  else{
    node <- xml2::xml_new_root("process")  
  }
  
  
  functionnamenode <- xml2::xml_new_root("functionname")
  xml2::xml_text(functionnamenode) <- process$FunctionName
  xml2::xml_add_child(node, functionnamenode)
  
  xml2::xml_add_child(node, getProcessParametersXml(process$ProcessParameters))

  for (n in names(process$FunctionInputs)){
    finode <- xml2::xml_new_root("functioninput")
    xml2::xml_attr(finode, "dataparameter") <- n
    xml2::xml_attr(finode, "inputprocessname") <- process$FunctionInputs[[n]]
    xml2::xml_add_child(node, finode)
  }
  
    
  for (n in names(process$FunctionParameters)){
    
    for (p in process$FunctionParameters[[n]]){
      xml2::xml_add_child(node, getFunctionParameterXml(n, p, class(process$FunctionParameters[[n]])))
    }
  }
  
  if (!is.null(process$Output)){
    outputnode <- xml2::xml_new_root("output")
    xml2::xml_text(outputnode) <- process$Output
    xml2::xml_add_child(node, outputnode)
  }
  
  if (baselineprocess){
    xml2::xml_add_child(node, getProcessDataXml(process$ProcessData))
  }
  

  
  return(node)
}

#' REMOVE ?
#' Create XML node for model
#' @param model nested list representation of model
#' @param baselinemodel logical, whether to write baselinemodel
#' @param nodename name to use for node
#' @keywords internal
#' @noRd
getModelXml <- function(model, baselinemodel, nodename){
  node <- xml2::xml_new_root(nodename)
  for (n in names(model)){
    processnode <- getProcessXml(model[[n]], baselinemodel)
    xml2::xml_attr(processnode, "processname") <- n 
    xml2::xml_add_child(node, processnode)
  }
  return(node)
}

#' REMOVE ?
#' Create XML node for restox dependencies
#' @param rstoxdependencies nested list representation of model
#' @param baselinemodel logical, whether to write baselinemodel
#' @param nodename name to use for node
#' @keywords internal
#' @noRd
getRstoxDependenciesXml <- function(rstoxdependencies){
  node <- xml2::xml_new_root("rstoxdependencies")
  for (n in names(rstoxdependencies)){
    depnode <- xml2::xml_new_root("rlibrary")
    xml2::xml_attr(depnode, "library") <- n
    xml2::xml_attr(depnode, "version") <- rstoxdependencies[[n]]
    xml2::xml_add_child(node, depnode)
  }
  return(node)
}

#
# /writing XML
#


#
# public functions
#

#' REMOVE ?
#' Read Stox project from project xml
#' @param projectxml xml filename
#' @return Nested list representation of project
#' @noRd
readProject <- function(projectxml){
  tree <- xml2::read_xml(projectxml)
  
  root <- xml2::xml_root(tree)
  
  projectDescription <- list()
  return(process_xml(root))
}

#' REMOVE ?
#' Save Stox project to project xml file using namespace http://www.imr.no/formats/stox/v3
#' @param projectDescription nested list representation of StoX project
#' @param filename to write to
#' @noRd
saveProjectEdvin <- function(projectDescription, filename){
  namespace = "http://www.imr.no/formats/stox/v3"
  
  doc <- xml2::xml_new_document()
  xml2::xml_add_child(doc, "project")
  root <- xml2::xml_root(doc)
  xml2::xml_attr(root, "xmlns") <- namespace
  
  xml2::xml_attr(root, "template") <- projectDescription$Template
  xml2::xml_attr(root, "description") <- projectDescription$Description
  if (!is.null(projectDescription$Lastmodified)){
    xml2::xml_attr(root, "lastmodified") <- gsub("\ ","T",as.character(projectDescription$Lastmodified))
  }
  xml2::xml_attr(root, "rstoxversion") <- projectDescription$Rstoxversion
  xml2::xml_attr(root, "stoxversion") <- projectDescription$Stoxversion
  xml2::xml_attr(root, "rversion") <- projectDescription$Rversion
  
  xml2::xml_add_child(root, getModelXml(projectDescription$Baseline, T, "baselinemodel"))
  xml2::xml_add_child(root, getModelXml(projectDescription$Statistics, F, "statistics"))
  xml2::xml_add_child(root, getModelXml(projectDescription$Report, F, "report"))
  xml2::xml_add_child(root, getRstoxDependenciesXml(projectDescription$RstoxDependencies))
  
  xml2::write_xml(doc, filename)
}

#
# Legacy stox project parsers
#

#' response when unrecognized elements are encountered.
#' @param message message to user
#' @param strict logical, error is raised if strict is true, otherwise a warning is generated
skipping <- function(message, strict){
  if (strict){
    stop(message)
  }
  else{
    warning(message)
  }
}

#' @noRd
processStox27Meta <- function(node, strict){
  
  meta <- list()
  attributes <- xml2::xml_attrs(node)
  if (length(attributes)>0){
    skipping("attributes on element 'meta' are not recognized.", strict)
  }
  
  meta$description <- character()
  meta$surveyTimeseriesName <- character()
  
  children <- xml2::xml_children(node)
  for (c in children){
    n <- xml2::xml_name(c)
    if (n=="description"){
      meta$description <- get_simple_content(c)
    }
    else if (n=="surveyTimeseriesName"){
      meta$surveyTimeseriesName <- get_simple_content(c)
    }
    else{
      skipping(paste("Unrecoginzed attribute of meta:", n), strict)
    }
  }
  
  return(meta)
}

#' @noRd
processStox27parameter <- function(node, strict){
  parameter <- list()
  attributes <- xml2::xml_attrs(node)
  
  parameter$name <- character()
  for(n in names(attributes)){
    if (n=="name"){
      parameter$name <- attributes[[n]]
    }
    else{
      skipping(paste("Unrecoginzed attribute of parameter:", n), strict)
    }
  }
  
  children <- xml2::xml_children(node)
  
  for (c in children){
    n <- xml2::xml_name(c)
    skipping(paste("Unrecoginzed child element of parameter:", n), strict)
  }
  
  parameter$value <- xml2::xml_text(node)
  
  return(parameter)
}

#' @noRd
processStox27process <- function(node, strict){
  process <- list()
  attributes <- xml2::xml_attrs(node)
  
  process$name <- character()
  
  for(n in names(attributes)){
    if (n=="name"){
      process$name <- attributes[[n]]
    }
    else{
      skipping(paste("Unrecoginzed attribute of process:", n), strict)
    }
  }
  
  process[["function"]] <- character()
  process$enabled <- logical()
  process$respondingui <- logical()
  process$breakingui <- logical()
  process$fileoutput <- logical()
  process$parameter <- list()
  process$output <- character()
  
  children <- xml2::xml_children(node)

  for (c in children){
    n <- xml2::xml_name(c)
    if (n=="function"){
      process[["function"]] <- get_simple_content(c)
    }
    else if (n=="enabled"){
      process$enabled <- as.logical(get_simple_content(c))
    }
    else if (n=="respondingui"){
      process$respondingui <- as.logical(get_simple_content(c))
    }
    else if (n=="breakingui"){
      process$breakingui <- as.logical(get_simple_content(c))
    }
    else if (n=="fileoutput"){
      process$fileoutput <- as.logical(get_simple_content(c))
    }
    else if (n=="parameter"){
      process$parameter <- processStox27parameter(c, strict)
    }
    else if (n=="output"){
      process$output <- get_simple_content(c)
    }
    
    else{
      skipping(paste("Unrecoginzed child element of process:", n), strict)
    }
  }
  return(process)
}

#' @noRd
processStox27Model <- function(node, strict){
  model <- list()
  attributes <- xml2::xml_attrs(node)
  
  model$name <- character()
  for(n in names(attributes)){
    if (n=="name"){
      model$name <- attributes[[n]]
    }
    else{
      skipping(paste("Unrecoginzed attribute of model:", n), strict)
    }
  }
  
  model$process <- list()
  
  processIndex <- 1
  children <- xml2::xml_children(node)
  for (c in children){
    n <- xml2::xml_name(c)
    if (n=="process"){
      model$process[[processIndex]] <- processStox27process(c, strict)
      processIndex <- processIndex + 1
    }
    else{
      skipping(paste("Unrecoginzed child element of model:", n), strict)
    }
  }
  return(model)
}

processStox27ValueWattribute <- function(node, strict, attributenames){
  value <- list()
  
  for (attributename in attributenames){
    value[[attributename]] <- character()    
  }

  attributes <- xml2::xml_attrs(node)
  for(n in names(attributes)){
    
    if (n %in% attributenames){
      value[[n]] <- attributes[[n]]
    }
    else{
      skipping(paste("Unrecoginzed attribute of assignmentresolution:", n), strict)  
    }
  }
  
  children <- xml2::xml_children(node)
  for (c in children){
    n <- xml2::xml_name(c)
    skipping(paste("Unrecoginzed child element of element 'value':", n), strict)
  }
  
  value$value <- xml2::xml_text(node)
  
  return(value)
}

#' @noRd
processStox27ValueWattributeList <- function(node, strict, elementname, attributenames){
  l <- list()
  
  attributes <- xml2::xml_attrs(node)
  for(n in names(attributes)){
    skipping(paste("Unrecoginzed attribute of assignmentresolution:", n), strict)
  }
  
  l[[elementname]] <- list()
  index <- 1
  
  children <- xml2::xml_children(node)
  for (c in children){
    n <- xml2::xml_name(c)
    if (n==elementname){
      l[[elementname]][[index]] <- processStox27ValueWattribute(c, strict, attributenames)
      index <- index + 1
    }
    else{
      skipping(paste("Unrecoginzed child element of", elementname, ":", n), strict)
    }
  }
  
  return(l) 
}

#' @noRd
processStox27Processdata <- function(node, strict){
  processdata <- list()
  attributes <- xml2::xml_attrs(node)
  for(n in names(attributes)){
    skipping(paste("Unrecoginzed attribute of processdata:", n), strict)
  }
  
  processdata$bioticassignment <- list()
  processdata$suassignment <- list()
  processdata$assignmentresolution <- list()
  processdata$edsupsu <- list()
  processdata$psustratum <- list()
  processdata$stratumpolygon <- list()
  processdata$temporal <- list()
  processdata$gearfactor <- list()
  processdata$spatial <- list()
  processdata$ageerror <- list()
  processdata$stratumneighbour <- list()
  
  children <- xml2::xml_children(node)
  for (c in children){
    n <- xml2::xml_name(c)
    if (n=="bioticassignment"){
      processdata$bioticassignment <- processStox27ValueWattributeList(c, strict, "stationweight", c("assignmentid", "station"))
    }
    else if (n=="suassignment"){
      processdata$suassignment <- processStox27ValueWattributeList(c, strict, "assignmentid", c("sampleunit", "estlayer"))
    }
    else if (n=="assignmentresolution"){
      processdata$assignmentresolution <- processStox27ValueWattributeList(c, strict, "value", "variable")
    }
    else if (n=="edsupsu"){
      processdata$edsupsu <- processStox27ValueWattributeList(c, strict, "psu", "edsu")
    }
    else if (n=="psustratum"){
      processdata$psustratum <- processStox27ValueWattributeList(c, strict, "stratum", "psu")
    }
    else if (n=="stratumpolygon"){
      processdata$stratumpolygon <- processStox27ValueWattributeList(c, strict, "value", c("polygonkey", "polygonvariable"))
    }
    else if (n=="temporal"){
      processdata$temporal <- processStox27ValueWattributeList(c, strict, "value", c("covariatesourcetype", "covariate"))
    }
    else if (n=="gearfactor"){
      processdata$gearfactor <- processStox27ValueWattributeList(c, strict, "value", c("covariatesourcetype", "covariate"))
    }
    else if (n=="spatial"){
      processdata$spatial <- processStox27ValueWattributeList(c, strict, "value", c("covariatesourcetype", "covariate"))
    }
    else if (n=="ageerror"){
      processdata$ageerror <- processStox27ValueWattributeList(c, strict, "probability", c("readage", "realage"))
    }
    else if (n=="stratumneighbour"){
      processdata$stratumneighbour <- processStox27ValueWattributeList(c, strict, "value", "variable")
    }
    else{
      skipping(paste("Unrecoginzed child element of processdata:", n), strict)
    }
  }
  
  return(processdata)
}

#' @noRd
processStox27Project <- function(node, strict){
  
  project <- list()
  attributes <- xml2::xml_attrs(node)
  
  #
  # handle attributes
  #
  
  project$template <- character()
  project$lastmodified <- character()
  project$rstoxversion <- character()
  project$stoxversion <- character()
  project$rversion <- character()
  project$resourceversion <- character()
  project$version <- character()
  
  for(n in names(attributes)){
    if (n=="template"){
      project$template <- attributes[[n]]
    }
    else if (n=="lastmodified"){
      project$lastmodified <- attributes[[n]]
    }
    else if (n=="rstoxversion"){
      project$rstoxversion <- attributes[[n]]
    }
    else if (n=="stoxversion"){
      project$stoxversion <- attributes[[n]]
    }
    else if (n=="rversion"){
      project$rversion <- attributes[[n]]
    }
    else if (n=="resourceversion"){
      project$resourceversion <- attributes[[n]]
    }
    else if (n=="version"){
      project$version <- attributes[[n]]
    }
    else{
      skipping(paste("Unrecoginzed attribute of project:", n), strict)
    }
  }
  
  #
  #handle child elements
  #
  
  project$meta <- list()
  project$model <- list()
  project$processdata <- list()
  
  children <- xml2::xml_children(node)
  for (c in children){
    n <- xml2::xml_name(c)
    if (n=="meta"){
      project$meta <- processStox27Meta(c, strict)
    }
    else if (n=="model"){
      project$model <- processStox27Model(c, strict)
    }
    else if (n=="processdata"){
      project$processdata <- processStox27Processdata(c, strict)
    }
    else{
      skipping(paste("Unrecoginzed child element of project:", n), strict)
    }
  }
  return(project)
}

#' @noRd
processprocessStox27Projects <- function(root, strict){
  projects <- list()
  attributes <- xml2::xml_attrs(root)
  if (length(attributes)>0){
    skipping("attributes on element 'projects' are not recognized.", strict)
  }
  
  children <- xml2::xml_children(root)
  projectIndex = 1
  for (c in children){
    n <- xml2::xml_name(c)
    if (n=="project"){
      projects[[projectIndex]] <- processStox27Project(c, strict)
      projectIndex <- projectIndex + 1
    }
    else{
      skipping(paste("Unrecognized child element of project:", n), strict)
    }
  }
  
  return(projects)
}

#' @noRd
processStox27Xml <- function(root, strict){
  
  projects <- list()
  if (xml2::xml_name(root) == "projects"){
    projects <- processprocessStox27Projects(root, strict)
  }
  else if (xml2::xml_name(root) == "project"){
    projects[[1]] <- processprocessStox27Project(root, strict)
  }
  else{
    stop("Root name", xml2::xml_name(root), "not recognized.")
  }
  
  return(projects)
}

#'
#' Stox 2.7 project
#' 
#' xml is parsed as a netsed list with names corresponding to those used in the xml, and with the following type mappings:
#' xs:string -> character
#'  
#'
#' @name stox27project
NULL

#' Read Stox project from project xml
#' @param projectxml xml filename
#' @return Nested list representation of project
#' @export
readStox27Project <- function(projectxml, strict=T){
  tree <- xml2::read_xml(projectxml)
  
  root <- xml2::xml_root(tree)
  
  return(processStox27Xml(root, strict))
}
