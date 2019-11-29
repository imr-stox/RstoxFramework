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

#' Parses covariate definitions from xml
process_covardef <- function(node){
  
  table <- data.table(covariatesourcetype=character(), covariate=character(), value=character())
  for (cc in xml_children(node)){
    sourcestype <- xml_attr(cc, "covariatesourcetype")
    covariate <- xml_attr(cc, "covariate")
    value <- xml_text(cc, T)
    table <- rbind(table, data.table(covariatesourcetype=c(sourcestype), covariate=c(covariate), value=c(value)))
  }
  return(table)
}

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
      for (cc in xml_children(c)){
        pd$BioticAssignment <- rbind(pd$BioticAssignment, data.table(assignmentid=c(xml_attr(cc, "assignmentid")), station=c(xml_attr(cc, "station")), stationweight=c(xml_text(cc, T))))  
      }
    }
    else if (n=="suassignment"){
      for (cc in xml_children(c)){
        pd$SuAssignment <- rbind(pd$SuAssignment, data.table(sampleunit=c(xml_attr(cc, "sampleunit")), estlayer=c(xml_attr(cc, "estlayer")), assignmentid=c(xml_text(cc, T))))  
      }
    }
    else if (n=="assignmentresolution"){
      for (cc in xml_children(c)){
        var <- xml_attr(cc, "variable")
        value <- xml_text(cc, T)
        pd$AssignmentResolution[[var]]<-value
      }
    }
    else if (n=="edsupsu"){
      for (cc in xml_children(c)){
        edsu <- xml_attr(cc, "edsu")
        value <- xml_text(cc, T)
        pd$EdsuPsu[[edsu]]<-value
      }
    }
    else if (n=="psustratum"){
      for (cc in xml_children(c)){
        psu <- xml_attr(cc, "psu")
        value <- xml_text(cc, T)
        pd$PsuStratum[[psu]]<-value
      }
    }
    else if (n=="stratumpolygon"){
      for (cc in xml_children(c)){
        key <- xml_attr(cc, "polygonkey")
        var <- xml_attr(cc, "polygonvariable")
        value <- xml_text(cc, T)
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
      for (cc in xml_children(c)){
        covariatetable <- xml_attr(cc, "covariatetable")
        parameter <- xml_attr(cc, "parameter")
        value <- xml_text(cc, T)
        if (is.null(pd$CovParam[[covariatetable]])){
          pd$CovParam[[covariatetable]]<-list()          
        }
        pd$CovParam[[covariatetable]][[parameter]]<-value
      }
    }
    else if (n=="ageerror"){
      for (cc in xml_children(c)){
        readage <- xml_attr(cc, "readage")
        realage <- xml_attr(cc, "realage")
        value <- xml_text(cc, T)
        pd$AgeError <-rbind(pd$AgeError, data.table(readage=c(readage), realage=c(realage), probability=c(value)))
      }
    }
    else if (n=="stratumneighbour"){
      for (cc in xml_children(c)){
        stratum <- xml_attr(cc, "variable")
        neighbour <- xml_text(cc, T)
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
    n <- xml_name(c)
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

#' Create XML node for processparameters
#' @keywords internal
#' @noRd
getProcessParametersXml <- function(processparameters){
  node <- xml_new_root("processparameters")
  
  enabled <- xml_new_root("enabled")
  xml_text(enabled) <- tolower(as.character(processparameters$Enabled))
  xml_add_child(node, enabled)
  
  if (!is.null(processparameters$BreakInGUI)){
    bi <- xml_new_root("breakingui")
    xml_text(bi) <- tolower(as.character(processparameters$BreakInGUI))
    xml_add_child(node, bi)
  }
  
  fo <- xml_new_root("fileoutput")
  xml_text(fo) <- tolower(as.character(processparameters$FileOutput))
  xml_add_child(node, fo)
  
  return(node)
}

#' Get xml for covariate definitions
#' @keywords internal
#' @noRd
getCovarDefXml <- function(dataframe, rootname){
  rootnode <- xml_new_root(rootname)
  nodename <- names(dataframe)[3]
  att1 <- names(dataframe)[1]
  att2 <- names(dataframe)[2]
  for (i in 1:nrow(dataframe)){
    node <- xml_new_root(nodename)
    xml_attr(node, att1) <- dataframe[[att1]][[i]]
    xml_attr(node, att2) <- dataframe[[att2]][[i]]
    xml_text(node) <- dataframe[[nodename]][[i]]
    xml_add_child(rootnode, node)
  }
  return(rootnode)
}

#' Create XML node for processdata
#' @keywords internal
#' @noRd
getProcessDataXml <- function(processdata){
  node <- xml_new_root("processdata")
  
  if (!is.null(processdata$BioticAssignment)){
    pd <- xml_new_root("bioticassignment")
    for (i in 1:nrow(processdata$BioticAssignment)){
      assignment <- xml_new_root("stationweight")
      xml_attr(assignment, "assignmentid") <- processdata$BioticAssignment$assignmentid[i]
      xml_attr(assignment, "station") <- processdata$BioticAssignment$station[i]
      xml_text(assignment) <- processdata$BioticAssignment$stationweight[i]
      xml_add_child(pd, assignment)
    }
    xml_add_child(node, pd)
  }


  if (!is.null(processdata$SuAssignment)){
    pd <- xml_new_root("suassignment")
    for (i in 1:nrow(processdata$SuAssignment)){
      assignment <- xml_new_root("assignmentid")
      xml_attr(assignment, "sampleunit") <- processdata$SuAssignment$sampleunit[i]
      xml_attr(assignment, "estlayer") <- processdata$SuAssignment$estlayer[i]
      xml_text(assignment) <- processdata$SuAssignment$assignmentid
      xml_add_child(pd, assignment)
    }
    xml_add_child(node, pd)
  }
  
  if (!is.null(processdata$AssignmentResolution)){
    pd <- xml_new_root("assignmentresolution")
    for (n in names(processdata$AssignmentResolution)){
      assignment <- xml_new_root("value")
      xml_attr(assignment, "variable") <- n
      xml_text(assignment) <- processdata$AssignmentResolution[[n]]
      xml_add_child(pd, assignment)
    }
    xml_add_child(node, pd)
  }
  
  if (!is.null(processdata$EdsuPsu)){
    pd <- xml_new_root("edsupsu")
    for (n in names(processdata$EdsuPsu)){
      psu <- xml_new_root("psu")
      xml_attr(psu, "edsu") <- n
      xml_text(psu) <- processdata$EdsuPsu[[n]]
      xml_add_child(pd, psu)
    }
    xml_add_child(node, pd)
  }
  
  if (!is.null(processdata$PsuStratum)){
    pd <- xml_new_root("psustratum")
    for (n in names(processdata$PsuStratum)){
      stratum <- xml_new_root("stratum")
      xml_attr(stratum, "psu") <- n
      xml_text(stratum) <- processdata$PsuStratum[[n]]
      xml_add_child(pd, stratum)
    }
    xml_add_child(node, pd)
  }

  if (!is.null(processdata$StratumPolygon)){
    pd <- xml_new_root("stratumpolygon")
    for (n in names(processdata$StratumPolygon)){
      for (entry in names(processdata$StratumPolygon[[n]])){
        value <- xml_new_root("value")
        xml_attr(value, "polygonkey") <- n
        xml_attr(value, "polygonvariable") <- entry
        xml_text(value) <- processdata$StratumPolygon[[n]][[entry]]
        xml_add_child(pd, value)
      }
    }
    xml_add_child(node, pd)
  }

  if (!is.null(processdata$Temporal)){
    xml_add_child(node, getCovarDefXml(processdata$Temporal, "temporal"))  
  }

  if (!is.null(processdata$Gearfactor)){
    xml_add_child(node, getCovarDefXml(processdata$Gearfactor, "gearfactor"))  
  }
  
  if (!is.null(processdata$Spatial)){
    xml_add_child(node, getCovarDefXml(processdata$Spatial, "spatial"))  
  }
  
  if (!is.null(processdata$Platformfactor)){
    xml_add_child(node, getCovarDefXml(processdata$Platformfactor, "platformfactor"))  
  }
  
  
  if (!is.null(processdata$CovParam)){
    pd <- xml_new_root("covparam")
    for (n in names(processdata$CovParam)){
      for (entry in names(processdata$CovParam[[n]])){
        value <- xml_new_root("value")
        xml_attr(value, "covariatetable") <- n
        xml_attr(value, "parameter") <- entry
        xml_text(value) <- processdata$CovParam[[n]][[entry]]
        xml_add_child(pd, value)
      }
    }
    xml_add_child(node, pd)
  }

  if (!is.null(processdata$AgeError)){
    pd <- xml_new_root("ageerror")
    for (i in 1:nrow(processdata$AgeError)){
      ageerror <- xml_new_root("probability")
      xml_attr(ageerror, "readage") <- processdata$AgeError$readage[i]
      xml_attr(ageerror, "realage") <- processdata$AgeError$realage[i]
      xml_text(ageerror) <- processdata$AgeError$probability[i]
      xml_add_child(pd, ageerror)
    }
    xml_add_child(node, pd) 
  }
  
  if (!is.null(processdata$StratumNeighbour)){
    pd <- xml_new_root("stratumneighbour")
    for (n in names(processdata$StratumNeighbour)){
      for (v in processdata$StratumNeighbour[[n]]){
        value <- xml_new_root("value")
        xml_attr(value, "variable") <- n
        xml_text(value) <- v
        xml_add_child(pd, value)
      }
    }
    xml_add_child(node, pd)
  }
  
  return(node)
}

#' Create XML node for function parameter
#' @keywords internal
#' @noRd
getFunctionParameterXml <- function(name, paramvalue, paramclass){
  paramnode <- xml_new_root("functionparameter")
  xml_attr(paramnode, "name") <- name
  
  if (paramclass=="character"){
    xml_attr(paramnode, "paramtypename") <- "character"
    xml_text(paramnode) <- as.character(paramvalue)    
  }
  else if (paramclass=="integer"){
    xml_attr(paramnode, "paramtypename") <- "integer"
    xml_text(paramnode) <- as.character(paramvalue)    
  }
  else if (paramclass=="numeric"){
    xml_attr(paramnode, "paramtypename") <- "numeric"
    xml_text(paramnode) <- as.character(paramvalue)    
  }
  else if (paramclass=="logical"){
    xml_attr(paramnode, "paramtypename") <- "logical"
    xml_text(paramnode) <- as.character(paramvalue)    
  }
  else{
    stop(paste("Function parameter of class", paramclass, "is not supported."))
  }
  
  return(paramnode)
}

#' Create XML node for process
#' @param model nested list representation of model
#' @param baselineprocess logical, whether to write baseline process (with processdata)
#' @keywords internal
#' @noRd
getProcessXml <- function(process, baselineprocess=F){
  
  if (baselineprocess){
    node <- xml_new_root("baselineprocess")
  }
  else{
    node <- xml_new_root("process")  
  }
  
  
  functionnamenode <- xml_new_root("functionname")
  xml_text(functionnamenode) <- process$FunctionName
  xml_add_child(node, functionnamenode)
  
  xml_add_child(node, getProcessParametersXml(process$ProcessParameters))

  for (n in names(process$FunctionInputs)){
    finode <- xml_new_root("functioninput")
    xml_attr(finode, "dataparameter") <- n
    xml_attr(finode, "inputprocessname") <- process$FunctionInputs[[n]]
    xml_add_child(node, finode)
  }
  
    
  for (n in names(process$FunctionParameters)){
    
    for (p in process$FunctionParameters[[n]]){
      xml_add_child(node, getFunctionParameterXml(n, p, class(process$FunctionParameters[[n]])))
    }
  }
  
  if (!is.null(process$Output)){
    outputnode <- xml_new_root("output")
    xml_text(outputnode) <- process$Output
    xml_add_child(node, outputnode)
  }
  
  if (baselineprocess){
    xml_add_child(node, getProcessDataXml(process$ProcessData))
  }
  

  
  return(node)
}

#' Create XML node for model
#' @param model nested list representation of model
#' @param baselinemodel logical, whether to write baselinemodel
#' @param nodename name to use for node
#' @keywords internal
#' @noRd
getModelXml <- function(model, baselinemodel, nodename){
  node <- xml_new_root(nodename)
  for (n in names(model)){
    processnode <- getProcessXml(model[[n]], baselinemodel)
    xml_attr(processnode, "processname") <- n 
    xml_add_child(node, processnode)
  }
  return(node)
}

#' Create XML node for restox dependencies
#' @param rstoxdependencies nested list representation of model
#' @param baselinemodel logical, whether to write baselinemodel
#' @param nodename name to use for node
#' @keywords internal
#' @noRd
getRstoxDependenciesXml <- function(rstoxdependencies){
  node <- xml_new_root("rstoxdependencies")
  for (n in names(rstoxdependencies)){
    depnode <- xml_new_root("rlibrary")
    xml_attr(depnode, "library") <- n
    xml_attr(depnode, "version") <- rstoxdependencies[[n]]
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

#' Read Stox project from project xml
#' @param projectxml xml filename
#' @return Nested list representation of project
#' @export
readProject <- function(projectxml){
  tree <- xml2::read_xml(projectxml)
  
  root <- xml2::xml_root(tree)
  
  projectDescription <- list()
  return(process_xml(root))
}

#' Save Stox project to project xml file using namespace http://www.imr.no/formats/stox/v3
#' @param projectDescription nested list representation of StoX project
#' @param filename to write to
#' @export
saveProjectEdvin <- function(projectDescription, filename){
  namespace = "http://www.imr.no/formats/stox/v3"
  
  doc <- xml_new_document()
  xml_add_child(doc, "project")
  root <- xml_root(doc)
  xml_attr(root, "xmlns") <- namespace
  
  xml_attr(root, "template") <- projectDescription$Template
  xml_attr(root, "description") <- projectDescription$Description
  if (!is.null(projectDescription$Lastmodified)){
    xml_attr(root, "lastmodified") <- gsub("\ ","T",as.character(projectDescription$Lastmodified))
  }
  xml_attr(root, "rstoxversion") <- projectDescription$Rstoxversion
  xml_attr(root, "stoxversion") <- projectDescription$Stoxversion
  xml_attr(root, "rversion") <- projectDescription$Rversion
  
  xml_add_child(root, getModelXml(projectDescription$Baseline, T, "baselinemodel"))
  xml_add_child(root, getModelXml(projectDescription$Statistics, F, "statistics"))
  xml_add_child(root, getModelXml(projectDescription$Report, F, "report"))
  xml_add_child(root, getRstoxDependenciesXml(projectDescription$RstoxDependencies))
  
  write_xml(doc, filename)
}
