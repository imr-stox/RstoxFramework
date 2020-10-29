

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

#
# Legacy stox project parsers
#

#' response when unrecognized elements are encountered.
#' @param message message to user
#' @param strict logical, error is raised if strict is true, otherwise a warning is generated
#' @noRd
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
      skipping(paste("Unrecognized attribute of meta:", n), strict)
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
      skipping(paste("Unrecognized attribute of parameter:", n), strict)
    }
  }
  
  if (length(parameter$name) <1){
    skipping(paste("Missing required attribute of parameter: 'name'"), strict)
  }
  
  
  children <- xml2::xml_children(node)
  
  for (c in children){
    n <- xml2::xml_name(c)
    skipping(paste("Unrecognized child element of parameter:", n), strict)
  }
  
  parameter$parameter <- xml2::xml_text(node)
  
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
      skipping(paste("Unrecognized attribute of process:", n), strict)
    }
  }
  
  if (length(process$name) <1){
    skipping(paste("Missing required attribute of process: 'name'"), strict)
  }
  
  
  process[["function"]] <- character()
  process$enabled <- logical()
  process$respondingui <- logical()
  process$breakingui <- logical()
  process$fileoutput <- logical()
  process$parameter <- list()
  process$output <- character()
  
  children <- xml2::xml_children(node)
  
  parameterIndex <- 1

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
      process$parameter[[parameterIndex]] <- processStox27parameter(c, strict)
      parameterIndex <- parameterIndex + 1
    }
    else if (n=="output"){
      process$output <- get_simple_content(c)
    }
    
    else{
      skipping(paste("Unrecognized child element of process:", n), strict)
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
      skipping(paste("Unrecognized attribute of model:", n), strict)
    }
  }
  
  if (length(model$name) <1){
    skipping(paste("Missing required attribute of model: 'name'"), strict)
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
      skipping(paste("Unrecognized child element of model:", n), strict)
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
      nodename <- xml2::xml_name(node)
      skipping(paste("Unrecognized attribute of",nodename,":", n), strict)  
    }
  }
  
  children <- xml2::xml_children(node)
  for (c in children){
    n <- xml2::xml_name(c)
    skipping(paste("Unrecognized child element of element", xml2::xml_name(node), ":", n), strict)
  }
  
  value[[xml2::xml_name(node)]] <- xml2::xml_text(node)
  
  return(value)
}

#' @noRd
processStox27ValueWattributeList <- function(node, strict, elementname, attributenames){
  l <- list()
  
  attributes <- xml2::xml_attrs(node)
  for(n in names(attributes)){
    skipping(paste("Unrecognized attribute of assignmentresolution:", n), strict)
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
      skipping(paste("Unrecognized child element of", elementname, ":", n), strict)
    }
  }
  
  return(l) 
}

#' @noRd
processStox27Processdata <- function(node, strict){
  processdata <- list()
  attributes <- xml2::xml_attrs(node)
  for(n in names(attributes)){
    skipping(paste("Unrecognized attribute of processdata:", n), strict)
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
  processdata$platformfactor <- list()
  processdata$covparam <- list()
  
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
    else if (n=="platformfactor"){
      processdata$platformfactor <- processStox27ValueWattributeList(c, strict, "value", c("covariatesourcetype", "covariate"))
    }
    else if (n=="covparam"){
      processdata$covparam <- processStox27ValueWattributeList(c, strict, "value", c("covariatetable", "parameter"))
    }
    else{
      skipping(paste("Unrecognized child element of processdata:", n), strict)
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
    else if (n=="xmlns"){
      project$xmlns <- attributes[[n]]
    }
    else{
      skipping(paste("Unrecognized attribute of project:", n), strict)
    }
  }
  
  #
  #handle child elements
  #
  
  project$meta <- list()
  project$model <- list()
  project$processdata <- list()
  
  modelIndex <- 1
  processDataIndex <- 1
  
  children <- xml2::xml_children(node)
  for (c in children){
    n <- xml2::xml_name(c)
    if (n=="meta"){
      project$meta <- processStox27Meta(c, strict)
    }
    else if (n=="model"){
      project$model[[modelIndex]] <- processStox27Model(c, strict)
      modelIndex <- modelIndex + 1
    }
    else if (n=="processdata"){
      project$processdata[[processDataIndex]] <- processStox27Processdata(c, strict)
      processDataIndex <- processDataIndex + 1
    }
    else{
      skipping(paste("Unrecognized child element of project:", n), strict)
    }
  }
  return(project)
}

#' @noRd
processStox27Projects <- function(root, strict){
  projects <- list()
  
  projects$xmlns <- character()
  
  attributes <- xml2::xml_attrs(root)
  for(n in names(attributes)){
    if (n=="xmlns"){
      projects$xmlns <- attributes[[n]]
    }
    else{
      skipping(paste("Unrecognized attribute of projects:", n), strict)
    }
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
    projects <- processStox27Projects(root, strict)
  }
  else if (xml2::xml_name(root) == "project"){
    projects[[1]] <- processStox27Project(root, strict)
  }
  else{
    stop("Root name", xml2::xml_name(root), "not recognized.")
  }
  
  return(projects)
}

#'
#' Stox 2.7 project
#' 
#' @description
#' Nested list representation of Stox 2.7 projects.
#' 
#' @details
#' Naming and definitions follow the XML.
#' Stox 2.7 projects conforms roughly to the namespace http://www.imr.no/formats/stox/v1.2,
#' defined at http://www.imr.no/formats/stox/v1.2/stoxv1_2.xsd and in formats/stoxv2.7.xsd.
#' 
#' This schema is used with some additions in the representation, with the data type mapping:
#' \describe{
#'  \item{xs:string}{character}
#'  \item{xs:boolean}{logical}
#' }
#' 
#' In addition to structures defined in this schema, stox 2.7 may store the following structures:
#' \describe{
#'  \item{platformfactor}{list of lists with members 'value', 'covariatesourcetype', 'covariate'.}
#'  \item{covparam}{list of lists with members 'value', 'covariatetable', 'parameter'}
#' }
#'  
#' @name stox27project
#' 
NULL

#' Checks that object is a list with the given members, and that the given typechecks are OK
#' @param members character names that object should have
#' @param typecheks functions mapping object to logical for checking types of corresponding members
#' @return logical
#' @noRd
isNestedList <- function(object, members, typechecks){
  
  stopifnot(length(members) == length(typechecks))
  
  if (!is.list(object)){
    return(F)
  }
  if (!all(members %in% names(object))){
    return(F)
  }
  
  for (i in 1:length(members)){
    member <- members[[i]]
    check <- typechecks[[i]]
    if(!check(object[[member]])){
      return(F)
    }
  }

  return(T)
}

#' @noRd
isEmptyList <- function(emptylist){
  if (!is.list(emptylist)){
    return(F)
  }
  if (length(emptylist)>0){
    return(F)
  }
  
  return(T)
}

#' optional
#' @noRd
isMeta <- function(meta){
  
  if (isEmptyList(meta)){
    return(T)
  }
  
  return(isNestedList(meta, c("description", "surveyTimeseriesName"), c(is.character, is.character)))
}

#' unbounded and optional, with required member 'name'
#' @noRd
isParameter <- function(parameter){
  
  if (!is.list(parameter)){
    return(F)
  }
  if (!is.null(names(parameter))){
    return(F)
  }

  for (p in parameter){
    if (!isNestedList(p, c("name", "parameter"), c(is.character, is.character))){
      return(F)
    }    
    if (length(p$name)<1){
      return(F)
    }
  }
  
  return(T)
}

#' unbounded and optional, with required member 'name'
#' @noRd
isProcess <- function(process){
  
  if (!is.list(process)){
    return(F)
  }
  if (!is.null(names(process))){
    return(F)
  }
  
  for (p in process){
    if (!isNestedList(p, c("name", "function", "enabled", "respondingui", "breakingui", "fileoutput", "parameter", "output"), c(is.character, is.character, is.logical, is.logical, is.logical, is.logical, isParameter, is.character))){
      return(F)
    }
    if (length(p$name)<1){
      return(F)
    }
  }
  
  return(T)
}

#' unbounded and optional, with required member 'name'
#' @noRd
isModel <- function(model){
  
  if (!is.list(model)){
    return(F)
  }
  if (!is.null(names(model))){
    return(F)
  }
  
  for (m in model){
    if(!isNestedList(m, c("name", "process"), c(is.character, isProcess))){
      return(F)
    }
    if (length(m$name)<1){
      return(F)
    }
  }
  
  return(T)
}

#' Checks an optional list is containing an optional unbounded list with only character members
#' @noRd
containsOptionalListOfCharValues <- function(charlist, listname, members){
  if (isEmptyList(charlist)){
    return(T)
  }
  
  if (!(listname %in% names(charlist))){
    return(F)
  }
  
  if (!is.list(charlist[[listname]])){
    return(F)
  }
  
  if (!is.null(names(charlist[[listname]]))){
    return(F)
  }
  
  typecheckers <- c()
  for (m in members){
    typecheckers <- c(typecheckers, is.character)
  }
  
  for (entry in charlist[[listname]]){
    if (!isNestedList(entry, members, typecheckers)){
      return(F)
    }
  }
  
  return(T)
}

#' optional, with an unbounded and optional subconstruct with all character members
#' @noRd
isBioticassignment <- function(bioticassignment){
  return(containsOptionalListOfCharValues(bioticassignment, "stationweight", c("assignmentid", "station", "stationweight")))
}
#' optional, with an unbounded and optional subconstruct with all character members
#' @noRd
isSuassignment <- function(suassignment){
  return(containsOptionalListOfCharValues(suassignment, "assignmentid", c("sampleunit", "estlayer", "assignmentid")))
}
#' optional, with an unbounded and optional subconstruct with all character members
#' @noRd
isAssignmentresolution <- function(assignmentresolution){
  return(containsOptionalListOfCharValues(assignmentresolution, "value", c("variable", "value")))
}
#' optional, with an unbounded and optional subconstruct with all character members
#' @noRd
isEdsupsu <- function(edsupsu){
  return(containsOptionalListOfCharValues(edsupsu, "psu", c("edsu", "psu")))
}
#' optional, with an unbounded and optional subconstruct with all character members
#' @noRd
isPsustratum <- function(psustratum){
  return(containsOptionalListOfCharValues(psustratum, "stratum", c("psu", "stratum")))
}
#' optional, with an unbounded and optional subconstruct with all character members
#' @noRd
isStratumpolygon <- function(stratumpolygon){
  return(containsOptionalListOfCharValues(stratumpolygon, "value", c("polygonkey", "polygonvariable", "value")))
}
#' optional, with an unbounded and optional subconstruct with all character members
#' @noRd
isCovariatesourcetype <- function(temporal){
  return(containsOptionalListOfCharValues(temporal, "value", c("covariatesourcetype", "covariate", "value")))
}
#' optional, with an unbounded and optional subconstruct with all character members
#' @noRd
isAgeerror <- function(ageerror){
  return(containsOptionalListOfCharValues(ageerror, "probability", c("readage", "realage", "probability")))
}
#' optional, with an unbounded and optional subconstruct with all character members
#' @noRd
isStratumneighbour <- function(stratumneighbour){
  return(containsOptionalListOfCharValues(stratumneighbour, "value", c("variable", "value")))
}
#' optional, with an unbounded and optional subconstruct with all character members
#' @noRd
isPlatformfactor <- function(platformfactor){
  return(containsOptionalListOfCharValues(platformfactor, "value", c("covariatesourcetype", "covariate", "value")))
}
#' optional, with an unbounded and optional subconstruct with all character members
#' @noRd
isCovparam <- function(covparam){
  return(containsOptionalListOfCharValues(covparam, "value", c("covariatetable", "parameter", "value")))
}

#' unbounded and optional
#' @noRd
isProcessData <- function(processdata){
  
  if (!is.list(processdata)){
    return(F)
  }
  if (!is.null(names(processdata))){
    return(F)
  }
  
  for (p in processdata){
    if (!isNestedList(p, c("bioticassignment", "suassignment", "assignmentresolution", "edsupsu", "psustratum", "stratumpolygon", "temporal", "gearfactor", "spatial", "ageerror", "stratumneighbour", "platformfactor", "covparam"), 
                        c(isBioticassignment, isSuassignment, isAssignmentresolution, isEdsupsu, isPsustratum, isStratumpolygon, isCovariatesourcetype, isCovariatesourcetype, isCovariatesourcetype, isAgeerror, isStratumneighbour, isPlatformfactor, isCovparam))){
      return(F)
    }
  }
  
  return(T)
}

#' valid stox 2.7 project
#' @description checks if an object is a valid ~\code{\link[RstoxFramework]{stox27project}}
#' @param stox27project object to check for validity
#' @return logical true if 'stox27project' is a valid ~\code{\link[RstoxFramework]{stox27project}}
#' @export
isStox27project <- function(stox27project){
  if (!is.list(stox27project)){
    return(F)
  }
  if (length(stox27project) < 1){
    return(F)
  }
  for (project in stox27project){
    projectmembers <- c("template", "lastmodified", "rstoxversion", "stoxversion", "rversion", "resourceversion", "version", "meta", "model", "processdata")
    if (!all(projectmembers %in% names(project))){
      return(F)
    }
    if (!is.character(project$template)){
      return(F)
    }
    if (!is.character(project$lastmodified)){
      return(F)
    }
    if (!is.character(project$rstoxversion)){
      return(F)
    }
    if (!is.character(project$stoxversion)){
      return(F)
    }
    if (!is.character(project$rversion)){
      return(F)
    }
    if (!is.character(project$resourceversion)){
      return(F)
    }
    if (!is.character(project$version)){
      return(F)
    }
    
    if (!isMeta(project$meta)){
      return(F)
    }
    
    if (!isModel(project$model)){
      return(F)
    }
    
    if (!isProcessData(project$processdata)){
      return(F)
    }
    
  }
  
  return(T)
}

### #' Read Stox project from project xml
### #' @param projectxml xml filename of stox v2.7 project file
### #' @param strict logical, whether errors should be raised for unkown elements and attributes or missing required attributes, ### if False warnings will be issued.
### #' @return Nested list representation of project, formatted as: \code{\link[RstoxFramework]{stox27project}}
### #' @export
### readStox27Project <- function(projectxml, strict=T){
###   tree <- xml2::read_xml(projectxml)
###   
###   root <- xml2::xml_root(tree)
###   
###   return(processStox27Xml(root, strict))
### }
