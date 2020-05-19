

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
