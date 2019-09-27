library(xml2)

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

#' Parses function inputs from xml
#'@keywords internal
#'@noRd
process_processparameters <- function(node){
  
  attributes <- xml2::xml_attrs(node)
  for (n in names(attributes)){
      stop(paste("Parsing of attribute", n, "not supported"))
  }
  
  
  fi <- list()
  fi$Enabled <- NULL
  fi$FileOutput <- NULL 
  fi$BreakInGUI <- NULL
  
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

#'Parses nonbaseline process from xml
#'@keywords internal
#'@noRd
parse_nonbaselineprocess <- function(node){
  
  process <- list()
  process$processname <- NULL
  
  attributes <- xml2::xml_attrs(node)
  for (n in names(attributes)){
    if (n=="processname"){
      process$ProcessName <- attributes[n]
    }
    else{
      stop(paste("Parsing of attribute", n, "not supported"))
    }
  }
  
  process$FunctionName <- NULL
  process$ProcessParameters <- list()
  process$FunctionParameters <- list()
  process$FunctionInputs <- list()
  process$output <- NULL
  
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
      if (length(xml2::xml_attrs(c))!=1){
        stop(paste("Unexpected number of attributes for element", n))
      }
      parname <-xml2::xml_attr(c, "name")
      parvalue <- xml2::xml_text(c,trim=T)
      process$FunctionParameters[[parname]] <- parvalue
    }
    else if (n == "output"){
      process$output <- get_simple_content(c)
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
  deps <- list()
  warning("Not implemented")
  return(list()) 
}

#'Parses non-baseline models from xml
#'@keywords internal
#'@noRd
process_nonbaselinemodel <- function(node){
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
      model[[processname]] <- parse_nonbaselineprocess(c)
    }
    else{
        stop(paste("Parsing of element", n, "not supported"))
    }
  }
  
  return(model) 
}

#'Parses baseline from xml
#'@keywords internal
#'@noRd
process_baseline <- function(node){
  baseline <- list()
  warning("Not implemented")
  return(list()) 
}

#'Parses project from xml
#'@keywords internal
#'@noRd
process_project <- function(projectDescription, node){
  
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
  
  projectDescription$Template <- NULL
  projectDescription$Description <- NULL
  projectDescription$Lastmodified <- NULL
  projectDescription$Rstoxversion <- NULL
  projectDescription$Stoxversion <- NULL
  projectDescription$Rversion <- NULL

  for(n in names(attributes)){
    if (n=="template"){
      projectDescription$Template <- attributes[[n]]
    }
    else if (n=="description"){
      projectDescription$Description <- attributes[[n]]
    }
    else if (n=="lastmodified"){
      projectDescription$Lastmodified <- attributes[[n]]
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
      projectDescription$Baseline <- process_baseline(c)
    }
    else if (n=="statistics"){
      projectDescription$Statistics <- process_nonbaselinemodel(c)
    }
    else if (n=="report"){
      projectDescription$Report <- process_nonbaselinemodel(c)
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
process_xml <- function(projectDescription, root){
  return(process_project(projectDescription, root))
}

#' Read Stox porject from project xml
#' @param projectxml xml filename
#' @return Nested list representation of project
#' @export
readProject <- function(projectxml){
  tree <- xml2::read_xml(projectxml)
  
  root <- xml2::xml_root(tree)
  
  projectDescription <- list()
  return(process_xml(projectDescription, root))
}

projectDescription <- list(
  Description = "fasdvabadf", 
  Baseline = list(
    ReadAcoustic = list(
      ProcessName = "ReadAcoustic", 
      FunctionName = "ReadAcoustic", 
      ProcessParameters = list(
        Enabled = TRUE, 
        BreakInGUI = FALSE, 
        FileOutput = TRUE
      ), 
      ProcessData = list(), 
      FunctionParameters = list(
        FileNames = c(
          "input/acoustic/Echosounder-1618.xml", 
          "input/acoustic/Echosounder-201605.xml", 
          "input/acoustic/Echosounder-2016205.xml", 
          "input/acoustic/Echosounder-2016857.xml", 
          "input/acoustic/Echosounder-A6-2016.xml"
        )
      ), 
      FunctionInputs = list(
        BioticData = "FilterBiotic", 
        Density = "AcousticDensity"
      )
    ), 
    DefineStrata = list(
      ProcessName = "DefineStrata", 
      FunctionName = "ReadAcoustic", 
      ProcessParameters = list(
        Enabled = TRUE, 
        BreakInGUI = FALSE, 
        FileOutput = TRUE
      ), 
      ProcessData = list("MUKLTIPOLYGIN((25)6(6)6rger)"), 
      FunctionParameters = list(
        FileNames = c(
          "input/acoustic/Echosounder-1618.xml"
        ), 
        UseProcessData = TRUE
      ), 
      FunctionInputs = list(
        BioticData = "FilterBiotic", 
        Density = "AcousticDensity", 
        StoxAcousticData = NA
      )
    ), 
    StoxAcoustic = list(
      ProcessName = "StoxAcoustic", 
      FunctionName = "StoxAcoustic", 
      ProcessParameters = list(
        Enabled = TRUE, 
        BreakInGUI = FALSE, 
        FileOutput = TRUE
      ), 
      ProcessData = list(), 
      FunctionParameters = list(
        FileNames = c(
          "input/acoustic/Echosounder-1618.xml", 
          "input/acoustic/Echosounder-201605.xml", 
          "input/acoustic/Echosounder-2016205.xml", 
          "input/acoustic/Echosounder-2016857.xml", 
          "input/acoustic/Echosounder-A6-2016.xml"
        )
      ), 
      FunctionInputs = list(
        BioticData = "FilterBiotic"
      )
    )
  ),
  
  Statistics = list(
    runBootstrap = list(
      ProcessName = "runBootstrap", 
      FunctionName = "runBootstrap", 
      ProcessParameters = list(
        Enabled = TRUE, 
        FileOutput = TRUE
      ), 
      FunctionParameters = list(
        bootstrapMethod = "AcousticTrawl", 
        acousticMethod = "PSU~Stratum", 
        bioticMethod = "PSU~Stratum", 
        startProcess = "TotalLengthDist", 
        endProcess = "SuperIndAbundance", 
        nboot = 50, 
        seed = 1234, 
        cores = 1
      )
    )
  ),
  
  
  Reports = list(
    reportAbundance = list(
      ProcessName = "reportAbundance", 
      FunctionName = "reportAbundance", 
      ProcessParameters = list(
        Enabled = TRUE, 
        FileOutput = TRUE
      ), 
      FunctionParameters = list(
        var = "count", 
        grp1 = "age",
        grp2 = "sex"
      )
    )
  )
  
)