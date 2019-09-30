#
#
# Changes done to version from AJH
# Require all function parameters to be strings (allow repititon)
# Require ProcessData to be stored in dedicated lists according to processdata type
# Can not write all elements and attributes missing from templates as empty nodes. Some have restrictions on values.
#

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
      ProcessData = list(
        StratumPolygon=list(stratum1=list(polygon="MUKLTIPOLYGIN((25)6(6)6rger)"))), 
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

#
# Tests for writing xml
#

context("save project")
tempfile <- tempfile()
# save
saveProject(projectDescription, tempfile)
# validate
data <- read_xml(tempfile)
schema <- read_xml("../../inst/formats/stoxProject.xsd")
expect_true(xml_validate(data, schema))
#read back in
reread <- readProject(tempfile)
file.remove(tempfile)
#compare
expect_equal(projectDescription$Description, reread$Description)
expect_equal(projectDescription$Baseline$ReadAcoustic$FunctionParameters$FileNames, reread$Baseline$ReadAcoustic$FunctionParameters$FileNames)
expect_gt(length(reread$Baseline$ReadAcoustic$ProcessData), length(projectDescription$Baseline$ReadAcoustic$ProcessData))

# validate dummy file
tempfile <- tempfile()
project <- readProject("../../inst/testresources/dummy_project.xml")
saveProject(project, tempfile)
data <- read_xml(tempfile)
schema <- read_xml("../../inst/formats/stoxProject.xsd")
expect_true(xml_validate(data, schema))
file.remove(tempfile)


#
# Tests for reading xml
#

context("read project")
# path should be relative to testthat directory for working with devtools::test()
project <- readProject("../../inst/testresources/dummy_project.xml")
expect_true(all(names(project) %in% c("Template", "Rversion", "Description", "Baseline", "Lastmodified", "Statistics", "Rstoxversion", "Report", "Stoxversion", "RstoxDependencies")))
expect_true(all(c("StratumPolygon", "BioticAssignment") %in% names(project$Baseline[[1]]$ProcessData)))
expect_equal(project$Baseline$str1234$ProcessData$StratumNeighbour$str1234, "str1234")


#
# Tests for function parameter type handling
#

context("function parameter data types")

tempfile <- tempfile()
# save
saveProject(projectDescription, tempfile)
# validate
data <- read_xml(tempfile)
schema <- read_xml("../../inst/formats/stoxProject.xsd")
expect_true(xml_validate(data, schema))
#read back in
reread <- readProject(tempfile)
file.remove(tempfile)
expect_equal(class(projectDescription$Baseline$DefineStrata$FunctionParameters$FileNames), class(reread$Baseline$DefineStrata$FunctionParameters$FileNames))
expect_equal(class(projectDescription$Baseline$DefineStrata$FunctionParameters$UseProcessData), class(reread$Baseline$DefineStrata$FunctionParameters$UseProcessData))

# expected errors writing
tempfile <- tempfile()
wrongobject <- projectDescription
wrongobject$Baseline$DefineStrata$FunctionParameters$Area <- as.factor(c(1.2,2.1))
expect_error(saveProject(wrongobject, tempfile))
if (file.exists(tempfile)){
  file.remove(tempfile)
}

# expected errors reading
expect_error(project <- readProject("../../inst/testresources/dummy_project_functionparametererror.xml"))