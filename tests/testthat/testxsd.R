context("stox process xsd")
#library(xml2)
#library(RstoxData)
schema <- xml2::read_xml("../../inst/formats/stoxProject.xsd")
expect_silent(processedschema <- RstoxData:::processXSD(schema))
expect_true("project" %in% processedschema$rootInfo)
xsdObjects$stoxv3.xsd <- RstoxData:::createXsdObject("../../inst/formats/stoxProject.xsd")

# consider pending fix in RstoxData
#context("read dummy xml")
#readXmlFile("../../inst/testresources/dummy_project.xml", F, "stoxv3")
