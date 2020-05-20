

# testing legacy stox reader

context("stox v 2.7 project parsing")

dummy27project <- system.file("testresources", "stox2_7_dummy_project.xml", package="RstoxFramework")

proj27 <- readStox27Project(dummy27project)
expect_true(isStox27project(proj27))
expect_true(is.null(names(proj27[[1]]$model)))

context("stox v 2.7 ECA project parsing")

ecatest <- readStox27Project(system.file("testresources", "ECA_proj.xml", package="RstoxFramework"))
expect_true(isStox27project(ecatest))

#contains one project
expect_equal(length(ecatest), 1)

#contains two models
expect_equal(length(ecatest[[1]]$model), 2)

#first model is baseline
expect_equal(ecatest[[1]]$model[[1]]$name, "baseline")

#contains two processes
expect_equal(length(ecatest[[1]]$model[[1]]$process), 2)

#second process is DefineAgeErrorMatrix
expect_equal(ecatest[[1]]$model[[1]]$process[[2]]$name, "DefineAgeErrorMatrix")

#has two parameters
expect_equal(length(ecatest[[1]]$model[[1]]$process[[2]]$parameter), 2)

# suassignment is empty (no tag in file)
expect_true("suassignment" %in% names(ecatest[[1]]$processdata[[1]]))
expect_equal(length(ecatest[[1]]$processdata[[1]]$suassignment), 0)

# bioticassignment contains one empty element (empty tag in file)
expect_equal(length(ecatest[[1]]$processdata[[1]]$bioticassignment),1)
expect_equal(length(ecatest[[1]]$processdata[[1]]$bioticassignment$stationweight),0)

# stratumpolygon contains list of 4 elements
expect_equal(length(ecatest[[1]]$processdata[[1]]$stratumpolygon$value),4)

context("stox v 2.7 loose parsing")
expect_error(readStox27Project(system.file("testresources", "ECA_proj_extra_nodes.xml", package="RstoxFramework")), "Unrecognized child element of model: insert")
expect_warning(loosetest <- readStox27Project(system.file("testresources", "ECA_proj_extra_nodes.xml", package="RstoxFramework"), strict = F), "Unrecognized child element of model: insert")
expect_true(isStox27project(loosetest))
expect_equal(length(loosetest[[1]]$model[[1]]$process[[2]]$parameter), 2)
