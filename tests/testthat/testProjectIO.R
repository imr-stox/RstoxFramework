

# testing legacy stox reader

context("stox v 2.7 project parsing")

dummy27project <- system.file("testresources", "stox2_7_dummy_project.xml", package="RstoxFramework")

proj27 <- readStox27Project(dummy27project)
expect_true(isStox27project(proj27))
expect_true(is.null(names(proj27[[1]]$model)))

stop("Add test files with more than one element for unbounded elements")
stop("Add test on strict")