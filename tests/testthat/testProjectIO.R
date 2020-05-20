

# testing legacy stox reader

dummy27project <- system.file("testresources", "stox2_7_dummy_project.xml", package="RstoxFramework")

proj27 <- readStox27Project(dummy27project)
expect_true(isStox27project(proj27))
expect_true(is.null(names(proj27[[1]]$model)))