# Run the test project:
projectPaths <- system.file("test",  "catch_99.zip", package = "RstoxFramework")
expect_true(compareProjectToStoredOutputFiles(projectPaths))
