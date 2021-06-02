# Run the test project:
projectPaths <- system.file("test",  "tobis_20.zip", package = "RstoxFramework")
expect_true(compareProjectToStoredOutputFiles(projectPaths))
