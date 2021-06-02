# Run the test project:
projectPaths <- system.file("test",  "split_18.zip", package = "RstoxFramework")
expect_true(compareProjectToStoredOutputFiles(projectPaths))
