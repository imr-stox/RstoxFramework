# Run the test project:
projectPaths <- system.file("test",  "cod_19.zip", package = "RstoxFramework")
expect_true(compareProjectToStoredOutputFiles(projectPaths))
