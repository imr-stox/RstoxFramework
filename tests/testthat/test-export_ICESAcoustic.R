# Run the test project:
projectPaths <- system.file("test",  "export_ICESAcoustic.zip", package = "RstoxFramework")
expect_true(compareProjectToStoredOutputFiles(projectPaths))
