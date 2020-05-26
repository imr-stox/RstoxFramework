
context("Test NULL in function parameters")

# Create an empty project:
projectPath <- tempdir()
createProject(
    projectPath = projectPath, 
    template = "UserDefined", 
    ow = TRUE
)
# Add a process
addProcess(
    projectPath = projectPath, 
    modelName = "baseline", 
    values = list(
        functionName = "DefineStratumPolygon"
    )
)

# Expect the following function parameters:
expected <- list(
    UseProcessData <- FALSE, 
    DefinitionMethod <- character(0), 
    FileName <- NULL
)

# Get and check the function parameters:
functionParameters <- getFunctionParameters(
    projectPath = projectPath, 
    modelName = modelName, 
    processID = "P001"
)
expect_equal(functionParameters, expected)

# Reopen the project:
closeProject(projectPath = projectPath, save = TRUE)
openProject(projectPath = projectPath)

# Get and check the function parameters (here there was an error on 2020-05-25, where parameters which were NULL were deleted by setListElements()):
functionParameters <- getFunctionParameters(
    projectPath = projectPath, 
    modelName = modelName, 
    processID = "P001"
)
expect_equal(functionParameters, expected)

