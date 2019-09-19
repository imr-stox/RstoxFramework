# # By default, retrun only information about the function:
# 
# # From Åsmund on 2019-05-15:
# 
# StoX GUI needs the following:
#     
#     There should be a function getting the outputFileNames given the outputDataType.
# 
# getOutputFileNames <- function(processName, projectName, fileExt="txt") {
#     
#     # Get the status of the project:
#     status <- getProjectStatus(projectName)
#     # Get the function name from the status:
#     functionName <- status[[processName]]$functionName
#     # Get the process index:
#     processIndex <- status[[processName]]$processIndex
#     
#     # Get meta information about of function:
#     meta <- do.call(functionName, list())
#     # Get the table names of the data type of the function:
#     dataType <- meta$outputDataType
#     # Get the output table names:
#     outputTableNames <- ***********************************************
#         
#         # Concatinate the index of the process, the process name, the data type, and the output tables:
#         outputFileNames <- paste(processIndex, processName, dataType, outputTableNames, sep="_")
#     
#     # Append file extension:
#     outputFileNames <- paste(outputFileNames, fileExt, sep=".")
#     
#     outputFileNames
# }
# 
# Static:
#     - functionName
# - parametersToShow: A function of the StoX parameters of the function given in a ... argument, returning # the parameters to show in StoXGUI given the values of the inputs. 
# - outputDataType
# - parentModel (should a StoX function only be used in one model?)
# 
# UserDefined (defaults should be stored in the meta information):
#     - processName
# - enabled
# - respondInGUI
# - breakInGUI
# - exportCSV Use fileOutput instead as today!
# - parameterValue
# 
# Status: 
#     - hasError
# - isPerformed
# 
# FromFormals: 
#     - parameterName
# - parameterDefaultValue
# - parameterPossibleValues
# - parameterDescription
# 
# 
# 
# 
# StoXparameterNames = parameterNames[isUpperFirstLetter(parameterNames)]
# 
# 
# metainfo <- list(
#     
    