% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Framework.R
\name{getProcessOutputFiles}
\alias{getProcessOutputFiles}
\title{Function to get all process output memory files of a process:}
\usage{
getProcessOutputFiles(
  projectPath,
  modelName,
  processID,
  onlyTableNames = FALSE,
  type = "memory"
)
}
\arguments{
\item{projectPath}{The path to the StoX project, i.e., the folder of the project with the sub folders "input", "output" and "process". Can possibly be the path to a file inside the project folder.}

\item{modelName}{The name of the model (one of "baseline", "analysis" and "report").}

\item{processID}{The ID of the process.}

\item{onlyTableNames}{Logical: If TRUE return only table names.}

\item{type}{The type of file to save the project to.}
}
\description{
Function to get all process output memory files of a process:
}
