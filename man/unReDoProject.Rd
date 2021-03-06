% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Framework.R
\name{unReDoProject}
\alias{unReDoProject}
\title{Function to undo or redo, i.e., reset the current project description file and change the indices. There will be separate GUI functions for undo and redo:}
\usage{
unReDoProject(projectPath, shift = 0)
}
\arguments{
\item{projectPath}{The path to the StoX project, i.e., the folder of the project with the sub folders "input", "output" and "process". Can possibly be the path to a file inside the project folder.}

\item{shift}{The position relative to the current memory status to un/redo to.}
}
\description{
Function to undo or redo, i.e., reset the current project description file and change the indices. There will be separate GUI functions for undo and redo:
}
