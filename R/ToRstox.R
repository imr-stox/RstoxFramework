# This file contains stuff to be moved to Rstox, and not in RstoxFramework:

initiateRstoxFramework <- function(){
    
    #### Define the default root directories of StoX: ####
    stoxRootDir <- "~/workspace/stox"
    stoxProjectDir <- file.path(stoxRootDir, "project")
    stoxResourceDir <- file.path(stoxResourceDir, "project")
    #stoxReferenceDir <- file.path(stoxRootDir, "reference")
}

##################################################
##################################################
#' Create the StoX root directory
#' 
#' This function creates the StoX root directory, which contains the "project" folder serving as the default location of StoX projects, and possibly other folders.
#' 
#' @return
#' The status of creating the folders.
#' 
#' @noRd
#' @seealso Use \code{\link{getRstoxFrameworkDefinitions}} to get the definitions.
#' 
createStoxRoot <- function(stoxRootDir = NULL, showWarnings = TRUE) {
    
    # Get the default paths to the StoX root:
    if(length(stoxRootDir) == 0) {
        stoxRootDir <- getRstoxFrameworkDefinitions("stoxRootDir")
    }
    stoxProjectDir <- getRstoxFrameworkDefinitions("stoxProjectDir")
    #stoxReferenceDir <- getRstoxFrameworkDefinitions("stoxReferenceDir")
    stoxResourceDir <- getRstoxFrameworkDefinitions("stoxResourceDir")
    
    # Create the directories:
    if(!dir.exists(stoxRootDir)) {
        message("Creating the directory ", stoxRootDir)
        stoxRootDir_status <- dir.create(stoxRootDir, showWarnings = showWarnings, recursive = TRUE)
    }
    if(!dir.exists(stoxProjectDir)) {
        message("Creating the directory ", stoxProjectDir)
        stoxProjectDir_status <- dir.create(stoxProjectDir, showWarnings = showWarnings)
    }
    if(!dir.exists(stoxResourceDir)) {
        message("Creating the directory ", stoxResourceDir)
        stoxProjectDir_status <- dir.create(stoxResourceDir, showWarnings = showWarnings)
    }
    
    c(
        stoxRootDir_status = stoxRootDir_status, 
        stoxProjectDir_status = stoxProjectDir_status, 
        stoxResourceDir = stoxResourceDir
    )
}

##################################################
##################################################
#' Download files ftom and FTP sever, possibly recursively in the folder structure of the server
#' 
#' The function \code{download_files_ftp} downloads files form an FTP srever using the function \code{download_files_ftp} which list files and directories.
#' 
#' @param ftp	    The path to an ftp server.
#' @param dir       The path to the directory in which to place the files. 
#' @param quiet     Logical: If TRUE suppress download messages.
#' @param recursive Logical: If TRUE recurse into sub folders.
#' 
#' @return
#' A data frame the columns \code{path} and \code{isDir}.
#' 
#' @export
#' @rdname download_files_ftp
#' 
download_files_ftp <- function(ftp, dir, quiet = TRUE, recursive = FALSE) {
    
    # Get the paths of the files on the ftp server:
    ftpPaths <- list_files_ftp(ftp, quiet = quiet, recursive = recursive)
    # If the 'dirs' is empty, return NULL:
    if(length(ftpPaths) == 0) {
        warning("No files downloaded. Check internett connection.")
        return(NULL)
    }
    ftpPaths <- subset(ftpPaths, !isDir)$path
    
    # Get the relative paths, and construct the file paths to download the files to:
    relativePaths <- sub(ftp, "", ftpPaths)
    filePaths <- file.path(dir, relativePaths)
    
    # Create all unique directories:
    uniqueDirectories <- unique(dirname(filePaths))
    suppressWarnings(lapply(uniqueDirectories, dir.create, recursive = recursive))
    
    # Download the files:
    if(length(ftpPaths)) {
        status <- mapply(download.file, ftpPaths, filePaths, quiet = quiet)
        if(any(status != 0)) {
            warning("The following files were not downloaded:\n ", paste("\t", ftpPaths[status != 0], collapse = "\n"))
        }
    }
    else {
        message("No files found. Maybe try recursive = TRUE.")
    }
    
    filePaths
}
#' 
#' @export
#' @rdname download_files_ftp
#' 
list_files_ftp <- function(ftp, quiet = TRUE, recursive = FALSE) {
    
    # Small function to remove the trailing slash of a file path:
    removeTrailingSlash <- function(x) {
        gsub('^/|/$', '', x)
    }
    # Function to read the contents of a ftp server (non-recursively):
    get_ftp_info <- function(ftp, quiet = TRUE) {
        
        # Download the list of contents of the ftp server to a temporary file (return NULL if failing):
        destfile <- tempfile()
        #download.file(ftp, destfile, quiet = quiet)
        err <- try(
            download.file(ftp, destfile, quiet = quiet), silent = TRUE
        )
        if(class(err) == "try-error") {
            warning("URL ", ftp, " not found.")
            return(NULL)
        }
        
        # Pick out the last column of the table returned from the frp server:
        dirs <- readLines(destfile)
        permission <- substr(dirs, 1, 10)
        fileName <- substring(dirs, 57)
        
        # Output the file name and permission
        data.frame(
            permission = permission, 
            fileName = fileName, 
            stringsAsFactors = FALSE
        )
    }
    
    # Get the file names of the current ftp path:
    dirs <- get_ftp_info(ftp, quiet = quiet)
    # If the 'dirs' is empty, return NULL:
    if(length(dirs) == 0) {
        return(NULL)
    }
    
    out <- NULL
    # Recursive downloading:
    for(ind in seq_len(nrow(dirs))) {
        
        # Detect whether the path is a directory:
        isDir <- startsWith(dirs$permission[ind], "d")
        
        # Define the current path in the for loop:
        ftpSansSlash <- removeTrailingSlash(ftp)
        folderPath <- file.path(ftpSansSlash, dirs$fileName[ind], "")
        folderPathSansSlash <- removeTrailingSlash(folderPath)
        
        # If the current path is a directory, recurse into that directory and store the output: 
        if(recursive && isDir) {
            temp <- list_files_ftp(ftp = folderPath, recursive = TRUE)
            # Add both the folder and the output from list_files_ftp to the output:
            out <- rbind(
                out, 
                data.frame(
                    path = folderPath, 
                    isDir = isDir, 
                    stringsAsFactors = FALSE
                ), 
                temp
            )
        }
        else {
            # Add the file path to the output:
            out <- rbind(
                out, 
                data.frame(
                    path = folderPathSansSlash, 
                    isDir = isDir, 
                    stringsAsFactors = FALSE
                )
            )
        } 
    }
    
    # Return the vector of pahts:
    out
}


##################################################
##################################################
#' Download StoX reference files
#' 
#' This function downloads reference files from the StoX ftp server.
#' 
#' @inheritParams download_files_ftp
#' 
#' @return
#' A data frame the columns \code{path} and \code{isDir}.
#' 
#' @export
#' @rdname downloadStoxReference
#' 
downloadStoxReference <- function(ftp = "ftp://ftp.imr.no/StoX/Download/reference/", quiet = TRUE) {
    
    # Get the path to the directory in which to place the reference data, and download the reference data:
    StoxReferenceDir <- getRstoxFrameworkDefinitions(StoxReferenceDir)
    # Create the StoxReferenceDir if missing.
    if(!file.exists(StoxReferenceDir)) {
        dir.create(StoxReferenceDir)
    }
    download_files_ftp_recursive(ftp = ftp, dir = StoxReferenceDir, quiet = quiet)
}

# system.time(l <- downloadStoxReference())




someCreateProjectFunctionInRstoxNotRstoxFramework <- function(ProjectPath, ProjectDirectory = NULL) {
    
    # Get the paths of the root directory and StoX skeleton:
    stoxProjectDir <- getRstoxFrameworkDefinitions("stoxProjectDir")
    stoxFolderStructure <- getRstoxFrameworkDefinitions("stoxFolderStructure")
    
    # If the ProjectDirectory is not given, set it to the default StoxRoot:
    if(length(ProjectDirectory) == 0) {
        ProjectDirectory <- stoxProjectDir
    }
    
    ProjectPath <- file.path(ProjectDirectory, ProjectName)
    
    # Check whether the project exists:
    if(dir.exists(ProjectPath)) {
        warning("The project '", ProjectPath, "' exists. Choose another name or another location.")
        return(NULL)
    }
    else {
        ProjectSkeleton <- file.path(ProjectPath, StoxFolderStructure)
        lapply(ProjectSkeleton, dir.create, showWarnings = FALSE, recursive = TRUE)
    }
    
    # Return the paths:
    ProjectSkeleton
}


