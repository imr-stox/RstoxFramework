

#' Function to get the non Rstox dependencies of a package
#' 
#' @param packageName The packages to get dependencies from.
#' @param dependencies A character vector listing the dependencies to get, with possible values "Depends", "Imports", "LinkingTo", "Suggests", "Enhances". Default is NA, implying c("Depends", "Imports", "LinkingTo").
#' @param Rstox.repos,nonRstox.repos Either NULL to search for packages locally, or a repository passed to \code{\link[utils]{available.packages}}, where \code{Rstox.repos} is used to locate direct dependencies of the Rstox packages, and \code{nonRstox.reposs} is used to get the dependencies of the direct dependencies recursievly, defaulted to the CRAN repository.
#' @param sort Logical: If TRUE sort the dependent packages, defaulted to FALSE to enable installing the most basic dependencies first.
#' @param destdir The directory to download binaries to, defaulted to NA which implies tempdir().
#' 
#' @export
#' 
getNonRstoxDependencies <- function(
    packageName = c("RstoxFramework","RstoxBase", "RstoxData"), 
    dependencies = NA, 
    Rstox.repos = NULL, 
    nonRstox.repos = "https://cloud.r-project.org", 
    sort = FALSE
) {
#getNonRstoxDependencies <- function(packageName = getRstoxFrameworkDefinitions("officialStoxLibraryPackagesAll"), dependencies = NA, repos = NULL, sort = FALSE) {
    # Get non-Rstox dependencies:
    nonRstoxDependencies <- lapply(
        packageName, 
        getDependencies, 
        repos = Rstox.repos, 
        dependencies = dependencies, 
        excludeStartingWith = "Rstox", 
        recursive = FALSE, 
        sort = sort
    )
    # The order of the pakcages is defined in getRstoxFrameworkDefinitions("officialStoxLibraryPackagesAll"), and is kept if sort = FALSE:
    nonRstoxDependencies <- unique(unlist(nonRstoxDependencies))
    
    # Get also dependencies of dependencies:
    allNonRstoxDependencies <- getDependencies(
        packageName = nonRstoxDependencies, 
        repos = nonRstox.repos, 
        dependencies = dependencies, 
        append = TRUE, 
        sort = sort
    )
    
    return(allNonRstoxDependencies)
}
#' 
#' @export
#' @rdname getNonRstoxDependencies
#' 
downloadNonRstoxDependencies <- function(
    packageName = c("RstoxFramework","RstoxBase", "RstoxData"), 
    destdir = NA, 
    dependencies = NA, 
    Rstox.repos = NULL, 
    download.repos = "https://cloud.r-project.org", 
    sort = FALSE, 
    platform = NA, 
    twoDigitRVersion = NA, 
    ...
) {
    
    # Get the dependencies:
    nonRstoxDependencies <- getNonRstoxDependencies(
        packageName = packageName, 
        dependencies = dependencies, 
        Rstox.repos = Rstox.repos, 
        nonRstox.repos = download.repos, 
        sort = sort
    )
    
    # Download the files to the specified directory (or to the tempdir() if not specified):
    if(length(destdir) && is.na(destdir)) {
        destdir <- tempdir()
    }
    
    # using the path to the releavnt binaries:
    binaries <- getPackageBinaryURL(
        packageName = nonRstoxDependencies, 
        repos = download.repos, 
        platform = platform, 
        twoDigitRVersion = twoDigitRVersion
    )
    
    # Download:
    binaryLocalFiles <- file.path(destdir, basename(binaries))
    system.time(mapply(download.file, binaries, destfile = binaryLocalFiles, ...))
    
    return(binaryLocalFiles)
}


installNonRstoxDependencies <- function(
    destdir = NA, 
    dependencies = NA, 
    Rstox.repos = NULL, 
    download.repos = "https://cloud.r-project.org", 
    sort = FALSE, 
    platform = NA, 
    twoDigitRVersion = NA, 
    ...
) {
    
    # Get the dependencies:
    binaryLocalFiles <- downloadNonRstoxDependencies(
        destdir = destdir, 
        dependencies = dependencies, 
        Rstox.repos = Rstox.repos, 
        download.repos = download.repos, 
        sort = sort, 
        platform = platform, 
        twoDigitRVersion = twoDigitRVersion, 
        ...
    )
    
    # For a clean install remove the packages first:
    packagesToRemove <- getOnlyPackageName(basename(binaryLocalFiles))
    removeExistingPackages(packagesToRemove)
    
    # Then install:
    lapply(rev(binaryLocalFiles), install.packages, repos = NULL)
}


installOfficialRstoxPackages <- function(
    StoXGUIVersion, 
    officialRstoxPackageVersionsFile, 
    destdir = NA, 
    include.optional = FALSE, 
    Rstox.repos = "https://stoxproject.github.io/repo", 
    platform = NA, 
    twoDigitRVersion = NA, 
    ...
) {
    
    # Get the official versions defined by the officialRstoxPackageVersionsFile for the particular StoXGUIVersion:
    officialRstoxPackageNameAndVersion <- getOfficialRstoxPackageVersion(
        StoXGUIVersion = StoXGUIVersion, 
        officialRstoxPackageVersionsFile = officialRstoxPackageVersionsFile,
        packageName = c("RstoxFramework","RstoxBase", "RstoxData"), 
        include.optional = include.optional, 
        toJSON = FALSE
    )
    officialRstoxPackageName <- getOnlyPackageName(officialRstoxPackageNameAndVersion)
    officialRstoxPackageVersionList <- extractPackageNameAsNames(officialRstoxPackageNameAndVersion)
    
    # Download and then install the official Rstox package versions from the StoXProject repo:
    binaries <- getPackageBinaryURL(
        packageName = officialRstoxPackageName, 
        version = officialRstoxPackageVersionList, 
        repos = Rstox.repos, 
        platform = platform, 
        twoDigitRVersion = twoDigitRVersion
    )
    if(length(destdir) && is.na(destdir)) {
        destdir <- tempdir()
    }
    binaryLocalFiles <- file.path(destdir, basename(binaries))
    system.time(mapply(download.file, binaries, destfile = binaryLocalFiles, ...))
    
    # For a clean install remove the packages first:
    packagesToRemove <- getOnlyPackageName(basename(binaryLocalFiles))
    removeExistingPackages(packagesToRemove)
    
    # Then install:
    lapply(rev(binaryLocalFiles), install.packages, repos = NULL)
    
    
    return(binaryLocalFiles)
}

removeExistingPackages <- function(pkgs) {
    installed <- installed.packages()[, "Package"]
    packagesToRemove <- intersect(pkgs, installed)
    lapply(pkgs, remove.packages)
}
#' Function to download all official Rstox packages and their CRAN dependencies
#' 
#' @inheritParams getNonRstoxDependencies
#' @param StoXGUIVersion The version of the StoX GUI defining the combination of official Rstox package versions.
#' @param officialRstoxPackageVersionsFile The path to the file holding the link between StoX GUI version and Rstox package versions.
#' @param platform The platfor to downoad binaries for, defaulted to the current platform, but with the option to download for other platforms (possible values are "windows" and "macosx").
#' @param twoDigitRVersion The two digit R version to downoad binaries for, defaulted to the current version, but with the option to download for other versions such as 3.6.
#' 
#' @export
#'
installOfficialRstoxPackagesWithDependencies <- function(
    StoXGUIVersion, 
    officialRstoxPackageVersionsFile, 
    destdir = NA, 
    Rstox.repos = "https://stoxproject.github.io/repo", 
    download.repos = "https://cloud.r-project.org", 
    dependencies = NA, 
    sort = FALSE, 
    platform = NA, 
    twoDigitRVersion = NA, 
    ...
) {
    
    # Install non-Rstox dependencies first:
    installNonRstoxDependencies(
        destdir = destdir, 
        dependencies = dependencies, 
        Rstox.repos = Rstox.repos, 
        download.repos = download.repos, 
        sort = FALSE, 
        platform = platform, 
        twoDigitRVersion = twoDigitRVersion, 
        ...
    )
    
    # Then install the officical Rstox pakcage versions:
    installOfficialRstoxPackages(
        StoXGUIVersion = StoXGUIVersion, 
        officialRstoxPackageVersionsFile = officialRstoxPackageVersionsFile, 
        include.optional = FALSE, 
        download.repos = download.repos, 
        platform = platform, 
        twoDigitRVersion = twoDigitRVersion, 
        ...
    )
}


getOnlyPackageName <- function(packageNameAndVersionString) {
    sub("\\_.*", "", packageNameAndVersionString)
}
getOnlyPackageVersion <- function(packageNameAndVersionString) {
    sub('.+_(.+)', '\\1', packageNameAndVersionString)
}


#' Function to read the OfficialRstoxFrameworkVersions.txt file for the given StoX GUI version
#' 
#' @inheritParams getNonRstoxDependencies
#' @inheritParams installOfficialRstoxPackagesWithDependencies
#' @param packageName The packages considered official Rstox pakcages, "RstoxFramework","RstoxBase" and "RstoxData".
#' @param include.optional Logical: If TRUE inclcude optional packages. Currently not used.
#' @param toJSON Logical: If TRUE output a JSON string.
#' 
#' @export
#'
getOfficialRstoxPackageVersion <- function(
    StoXGUIVersion, 
    officialRstoxPackageVersionsFile, 
    packageName = c("RstoxFramework","RstoxBase", "RstoxData"), 
    include.optional = FALSE, 
    toJSON = FALSE
) {
    
    # Read the officialRstoxPackageVersionsFile:
    official <- tryCatch(
        read.table(
            officialRstoxPackageVersionsFile, 
            header = TRUE, 
            stringsAsFactors = FALSE
        ), 
        error = function(e) {
            NULL
        }
    )
    
    if(length(official)) {
        # Get rows of RstoxFrameworkVersions within the minimum and maximum StoXGUI version:
        
        official <- subset(official, StoX == StoXGUIVersion)
        
        # Split the Dependencies:
        dependencies <- strsplit(official$Dependencies, "[,]")[[1]]
        dependencies <- extractPackageNameAsNames(dependencies)
        packageVersionList <- c(list(RstoxFramework = official$RstoxFramework), dependencies)
        
        if(!include.optional) {
            packageVersionList <- packageVersionList[intersect(packageName, names(packageVersionList))]
        }
        
        packageVersions <- getPackageNameAndVersionString(packageVersionList)
        
        if(toJSON) {
            packageVersions <- vector2json(packageVersions)
        }
        
        return(packageVersions)
    }
    else {
        return(NULL)
    }
}
# Small function to parse the string defining officical Rstox-package versions (for each RstoxFramwork):
extractPackageNameAsNames <- function(x) {
    x <- strsplit(x, "[_]")
    x <- structure(lapply(x, "[", 2), names = sapply(x, "[", 1))
    return(x)
}

getPackageNameAndVersionString <- function(x) {
    paste(names(x), x, sep = "_")
}





# Function to read the description file of an RstoxPackage
getDependencies <- function(packageName, repos = "https://cloud.r-project.org", dependencies = NA, excludeStartingWith = NULL, recursive = TRUE, append = FALSE, sort = TRUE) {
    
    # Get the dependencies of the Rstox packages:
    if(identical(NA, dependencies)) {
        dependencies <- c("Depends", "Imports", "LinkingTo")
    }
    
    # Read the available packages:
    allAvail <- getAvailablePackages(repos = repos)
    
    # Get the dependent packages
    dependentPackages <- extractDependencies(
        packageName = packageName, 
        allAvail = allAvail, 
        dependencies = dependencies, 
        recursive = recursive
    )
    # Remove the intitial packageName:
    if(!append) {
        dependentPackages <- setdiff(dependentPackages, packageName)
    }
    
    # Remove other starting patterns:
    for(pattern in excludeStartingWith) {
        dependentPackages <- subset(dependentPackages, !startsWith(dependentPackages, pattern))
    }
    
    dependentPackages <- unique(dependentPackages)
    if(sort) {
        dependentPackages <- sort(dependentPackages)
    }
    
    return(dependentPackages)
}


extractDependencies <- function(packageName, allAvail, dependencies, recursive = TRUE) {
    # Extract the dependencies of the packageName:
    onlyDependencies <- subset(allAvail, Package %in% packageName)[dependencies]
    if(!nrow(onlyDependencies)) {
        warning("Package ", paste(packageName, collapse = ", "), " not present in the repos.")
    }
    
    # Paste together to prepare for parsing:
    onlyDependencies <- apply(onlyDependencies, 1, paste, collapse = ", ")
    
    # Remove annoying line spaces:
    onlyDependencies <- gsub("\n", " ", onlyDependencies)
    # Split by comma:
    onlyDependencies <- strsplit(onlyDependencies, ", |,")
    # Remove R and NA:
    onlyDependencies <- lapply(onlyDependencies, function(x) subset(x, !startsWith(x, "R ") & !startsWith(x, "R(") & x != "NA"))
    
    # Get only package names:
    onlyDependencies <- unique(unlist(lapply(onlyDependencies, function(x) gsub(" .*$", "", x))))
    
    # Exlude base packages:
    onlyDependencies <- exlcudeBasePackages(onlyDependencies)
    
    if(recursive) {
        if(length(onlyDependencies)) {
            return(c(
                packageName, 
                extractDependencies(
                    onlyDependencies, 
                    allAvail = allAvail, 
                    dependencies = dependencies
                )
            ))
        }
        else {
            return(packageName)
        }
    }
    else {
        return(onlyDependencies)
    }
}

getAvailablePackages <- function(packageName = NULL, repos = "https://cloud.r-project.org", platform = NA, twoDigitRVersion = NA) {
    
    # Get the available packages in the repos:
    if(length(repos)) {
        URL <- buildReposContrib(
            repos = repos, 
            platform = platform, 
            twoDigitRVersion = twoDigitRVersion
        )
        #avail <- utils::available.packages(utils::contrib.url(repos, type = type))
        avail <- utils::available.packages(URL)
    }
    # Or locally:
    else {
        avail <- installed.packages()
    }
    
    # For convenience convert to data.frame:
    avail <- as.data.frame(avail, stringsAsFactors = FALSE)
    
    # Check for packageName not in the repos:
    notPresent <- setdiff(packageName, avail$Package)
    if(length(notPresent)) {
        warning("The following packages were not found in the rpeos ", repos, ": ", paste(notPresent, collapse = ", "))
    }
    
    # Extract the packages:
    if(length(packageName)) {
        avail <- subset(avail, Package %in% packageName)
    }
    
    return(avail)
}

exlcudeBasePackages <- function(x) {
    setdiff(x, rownames(installed.packages(priority="base")))
}

# Funcction to get platform code used in the path to the package binarry:
getPlatformCode <- function(platform = NA, twoDigitRVersion = NA) {
    # Declare the output:
    platformCode <- getPlatform(platform)
    
    # Append "el-capitan" if R_3.6 on mac:
    if (platformCode == "macosx") {
        if(getTwoDigitRVersion(twoDigitRVersion) == "3.6") {
            platformCode <- file.path(platformCode, "el-capitan")
        }
    }
    
    return(platformCode)
}

getBinaryType <- function(platform = NA, twoDigitRVersion = NA) {
    # Declare the output:
    binaryType <- paste(substr(getPlatform(platform), 1, 3), "binary", sep = ".")
    
    # Append "el-capitan" if R_3.6 on mac:
    if (platform == "macosx") {
        if(getTwoDigitRVersion(twoDigitRVersion) == "3.6") {
            binaryType <- paste(binaryType, "el-capitan", sep = ".")
        }
    }
    
    return(binaryType)
}





getPlatform <- function(platform = NA) {
    if(is.na(platform)) {
        if (.Platform$OS.type == "windows") {
            platform <- "windows"
        }
        else if (Sys.info()["sysname"] == "Darwin") {
            platform <- "macosx"
        } 
        else {
            stop("Only Windows and MacOS are currently supported.")
        }
    }
    
    return(platform)
}


getTwoDigitRVersion <- function(twoDigitRVersion = NA, coerceToCRANContrib = TRUE) {
    if(is.na(twoDigitRVersion)) {
        RMajor <- R.Version()$major
        RMinor <- gsub("(.+?)([.].*)", "\\1", R.Version()$minor)
        twoDigitRVersion <- paste(RMajor, RMinor, sep = ".")
    }
    
    # Conver e.g. 3.7 to 3.6 as per the listing on 
    #   url <- 'https://cloud.r-project.org/bin/windows/contrib/'
    #   filenames = getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE)
    #   strsplit(filenames, "\n", fixed = TRUE)[[1]]
    # and on
    #   url <- 'https://cloud.r-project.org/bin/macosx/el-capitan/contrib/'
    #   filenames = getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE)
    #   strsplit(filenames, "\n", fixed = TRUE)[[1]]
    # and
    #   url <- 'https://cloud.r-project.org/bin/macosx/contrib/'
    #   filenames = getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE)
    #   strsplit(filenames, "\n", fixed = TRUE)[[1]]
    
    if(coerceToCRANContrib) {
        if(twoDigitRVersion < "3.6") {
            stop("R 3.6 is the minimum required R version for StoX")
        }
        else if(twoDigitRVersion >= "3.6" && twoDigitRVersion < "4.0") {
            twoDigitRVersion <- "3.6"
        }
        if(twoDigitRVersion > "4.1") {
            stop("StoX does not support R > 4.1")
        }
    }
    
    return(twoDigitRVersion)
}



# Function to build one installation line with install.packages():
getPackageBinaryURL <- function(packageName, version = NULL, repos = "https://cloud.r-project.org", platform = NA, twoDigitRVersion = NA) {
    # https://stoxproject.github.io/repo/bin/windows/contrib/4.0/RstoxData_1.0.9.zip
    # windows:
    # https://stoxproject.github.io/repo/bin/windows/contrib/<R-ver>/<package_name>_<ver>.zip
    # macos-R3.6:
    # https://stoxproject.github.io/repo/bin/macosx/el-capitan/contrib/3.6/<package_name>_<ver>.tgz
    # macos-R4.0:
    # https://stoxproject.github.io/repo/bin/macosx/contrib/4.0/<package_name>_<ver>.tgz
    
    # Get the available packages in the repos:
    avail <- getAvailablePackages(
        packageName = packageName, 
        repos = repos, 
        #type = getBinaryType(
        platform = platform, 
        twoDigitRVersion = twoDigitRVersion
        #)
    ) 
    
    
    # Overwrite versions if given:
    for(ind in seq_along(version)) {
        avail[avail$Package == names(version)[ind], ]$Version <- version[[ind]]
    }
    
    # Get the R version as two digit string:
    twoDigitRVersion <- getTwoDigitRVersion(twoDigitRVersion)
    
    # Get the file extention:
    fileExt <- getBinaryFileExt(platform)
    
    # Build the path to the package binary:
    pathSansExt <- paste(
        buildReposContrib(
            repos = repos, 
            platform = platform, 
            twoDigitRVersion = twoDigitRVersion
        ), 
        paste(
            avail$Package, 
            avail$Version, 
            sep = "_"
        ), 
        sep = "/"
    )
    
    # Add file extension:
    path <- paste(pathSansExt, fileExt, sep = ".")
    
    return(path)
}

getBinaryFileExt <- function(platform = NA) {
    platform <- getPlatform(platform)
    
    if (platform == "windows") {
        fileExt <- "zip"
    }
    else if (platform == "macosx") {
        fileExt <- "tgz"
    }
    
    return(fileExt)
}

buildReposContrib <- function(repos = "https://cloud.r-project.org", platform = NA, twoDigitRVersion = NA) {
    
    # Get the two digit R version, as it is used several places below:
    twoDigitRVersion <- getTwoDigitRVersion(twoDigitRVersion)
    
    reposContrib <- paste(
        repos, 
        "bin", 
        getPlatformCode(
            platform = platform, 
            twoDigitRVersion = twoDigitRVersion
        ), 
        "contrib", 
        twoDigitRVersion, 
        sep = "/"
    )
    
    return(reposContrib)
}


vector2json <- function(x) {
    paste0("[", paste(sapply(x, deparse), collapse = ","), "]")
}

