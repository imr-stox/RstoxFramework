# RstoxFramework v3.0.30 (2021-06-16)
* Added parallel bootstrapping.
* Added BaselineSeedTable in Bootstrap().
* Added testing of full projects (using parallel testthat). Completed documentation. Added readModelData().
* Changed name of output text files to not contain the process name.
* Added runProjects().
* Added warning if Rstox packcages to be installed are not built on the installed R version.
* Fixed bug where NAs were not preserved in project.json. Fixed bug in createProject().
* Added support for mixed OutputDepth (particularly useful in Bootstrap).
* Reintroduced possible values in filter for all-unique variables such as Station and EDSU. Allowing drop down list of only one * unique element.
* Added encoding = UTF-8 in readStoxOutputFile().
* project.json now uses ISO8601 for time..

# RstoxFramework v3.0.0 (2021-02-11)
* Final version for the release of StoX 3.0.0.

# RstoxFramework v1.2.40 (2021-01-30)
* Added %notequal%.

# RstoxFramework v1.2.39 (2021-01-28)
* Fixed bug with possible values of length 1 being dropped.
* Renamed official to certified in the project.json. 
* Removing empty function arguments in runProcess(), in order to use the error "argument ___ is missing, with no default".
* Added warning for non-official project.json

# RstoxFramework v1.2.38 (2021-01-21)
* Fixed bug where Hauls could not be removed from assignment.
* Added support for numeric, integer and logical vectors in addition to the already supported character vector in data types.

# RstoxFramework v1.2.33 (2021-01-12)
* Renamed GUI to Application in project.json.

# RstoxFramework v1.2.33 (2020-12-22)
* Added backwards compatibility (for new projects).
* Increased timeout when downloading packages.
* Replaced geojsonio by geojsonsf to reduce dependencies.

# RstoxFramework v1.2.25 (2020-10-11)
* Introduced project.JSON.

# RstoxFramework v1.2.20 (2020-09-07)
* Added UseOutputData in Bootstrap().

# RstoxFramework v1.2.18 (2020-08-28)
* Removed unit in variable and parameter names

# RstoxFramework v1.2.14 (2020-07-19)
* Added Bootstrap
