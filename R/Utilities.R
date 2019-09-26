initiateRstox <- function() {

  # NMD and Stox defines different data types (Stox has the more general category "acoustic"):
  NMD_data_sources <- c(acoustic = "echosounder", biotic = "biotic", landing = "landing")
  # The implemented NMD APIs for the NMD_data_sources:
  NMD_API_versions <- list(
        biotic = c(1, 2, 3),
        echosounder = 1,
        reference = c(1, 2),
        landing = NULL
    )

  # The format used by NMD for shapshot time:
  dateTimeNMDAPIFormat <- "%Y-%m-%dT%H.%M.%OSZ"

  # The current API and datasource formats:
  ver <- list(
        API = list(
            biotic = "3",
            echosounder = "1",
            reference = "2",
            landing = NA
        ),
        reference = "2.0",
        biotic = "3.0",
        echosounder = NA,
        landing = NA
    )

  # Define project types:
  project_types <- c("AcousticTrawl", "SweptAreaLength", "SweptAreaTotal")

  # Define the process levels for the presicion estimate:
  processLevels <- c("bootstrap", "bootstrapImpute")

  # Define ECA covariates ******************** THIS SHOULD PROBABLY BE MOVED TO THE RstoxECA package:
  ECACovariates <- data.frame(
        Name = c(
            "temporal",
            "gearfactor",
            "spatial",
            "platformfactor"
        ),
        Processe = c(
            "DefineTemporalLanding",
            "DefineGearLanding",
            "DefineSpatialLanding",
            "DefinePlatformLanding"
        ),
        Description = c(
            "The temporal covariate",
            "The gear covariate given as groups of gear codes",
            "The spatial covariate giving polygons or locations",
            "The platform covariate (vessels)"
        ),
        stringsAsFactors = FALSE
    )

  # Define the dependencies to include in the version names of the automated testing:
  internal_dependencies <- c("Reca")

  # Assign to RstoxEnv and return the definitions:
  Definitions <- list(
        Stox_reading_processes = Stox_reading_processes,
        dateTimeNMDAPIFormat = dateTimeNMDAPIFormat,
        NMD_data_sources = NMD_data_sources,
        ver = ver,
        project_types = project_types,
        processLevels = processLevels,
        ECACovariates = ECACovariates,
        internal_dependencies = internal_dependencies
    )

  # Create the RstoxFrameworkEnv environment, holding definitions on folder structure and all the projects. This environment cna be accesses using RstoxFramework:::RstoxFrameworkEnv:
  utils::globalVariables("RstoxEnv")
  assign("RstoxEnv", new.env(), parent.env(environment()))

  assign("Definitions", Definitions, envir = get("RstoxEnv"))
  assign("Projects", list(), envir = get("RstoxEnv"))

  # Return the definitions:
  Definitions
}

# deg to rad function
deg2rad <- function(deg) {
  deg * pi / 180
}

##################################################
#' PolygonArea
#' 
#' This function calculates the area of the polygon.
#' 
#' @return
#' simple area.
#' 
#' @noRd
#' @seealso 
#' 
polygonArea <- function(x, y = NULL) {
  xy <- xy.coords(x, y)
  xy <- data.frame(xy.x, xy.y)
  area <- 0;
  len <- length(xy$x);
  for (i in seq_len(len - 1)) {
    p1 <- xy[i,]
    p2 <- xy[i + 1,]
    area <- area + deg2rad(p1$x - p2$x)
     * (2 + sin(deg2rad(p1$y))
     + sin(deg2rad(p2$y)))
  }
  r <- 6371000.0 * 0.000539956803;
  abs(area * r ^ 2 / 2.0);
}

