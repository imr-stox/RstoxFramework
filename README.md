# RstoxFramework

![R-CMD-check](https://github.com/StoXProject/RstoxFramework/workflows/R-CMD-check/badge.svg)

## Introduction

The RstoxFramework package is the engine of the stock assesment utility StoX, which is an open source application fo acoustic-trawl and swept-area survey estimation.

The package creates an evironment containing the StoX project(s) in separate environments. Each StoX project consists of a list of data tables holding e.g. biotic and acoustic data, filtered versions of the data, strata system, definitios of primary sampling units, accompanied by a list of specifications of the StoX processes comprising the StoX project. A StoX process is an R function taking as input the project name, the input data and parameters used by the function.

The package replaces the old Java library in StoX versions prior to StoX 3.0.0.

## Installation

1. Install the latest release:
    ```r
    install.packages("RstoxFramework", repos = c("https://stoxproject.github.io/repo", "https://cloud.r-project.org"))
    ```

2. Install the latest version from GitHub:
    ```r
    devtools::install_github("https://github.com/StoXProject/RstoxFramework")
    ```

## License

LGPL-3 © Norwegian Institute of Marine research (IMR) ([homepage](https://www.hi.no/en)).

The development of RstoxFramework package is mainly supported by IMR's [REDUS](http://www.redus.no) and SEA2DATA projects.

RstoxFramework is part of the bigger [StoX ecosystem](https://stoxproject.github.io).
