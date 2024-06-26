# 2024_had.27.7a_assessment
2024 - Haddock (Melanogrammus aeglefinus) in Division 7.a (Irish Sea) - WGCSE(ADGCS)

================

This repository recreates the stock assessment for Haddock(*Melanogrammus aeglefinus*)
in Division 7.a (Irish Sea) in `R` from WGCSE 2024.

## R packages

The following R packages from CRAN are required to run the assessment:

``` r
icesTAF
icesAdvice
ggplot2
dplyr
reshape
tidyr
Hmisc
ASAPplots
```

They can be installed with:

``` r
### list with required packages
req_pkgs <- c("icesTAF", "icesAdvice", "ggplot2", "dplyr", "reshape", 
              "tidyr","Hmisc")
### install packages which are not installed on the system
install.packages(req_pkgs[!req_pkgs %in% installed.packages()])
devtools::install_github("cmlegault/ASAPplots", build_vignettes = TRUE)
```

Furthermore, the following FLR (<https://www.flr-project.org>,
<https://github.com/flr>) packages are required:

``` r
FLCore
FLAssess
FLash
```

For exact reproducibility, it is recommended to use exactly the same
package version as used in the assessment. These FLR package are
automatically installed into a local library when running the TAF
assessment (see below) or by calling

``` r
taf.bootstrap()
```

Alternatively, they can be manually installed from GitHub with

``` r
devtools::install_github("flr/FLCore", ref = "7219078")
devtools::install_github("flr/FLAssess", INSTALL_opts = "--no-multiarch",
                         ref = "14c7752")
devtools::install_github("flr/FLash", INSTALL_opts = "--no-multiarch",
                         ref = "487370c")
```

The latest release versions are available from
<http://www.flr-project.org/>:

``` r
install.packages(pkgs =  c("FLCore", "FLAssess", "FLash", "ggplotFL"), 
                 repos = "http://flr-project.org/R")
```

Please note, on Windows `FLAssess` and `FLash` can only be
installed and used in the 64-bit version of `R` (previously they only
worked on 32-bit).

## Running the assessment

The easiest way to run the assessment is to clone or download this
repository and run:

``` r
### load the icesTAF package
library(icesTAF)
### load data and install R packages
taf.bootstrap()
### run all scripts
sourceAll()
```

This code snippet runs the entire data compilation and assessment and
creates the tables and figures presented in the WG report.

## Explore results

To view the results created on the TAF server, browse the [GitHub taf
branch](https://github.com/ices-taf/2024_had.27.7a_assessment/tree/taf).

Assessment results can also be browsed on the [TAF
application](https://taf.ices.dk/app/stock#!/2024/had.27.7a).

