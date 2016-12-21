Temperature Logger Calibration
==============================

Quick example
-------------

The first step is to load the loggercal package

``` r
devtools::install_github("faskally/loggercal")
library(loggercal)
```

Next we look for the external calibration files from a UKAS calibration. These are normally to be found in a folder called "External Calibrations", but if you are working from within the package directory (as this readme file is) the you will have to look in "data-raw/External Calibration".

``` r
externalCalDir <- "data-raw/External Calibrations/"
externalCalFiles <- list.files(externalCalDir, full.names = TRUE)
```

In this case we find two external calibrations. This is read in using the `loggercal` function `readExternalCal`

``` r
ukas <- readExternalCal(externalCalFiles)
str(ukas)
```

    ## List of 2
    ##  $ SN  : chr [1:2] "319151" "613892"
    ##  $ data:List of 2
    ##   ..$ 319151:'data.frame':   8 obs. of  5 variables:
    ##   .. ..$ control    : num [1:8] 19.996 25.013 29.993 5.025 -0.002 ...
    ##   .. ..$ cal        : num [1:8] 19.946 24.975 29.954 4.929 -0.118 ...
    ##   .. ..$ Error      : num [1:8] -0.05 -0.038 -0.039 -0.096 -0.116 -0.081 -0.062 -0.051
    ##   .. ..$ Time       : chr [1:8] " 00:30:00" " 00:30:00" " 00:30:00" " 00:30:00" ...
    ##   .. ..$ Uncertainty: num [1:8] 0.025 0.025 0.025 0.025 0.025 0.025 0.025 0.025
    ##   ..$ 613892:'data.frame':   8 obs. of  5 variables:
    ##   .. ..$ control    : num [1:8] 19.996 25.013 29.993 5.025 -0.002 ...
    ##   .. ..$ cal        : num [1:8] 19.946 24.975 29.954 4.929 -0.118 ...
    ##   .. ..$ Error      : num [1:8] -0.05 -0.038 -0.039 -0.096 -0.116 -0.081 -0.062 -0.051
    ##   .. ..$ Time       : chr [1:8] " 00:30:00" " 00:30:00" " 00:30:00" " 00:30:00" ...
    ##   .. ..$ Uncertainty: num [1:8] 0.025 0.025 0.025 0.025 0.025 0.025 0.025 0.025
    ##  - attr(*, "class")= chr "ExternalCal"

we now fit a model to the external calibration data

``` r
externalCalMod <- lapply(ukas $ data, function(x) lm(cal ~ poly(control, 2), data = x[-1,]))
```
