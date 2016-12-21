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
```

we now fit a model to the external calibration data

``` r
externalCalMod <- lapply(ukas $ data, function(x) lm(cal ~ poly(control, 2), data = x[-1,]))
```
