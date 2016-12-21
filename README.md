[![Build Status](https://travis-ci.org/Faskally/loggercal.svg?branch=master)](https://travis-ci.org/Faskally/loggercal)

Temperature Logger Calibration
==============================

The first step is to load the loggercal package

``` r
devtools::install_github("faskally/loggercal")
library(loggercal)
```

The first thing to do is to set the location of the external and internal calibrations we are looking to compare.

``` r
externalCalDir <- "data-raw/External Calibrations/"
internalCalDir <- "data-raw/100214/"
```

External calibration data
-------------------------

Next we look for the external calibration files from a UKAS calibration. These should all be in one directory. In this case we find two external calibrations. These are read in using the `loggercal` function `readExternalCal`

``` r
ukas <- readExternalCal(externalCalDir)
```

    ## Calibration file summary
    ## 
    ##   Current working directory:
    ##  D:/projects/faskally/loggercal
    ##   Files read:
    ##  data-raw/External Calibrations/UKASCalibration_319151.csv
    ##  data-raw/External Calibrations/UKASCalibration_613892.csv

``` r
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

Read in internal calibration data
---------------------------------

``` r
internalCal <- readInternalCal(internalCalDir)
```

    ## Reading calibration experiment
    ## 
    ##   Current working directory:
    ##  D:/projects/faskally/loggercal
    ##   Files:
    ##  data-raw/100214//FullCalibration_100214.csv

``` r
internalCal
```

    ## 
    ## Internal calibration experiment:
    ##        SN                   Type Description    Property
    ## 1  640248                TG-4100     Tinytag Temperature
    ## 2  642016                TG-4100     Tinytag Temperature
    ## 3  642027                TG-4100     Tinytag Temperature
    ## 4  642031                TG-4100     Tinytag Temperature
    ## 5  642042                TG-4100     Tinytag Temperature
    ## 6  642269                TG-4100     Tinytag Temperature
    ## 7  642284                TG-4100     Tinytag Temperature
    ## 8  642302                TG-4100     Tinytag Temperature
    ## 9  642303                TG-4100     Tinytag Temperature
    ## 10 642315                TG-4100     Tinytag Temperature
    ## 11 642326                TG-4100     Tinytag Temperature
    ## 12 642337                TG-4100     Tinytag Temperature
    ## 13 642362                TG-4100     Tinytag Temperature
    ## 14 642387                TG-4100     Tinytag Temperature
    ## 15 643055                TG-4100     Tinytag Temperature
    ## 16 319151 Tinytag PLUS2 -40/85°C     Tinytag Temperature
    ## 17 613892               TGP-4017     Tinytag Temperature

Now it should be checked that the external calibration loggers were included in the experiment

``` r
ukas
```

    ## 
    ## External calibration serial numbers:
    ## [1] "319151" "613892"

And here, the last two loggers in the internal calibration are the externally calibrated ones. The next step is to define the start and stop of the calibration. This can be done manually (note that the value `startStopSec` is in seconds, while the plot axis is in minutes, so you need to multiply by 60 to set limits in secs):

``` r
plot(internalCal)
```

![](ReadMe_files/figure-markdown_github/trim_manual-1.png)

``` r
internalCal$startStopSec <- c(100,9900) * 60
plot(internalCal)
```

![](ReadMe_files/figure-markdown_github/trim_manual-2.png)

Or this can be done using a graphical approach, in a handy function.

``` r
internalCal <- findStartSec(internalCal)
internalCal <- findStopSec(internalCal)
plot(internalCal)
```

run the calibration
-------------------

The function `calibration` runs the full calibration methif

``` r
cal <- calibration(internalCal, externalCalMod, n = 5)
```

``` r
coefs <- getCoefs(cal)
tabCoefs <- tableCoefs(coefs, cal = internalCal)
```
