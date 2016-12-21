-   [Temperature Logger Calibration](#temperature-logger-calibration)
    -   [External calibration data](#external-calibration-data)
    -   [Read in internal calibration data](#read-in-internal-calibration-data)
    -   [run the calibration](#run-the-calibration)
    -   [A full analysis script](#a-full-analysis-script)

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

The function `calibration` runs the full calibration method. Here, as we are running an example, we only use 5 simulations to back calculate the logger error. Normally a large number like 99 or 999 should be used, but this can take up to a day to run.

``` r
cal <- calibration(internalCal, externalCalMod, n = 5)
```

    ## Setting up calibration regressions.

    ## multiple caliibration loggers found in experiment. Only the first used.

    ## fitting 640248: 1 of 15
    ## fitting 642016: 2 of 15
    ## fitting 642027: 3 of 15
    ## fitting 642031: 4 of 15
    ## fitting 642042: 5 of 15
    ## fitting 642269: 6 of 15
    ## fitting 642284: 7 of 15
    ## fitting 642302: 8 of 15
    ## fitting 642303: 9 of 15
    ## fitting 642315: 10 of 15
    ## fitting 642326: 11 of 15
    ## fitting 642337: 12 of 15
    ## fitting 642362: 13 of 15
    ## fitting 642387: 14 of 15
    ## fitting 643055: 15 of 15

Once the calibration simulations have completed, the coefficients from the calibrations are extracted and tabled (e.g. for import into a logger database with calibration functionality).

``` r
coefs <- getCoefs(cal)
tabCoefs <- tableCoefs(coefs, cal = internalCal)
tabCoefs
```

    ##       Make   Model     SN Unit Start_date Cal_Eq_type         Cal_coef1
    ## 1  Tinytag TG-4100 640248   °C 2014-02-10           2 0.254287076965235
    ## 2  Tinytag TG-4100 642016   °C 2014-02-10           2 0.265863117306381
    ## 3  Tinytag TG-4100 642027   °C 2014-02-10           2 0.241299409885693
    ## 4  Tinytag TG-4100 642031   °C 2014-02-10           2 0.226996462005732
    ## 5  Tinytag TG-4100 642042   °C 2014-02-10           2 0.252688125684804
    ## 6  Tinytag TG-4100 642269   °C 2014-02-10           2 0.210640194964071
    ## 7  Tinytag TG-4100 642284   °C 2014-02-10           2 0.137561431846689
    ## 8  Tinytag TG-4100 642302   °C 2014-02-10           2 0.324964105650781
    ## 9  Tinytag TG-4100 642303   °C 2014-02-10           2 0.312552453816963
    ## 10 Tinytag TG-4100 642315   °C 2014-02-10           2 0.196668508055947
    ## 11 Tinytag TG-4100 642326   °C 2014-02-10           2 0.280664818826085
    ## 12 Tinytag TG-4100 642337   °C 2014-02-10           2   0.2440362646843
    ## 13 Tinytag TG-4100 642362   °C 2014-02-10           2 0.211594433370882
    ## 14 Tinytag TG-4100 642387   °C 2014-02-10           2 0.221153391323963
    ## 15 Tinytag TG-4100 643055   °C 2014-02-10           2 0.187347154209724
    ##            Cal_coef2            Cal_coef3 Cal_coef4 Cal_coef5 Cal_coef6
    ## 1  0.989112536669993 0.000175514215292786                              
    ## 2   0.98824782460934 0.000164689429651518                              
    ## 3  0.990027569832121 0.000163281990990505                              
    ## 4  0.990413738414132 0.000153213057335697                              
    ## 5   0.98989835573959 0.000192271142488412                              
    ## 6  0.989712240472111 0.000156994793073062                              
    ## 7  0.990640938856286 0.000145657666795488                              
    ## 8   0.98813084262372 0.000178029427417644                              
    ## 9  0.989058932080252  0.00014468750582636                              
    ## 10 0.991055950292287 0.000167240088936821                              
    ## 11 0.987934737244854 0.000169195722228134                              
    ## 12 0.989235026785506 0.000170186892557371                              
    ## 13 0.989961404361273 0.000146495209220375                              
    ## 14 0.989815070373874 0.000170330418923062                              
    ## 15 0.990406561838821 0.000176692956848697                              
    ##    Cal_coef7 Cal_coef8 Cal_coef9 Cal_coef10 SE_Eq_type
    ## 1                                                    4
    ## 2                                                    4
    ## 3                                                    4
    ## 4                                                    4
    ## 5                                                    4
    ## 6                                                    4
    ## 7                                                    4
    ## 8                                                    4
    ## 9                                                    4
    ## 10                                                   4
    ## 11                                                   4
    ## 12                                                   4
    ## 13                                                   4
    ## 14                                                   4
    ## 15                                                   4
    ##                 SE_coef1              SE_coef2              SE_coef3
    ## 1     0.0052936364761588  0.000135270596789807  -5.4104304852549e-05
    ## 2    0.00540860296114349 -0.000452469531358579  6.77274635553011e-05
    ## 3    0.00639064084305104 -0.000756782313215505  0.000113347425015023
    ## 4    0.00540004861662462  0.000102631672349224  -3.3741796041862e-05
    ## 5    0.00549614464843149  0.000158107182621589 -2.43637063214406e-05
    ## 6    0.00571256776243268  0.000108004863726642 -4.75962192507944e-05
    ## 7    0.00643314176950677 -0.000761486538259179  0.000101114885629898
    ## 8  -0.000492189066546556   0.00179803364170091 -0.000167605806142069
    ## 9   0.000532812519866144   0.00168187499613931 -0.000178990174293142
    ## 10   0.00685357416016966  -0.00106726426109782  0.000136706367796065
    ## 11   0.00500299979992619 -0.000160045880941697  3.39440298946299e-05
    ## 12   0.00562882145139061  3.53164111989196e-05 -2.62323998814829e-06
    ## 13   0.00613176092320401 -0.000387853185158775  4.25277734075165e-05
    ## 14   0.00420998707192664  9.09820652057902e-05 -1.32986787047258e-05
    ## 15   0.00632650550820723 -0.000201947850172702  1.81161192657693e-05
    ##                 SE_coef4              SE_coef5 SE_coef6 SE_coef7 SE_coef8
    ## 1   4.03793237122898e-06 -8.17269092953887e-08                           
    ## 2  -3.20728530248173e-06  4.92492204642145e-08                           
    ## 3  -5.90849261047134e-06  1.00238708404209e-07                           
    ## 4    2.1322972717788e-06 -3.75300152527614e-08                           
    ## 5   1.33980127315555e-06 -2.44235702475527e-08                           
    ## 6   3.03634389553818e-06 -5.22322796598406e-08                           
    ## 7  -5.14207333131411e-06  8.93341634331487e-08                           
    ## 8   6.04353965615716e-06 -7.35081043719505e-08                           
    ## 9   7.11829763335747e-06 -9.36943137321745e-08                           
    ## 10 -6.39514077122806e-06  9.91390066035046e-08                           
    ## 11 -2.12794110785402e-06    4.129437175207e-08                           
    ## 12 -2.60180209978999e-07  1.05480440908682e-08                           
    ## 13  -1.7654836865286e-06  2.41608932949335e-08                           
    ## 14  6.19867127840693e-07 -6.80357523134499e-09                           
    ## 15  -7.6948077062882e-07  1.18795101798483e-08                           
    ##    SE_coef9 SE_coef10
    ## 1                    
    ## 2                    
    ## 3                    
    ## 4                    
    ## 5                    
    ## 6                    
    ## 7                    
    ## 8                    
    ## 9                    
    ## 10                   
    ## 11                   
    ## 12                   
    ## 13                   
    ## 14                   
    ## 15

A full analysis script
----------------------

``` r
library(loggercal)

externalCalDir <- "data-raw/External Calibrations/"
internalCalDir <- "data-raw/100214/"

# external calibration -------------------------

# read in external calibrations
ukas <- readExternalCal(externalCalDir)

# fit a model to the external calibration data
externalCalMod <- lapply(ukas $ data, function(x) lm(cal ~ poly(control, 2), data = x[-1,]))

# internal calibration -------------------------

# read in internal calibration experiment
internalCal <- readInternalCal(internalCalDir)

# trim off ends
internalCal$startStopSec <- c(100,9900) * 60

# plot to check
plot(internalCal)

# perform a calibration ------------

cal <- calibration(internalCal, externalCalMod, n = 5)

# convert coefficients to non orthogonal polynomials
coefs <- getCoefs(cal)
tabCoefs <- tableCoefs(coefs, cal = internalCal)

write.csv(file = file.path(file.path(internalCalDir), "coefficients.csv"), tabCoefs, row.names = FALSE)

# Done -----------------
```
