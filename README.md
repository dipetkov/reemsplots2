[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active) [![Travis-CI Build Status](https://travis-ci.org/dipetkov/reemsplots2.svg?branch=master)](https://travis-ci.org/dipetkov/reemsplots2) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/dipetkov/reemsplots2?branch=master&svg=true)](https://ci.appveyor.com/project/dipetkov/reemsplots2)

------------------------------------------------------------------------

[![minimal R version](https://img.shields.io/badge/R%3E%3D-3.3.2-6666ff.svg)](https://cran.r-project.org/) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/reemsplots2)](https://cran.r-project.org/package=reemsplots2) [![packageversion](https://img.shields.io/badge/Package%20version-0.1.0-orange.svg?style=flat-square)](commits/master)

------------------------------------------------------------------------

[![Last-changedate](https://img.shields.io/badge/last%20change-2016--12--23-yellowgreen.svg)](/commits/master)

<!-- README.md is generated from README.Rmd. Please edit that file -->
### reemsplots2: Generate plots to inspect and visualize the results of EEMS

### Installation

``` r
library("devtools")
install_github("dipetkov/reemsplots2")
```

Now, with the function `make_eems_plots`, we can produce a set of several figures to visualize the results of analyzing geo-referenced genetic data with EEMS as well as to evaluate the EEMS model fit.

``` r
library("reemsplots2")
mcmcpath <- system.file("extdata", "EEMS-example", package = "reemsplots2")
plots <- make_eems_plots(mcmcpath, longlat = TRUE)
#> Joining, by = "id"
#> Generate effective migration surface (posterior mean of m rates). See plots$mrates01 and plots$mrates02.
#> Generate effective diversity surface (posterior mean of q rates). See plots$qrates01 and plots$qrates02.
#> Generate average dissimilarities within and between demes. See plots$rdist01, plots$rdist02 and plots$rdist03.
#> Generate posterior probability trace. See plots$pilog01.
names(plots)
#> [1] "mrates01" "mrates02" "qrates01" "qrates02" "rdist01"  "rdist02" 
#> [7] "rdist03"  "pilogl01"
```

``` r
plots$mrates01
```

<img src="README-EEMS-example-default-mrates01-1.png" width="75%" />

``` r
plots$mrates02
```

<img src="README-EEMS-example-default-mrates02-1.png" width="75%" />

``` r
plots$qrates01
```

<img src="README-EEMS-example-default-qrates01-1.png" width="75%" />

``` r
plots$qrates02
```

<img src="README-EEMS-example-default-qrates02-1.png" width="75%" />

``` r
plots$rdist01
```

<img src="README-EEMS-example-default-rdist01-1.png" width="75%" />

``` r
plots$rdist02
```

<img src="README-EEMS-example-default-rdist02-1.png" width="75%" />

``` r
plots$rdist03
```

<img src="README-EEMS-example-default-rdist03-1.png" width="75%" />

``` r
plots$pilogl01
```

<img src="README-EEMS-example-default-pilogl01-1.png" width="75%" />
