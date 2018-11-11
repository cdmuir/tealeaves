tealeaves
=======

[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)

<!---
[![Build Status](https://travis-ci.org/ropensci/taxa.svg?branch=master)](https://travis-ci.org/cdmuir/tealeaves)
[![codecov](https://codecov.io/gh/ropensci/taxa/branch/master/graph/badge.svg)](https://codecov.io/gh/cdmuir/tealeaves)
[![rstudio mirror downloads](http://cranlogs.r-pkg.org/badges/tealeaves)](https://github.com/metacran/cranlogs.app)
[![cran version](http://www.r-pkg.org/badges/version/tealeaves)](https://cran.r-project.org/package=tealeaves)
-->

## Solve for leaf temperature using energy balance

## Description

`tealeaves` is a lightweight R package to model leaf temperature using leaf energy balance. It uses the R packge `citation(package = 'units')` to ensure that parameters are properly specified and transformed before calculations. It allows separate lower and upper surface conductances to heat and water vapour, so sensible and latent heat loss are calculated for each surface separately. It's straightforward to model leaf temperature over environmental gradients such as light, air temperature, humidity, and wind, or trait gradients such as leaf size or stomatal conductance. 

## Get tealeaves

<!--- From CRAN

```r
install.packages("tealeaves")
```

or from -->GitHub

```r
install.packages("devtools")
devtools::install_github("cdmuir/tealeaves")
```

And load tealeaves

```r
library("tealeaves")
```

## Code of Conduct

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md).
By participating in this project you agree to abide by its terms.