tealeaves <img src="hex-sticker/hex-sticker.png" align="right" height="200" width="200"/>
=========================================================================================

[![Project Status: WIP - Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
[![Build
Status](https://travis-ci.com/cdmuir/tealeaves.svg?branch=master)](https://travis-ci.com/cdmuir/tealeaves)

<!---
[![codecov](https://codecov.io/gh/cdmuir/tealeaves/branch/master/graph/badge.svg)](https://codecov.io/gh/cdmuir/tealeaves)
[![rstudio mirror downloads](http://cranlogs.r-pkg.org/badges/tealeaves)](https://github.com/metacran/cranlogs.app)
[![cran version](http://www.r-pkg.org/badges/version/tealeaves)](https://cran.r-project.org/package=tealeaves)
-->
Solve for leaf temperature using energy balance
-----------------------------------------------

Description
-----------

`tealeaves` is a lightweight R package to model leaf temperature using
leaf energy balance. It uses the R package
[units](https://cran.r-project.org/web/packages/units/index.html) to
ensure that parameters are properly specified and transformed before
calculations. It allows separate lower and upper surface conductances to
heat and water vapour, so sensible and latent heat loss are calculated
for each surface separately. It's straightforward to model leaf
temperature over environmental gradients such as light, air temperature,
humidity, and wind, or trait gradients such as leaf size or stomatal
conductance.

Get `tealeaves`
---------------

<!--- From CRAN

```r
install.packages("tealeaves")
```

or from -->
GitHub

    install.packages("devtools")
    devtools::install_github("cdmuir/tealeaves")

And load `tealeaves`

    library("tealeaves")

The `tealeaves` package solves for leaf temperature given a set of
environmental conditions and leaf traits by balancing the leaf energy
budget. There are two main steps to using `tealeaves`:

1.  define leaf parameters, environmental parameters, and physical
    constants; and
2.  solve for the a leaf temperature that balances its energy budget
    (`tleaf` and `tleaves` for single and multiple parameter sets,
    respectively).

In this vignette, I'll show you how to:

-   run a minimum worked example using default parameters
-   replace default parameters
-   solve for leaf temperature along an environmental gradient

Minimum worked example
----------------------

You can use the default parameter settings and solve for leaf
temperature in a single leaf using the `make_*()` functions and
`tleaf()`.


    library(magrittr)
    library(tealeaves)

    # Leaving the make_* functions empty will automatically default to defaults
    # parameters.
    leaf_par   <- make_leafpar()   # leaf parameters
    enviro_par <- make_enviropar() # environmental parameters
    constants  <- make_constants() # physical constants

    T_leaf <- tleaf(leaf_par, enviro_par, constants, progress = FALSE, quiet = TRUE)

    T_leaf
    #>     T_leaf   init        value convergence   R_abs      S_r        H
    #> 1 304.3652 298.15 5.034899e-05           0 1600.25 943.9849 203.6401
    #>          L          E
    #> 1 452.6249 0.01035236

Replace default parameters
--------------------------

You can look at default parameters settings in the manual (run
`?make_parameters`). These defaults are reasonable, but of course you
will probably want to use different choices and allow some parameters to
vary. Here, I'll demonstrate how to replace a default. In the next
section, I'll show you how to set up a gradient of parameter values over
which to solve for leaf temperature.


    # Use the `replace` argument to replace defaults. This must be a named list, and
    # each named element must have the proper units specified. See `?make_parameters`
    # for all parameter names and proper units.

    # First, we'll change stomatal conductance to 4 umol / (m^2 s Pa)
    leaf_par   <- make_leafpar(
      replace = list(
        g_sw = set_units(4, "umol/m^2/s/Pa")
        )
      )

    # Next, we'll change the air temperature to 25 degree C (= 298.15 K)
    enviro_par <- make_enviropar(
      replace = list(
        T_air = set_units(298.15, "K")
        )
      )

    # Physical constants probably do not need to be replaced in most cases,
    # that's why we call them 'constants'!
    constants  <- make_constants()

    T_leaf <- tleaf(leaf_par, enviro_par, constants, progress = FALSE, quiet = TRUE)

    T_leaf
    #>     T_leaf   init       value convergence   R_abs      S_r        H
    #> 1 305.2116 298.15 3.06387e-05           0 1600.25 954.5288 231.1993
    #>          L           E
    #> 1 414.5218 0.009488798

Environmental gradients
-----------------------

In the previous two examples, I used the `tleaf` function to solve for a
single parameter set. In most cases, you'll want to solve for many
parameter sets. The function `tleaves` generalizes `tleaf` and makes it
easy to solve for multiple parameter sets using the same argument
structure. All you need to do is specify multiple values for one or more
leaf or environmental parameters and `tleaves` uses the
`tidyr::crossing` function to fit all combinations[1].


    # As before, use the `replace` argument to replace defaults, but this time we
    # enter multiple values

    # First, we'll change stomatal conductance to to 2 and 4 umol / (m^2 s Pa)
    leaf_par  <- make_leafpar(
      replace = list(
        g_sw = set_units(c(2, 4), "umol/m^2/s/Pa")
        )
      )

    # Next, we'll change the air temperature to 20 and 25 degree C (= 293.15 and 298.15 K)
    enviro_par <- make_enviropar(
      replace = list(
        T_air = set_units(c(293.15, 298.15), "K")
        )
      )

    constants  <- make_constants()

    # Now there should be 4 combinations (high and low g_sw crossed with high and low T_air)
    T_leaves <- tleaves(leaf_par, enviro_par, constants, progress = FALSE, quiet = TRUE)

    T_leaves %>% dplyr::select(T_air, g_sw, T_leaf)
    #> # A tibble: 4 x 3
    #>    T_air            g_sw   T_leaf
    #>      [K] [umol/m^2/Pa/s]      [K]
    #> 1 293.15               2 304.8484
    #> 2 298.15               2 307.8130
    #> 3 293.15               4 302.3785
    #> 4 298.15               4 305.2116

Contributors
------------

-   [Chris Muir](https://github.com/cdmuir)

Comments and contributions
--------------------------

I welcome comments, criticisms, and especially contributions! GitHub
issues are the preferred way to report bugs, ask questions, or request
new features. You can submit issues here:

<https://github.com/cdmuir/tealeaves/issues>

Meta
----

-   Please [report any issues or
    bugs](https://github.com/cdmuir/tealeaves/issues).
-   License: MIT
    <!--- * Get citation information for `tealeaves` in R doing `citation(package = 'tealeaves')` -->
-   Please note that this project is released with a [Contributor Code
    of Conduct](CONDUCT.md). By participating in this project you agree
    to abide by its terms.

[1] Since optimization is somewhat time-consuming, be careful about
crossing too many combinations. Use `progress = TRUE` to show progress
bar with estimated time remaining.
