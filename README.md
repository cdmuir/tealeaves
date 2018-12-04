tealeaves <img src="hex-sticker/hex-sticker.png" align="right" height="200" width="200"/>
=========================================================================================

[![Project Status: WIP - Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
[![Build
Status](https://travis-ci.com/cdmuir/tealeaves.svg?branch=master)](https://travis-ci.com/cdmuir/tealeaves)

Solve for leaf temperature using energy balance
-----------------------------------------------

Description
-----------

{tealeaves} is a lightweight R package to model leaf temperature using
leaf energy balance. It uses the R package
[units](https://cran.r-project.org/web/packages/units/index.html) to
ensure that parameters are properly specified and transformed before
calculations. It allows separate lower and upper surface conductances to
heat and water vapour, so sensible and latent heat loss are calculated
for each surface separately. It's straightforward to model leaf
temperature over environmental gradients such as light, air temperature,
humidity, and wind, or trait gradients such as leaf size or stomatal
conductance.

Get tealeaves
-------------

<!--- From CRAN

```r
install.packages("tealeaves")
```

or from -->
GitHub

    install.packages("devtools")
    devtools::install_github("cdmuir/tealeaves")

And load tealeaves

    library("tealeaves")

Vignette
--------

The {tealeaves} package solves for leaf temperature given a set of
environmental conditions and leaf traits by balancing the leaf energy
budget. There are two main steps to using {tealeaves}:

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

    T_leaf <- tleaf(leaf_par, enviro_par, constants, quiet = TRUE, unitless = FALSE)

    T_leaf %>% knitr::kable()

<table>
<thead>
<tr class="header">
<th align="right">T_leaf</th>
<th align="right">value</th>
<th align="right">convergence</th>
<th align="right">R_abs</th>
<th align="right">S_r</th>
<th align="right">H</th>
<th align="right">L</th>
<th align="right">E</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="right">304.3652 [K]</td>
<td align="right">-1e-07</td>
<td align="right">0</td>
<td align="right">1600.25 [W/m^2]</td>
<td align="right">943.9849 [W/m^2]</td>
<td align="right">203.6401 [J/m^2/s]</td>
<td align="right">452.625 [W/m^2]</td>
<td align="right">0.01035237 [mol/m^2/s]</td>
</tr>
</tbody>
</table>

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
    leaf_par <- make_leafpar(
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

    T_leaf <- tleaf(leaf_par, enviro_par, constants, quiet = TRUE, unitless = FALSE)

    T_leaf %>% knitr::kable()

<table>
<thead>
<tr class="header">
<th align="right">T_leaf</th>
<th align="right">value</th>
<th align="right">convergence</th>
<th align="right">R_abs</th>
<th align="right">S_r</th>
<th align="right">H</th>
<th align="right">L</th>
<th align="right">E</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="right">305.2116 [K]</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">1600.25 [W/m^2]</td>
<td align="right">954.5288 [W/m^2]</td>
<td align="right">231.1993 [J/m^2/s]</td>
<td align="right">414.5218 [W/m^2]</td>
<td align="right">0.009488798 [mol/m^2/s]</td>
</tr>
</tbody>
</table>

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
    T_leaves <- tleaves(leaf_par, enviro_par, constants, progress = FALSE, 
                        quiet = TRUE, unitless = FALSE)

    T_leaves %>% 
      dplyr::select(T_air, g_sw, T_leaf) %>%
      knitr::kable()

<table>
<thead>
<tr class="header">
<th align="right">T_air</th>
<th align="right">g_sw</th>
<th align="right">T_leaf</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="right">293.15 [K]</td>
<td align="right">2 [umol/m^2/Pa/s]</td>
<td align="right">304.8484</td>
</tr>
<tr class="even">
<td align="right">298.15 [K]</td>
<td align="right">2 [umol/m^2/Pa/s]</td>
<td align="right">307.8130</td>
</tr>
<tr class="odd">
<td align="right">293.15 [K]</td>
<td align="right">4 [umol/m^2/Pa/s]</td>
<td align="right">302.3785</td>
</tr>
<tr class="even">
<td align="right">298.15 [K]</td>
<td align="right">4 [umol/m^2/Pa/s]</td>
<td align="right">305.2116</td>
</tr>
</tbody>
</table>

Parallel processing
-------------------

It can take a little while to model many different parameter sets. If
you have multiple processors available, you can speed things up by
running simulations in parallel. In the `tealeaves` function, simply use
the `parallel = TRUE` argument to simulate in parallel. Here I'll
provide an example looking at how leaf-to-air temperature differentials
change with air temperature.


    # We'll use the `replace` argument to enter multiple air temperatures and two light levels

    leaf_par  <- make_leafpar()

    enviro_par <- make_enviropar(
      replace = list(
        S_sw = set_units(c(300, 1000), "W/m^2"),
        T_air = set_units(seq(273.15, 313.15, length.out = 10), "K")
        )
      )

    constants  <- make_constants()

    tl <- tleaves(leaf_par, enviro_par, constants, progress = FALSE, quiet = TRUE,
                  parallel = TRUE)
    tl$T_air %<>% drop_units() # for plotting
    tl %<>% dplyr::mutate(Light = dplyr::case_when(
     round(drop_units(S_sw), 0) == 300 ~ "Shade",
     round(drop_units(S_sw), 0) == 1000 ~ "Sun"
    ))

    # Plot T_air versus T_leaf - T_air at different light levels
    library(ggplot2)
    ggplot(tl, aes(T_air, T_leaf - T_air, color = Light)) +
      geom_line() +
      xlab("Air Temperature [K]") +
      ylab("Leaf - Air Temperature [K]") +
      theme_minimal() +
      NULL

![](README_files/figure-markdown_strict/Parallel%20example-1.png)

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
