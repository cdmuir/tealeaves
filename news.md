# tealeaves (development version)

# tealeaves 1.0.6

* Fixed name in inst/CITATION
* Stopped parallel evaluation in vignette
* Fixed links in README

# tealeaves 1.0.5

* Updated site, vignettes, and README

# tealeaves 1.0.4

* Added unit tests for parameter functions under `tests/test-parameter-functions.R`
* Fixed bug with crossing parameters in `tleaves()` that was introduced with new `T_sky` function. This led to crossing all parameter values with all unique values of calculated `T_sky`, which was incorrect. Added unit tests to ensuring that crossing is done correctly under `tests/test-tleaves-crossing.R`
* Added code coverage using [codecov](https://app.codecov.io/gh/cdmuir/tealeaves?branch=master)

# tealeaves 1.0.3

* Fixed bug in `.get_Rabs()` that would have over-written custom `T_sky` function.

# tealeaves 1.0.2

* Added citation to published paper. See `citation(package = "tealeaves")`.
* Compatible with **dplyr 1.0.0**
* In `enviro_par()`, "sky" temperature (`T_sky`) can now be provided directly as a values (in K) or as a function (the default).
* If `parallel = TRUE` in `tleaves()`, **future** uses `plan("multisession")` rather than `plan("multiprocess")`.
* New vignette on making parameters functions of other parameters.
* Added full URL for `CONDUCT.md` in README

# tealeaves 1.0.1

* `constants()`, `enviro_par()`, and `leaf_par()` no longer require values be provided with units. If values are provided without units, they are assigned proper units, but no unit conversion is performed. If values have units, these functions check whether units are correct and convert them to units used in the package.

* Release to be archived with revision of "Is amphistomy an adaptation to high light? Optimality models of stomatal traits along light gradients."

* [Blog post.](https://cdmuir.netlify.app/post/2019-05-21-phyteclub/)

# tealeaves 1.0.0

tealeaves implements models of leaf temperature using energy balance. It uses units to ensure that parameters are properly specified and transformed before calculations. It allows separate lower and upper surface conductances to heat and water vapor, so sensible and latent heat loss are calculated for each surface separately as in Foster and Smith (1986). It's straightforward to model leaf temperature over environmental gradients such as light, air temperature, humidity, and wind. It can also model leaf temperature over trait gradients such as leaf size or stomatal conductance.

* Added a `NEWS.md` file to track changes to the package.
