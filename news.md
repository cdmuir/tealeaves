# tealeaves (development version)

# tealeaves 1.0.0

tealeaves implements models of leaf temperature using energy balance. It uses units to ensure that parameters are properly specified and transformed before calculations. It allows separate lower and upper surface conductances to heat and water vapor, so sensible and latent heat loss are calculated for each surface separately as in Foster and Smith (1986). It's straightforward to model leaf temperature over environmental gradients such as light, air temperature, humidity, and wind. It can also model leaf temperature over trait gradients such as leaf size or stomatal conductance.

* Added a `NEWS.md` file to track changes to the package.