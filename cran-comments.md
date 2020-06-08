## Resubmission
This is a resubmission. In this version I have:
* Added citation to published paper. See `citation(package = "tealeaves")`.
* In `enviro_par()`, "sky" temperature (`T_sky`) can now be provided directly as a values (in K) or as a function (the default).
* If `parallel = TRUE` in `tleaves()`, **future** uses `plan("multisession")` rather than `plan("multiprocess")`.
* New vignette on making parameters functions of other parameters.
* Added full URL for `CONDUCT.md` in README

## Test environments
* local R installation, R 4.0.0
* ubuntu 16.04 (on travis-ci), R 4.0.0
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 0 notes
