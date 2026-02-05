## Resubmission
* Removed broken link to travis-ci badge in README
* Replaced deprecated `dplyr::progress_estimated()` with `progressr::progressor()`
* Removed `@docType` in `R/tealeaves.R`
* Removed `@inheritParams` tags where all parameters already documented
* Fixed `constants()`, `enviro_par()`, and `leaf_par()` classes
* Fixed URLs

## Test environments
* local: R 4.5.2, macOS (Tahoe 26.2)
* win-builder (release): R 4.5.2 (x86_64-w64-mingw32)
* win-builder (old-release): R 4.4.3 (x86_64-w64-mingw32)
* win-builder (devel), R 2026-02-04 r89376 ucrt (x86_64-w64-mingw32) 

## R CMD check results
0 errors | 0 warnings | 0 notes

## Reverse dependencies
- Ran revdepcheck::revdep_check(); 0 problems; 0 failed checks.