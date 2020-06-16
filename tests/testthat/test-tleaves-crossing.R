context("tleaves() crossing")
library(tealeaves)

test_that("tleaves output has correct dimensions", {

library(testthat)

cs <- make_constants()
lp <- make_leafpar()
ep <- make_enviropar(replace = list(
  T_sky = set_units(c(298.15, 303.15), K)
))

tl <- tleaves(lp, ep, cs, progress = FALSE, quiet = TRUE, 
              set_units = FALSE, parallel = FALSE)

n <- exp(sum(log(c(sapply(lp, length), sapply(ep, length)))))
expect_equal(nrow(tl), n)

ep <- make_enviropar(replace = list(
  T_sky = c(function(pars) {pars$T_air}, function(pars) {2 * pars$T_air})))

tl <- tleaves(lp, ep, cs, progress = FALSE, quiet = TRUE, 
              set_units = FALSE, parallel = FALSE)

n <- exp(sum(log(c(sapply(lp, length), sapply(ep, length)))))
expect_equal(nrow(tl), n)

ep <- make_enviropar(replace = list(
  T_sky = c(function(pars) {pars$T_air}, function(pars) {2 * pars$T_air})))

replace <- list(sh_constant = set_units(1), nu_constant = set_units(1), s = set_units(1, W/K^4/m^2))
})