context("tleaves() crossing")
library(tealeaves)

test_that("tleaves output has correct dimensions", {

cs <- make_constants()
lp <- make_leafpar()
ep <- make_enviropar(replace = list(
  T_sky = set_units(c(298.15, 303.15), K)
))

tl <- tleaves(lp, ep, cs, progress = FALSE, quiet = TRUE, 
              set_units = FALSE, parallel = FALSE)

n <- exp(sum(log(c(sapply(lp, length), sapply(ep, length)))))
expect_equal(nrow(tl), n)


ep <- make_enviropar(replace = list(T_sky = function(pars) {pars$T_air}))

tl <- tleaves(lp, ep, cs, progress = FALSE, quiet = TRUE, 
              set_units = FALSE, parallel = FALSE)

n <- exp(sum(log(c(sapply(lp, length), sapply(ep, length)))))
expect_equal(nrow(tl), n)


ep <- make_enviropar(replace = list(
  T_air = set_units(c(298.15, 303.15), K),
  T_sky = function(pars) {pars$T_air}
))
tl <- tleaves(lp, ep, cs, progress = FALSE, quiet = TRUE, 
              set_units = FALSE, parallel = FALSE)
n <- exp(sum(log(c(sapply(lp, length), sapply(ep, length)))))
expect_equal(nrow(tl), n)

})