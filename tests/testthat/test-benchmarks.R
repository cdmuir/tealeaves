context("benchmarks")
library(magrittr)
library(tealeaves)

test_that("benchmarks haven't changed", {
  
  cs <- make_constants()
  lp <- make_leafpar(replace = list(abs_s = set_units(0.8)))
  ep <- make_enviropar()

  tl <- tleaf(lp, ep, cs, set_units = TRUE)
  expect_equal(round(tl$T_leaf, 4), set_units(305.8332, K))
  
  p <- tidyr::crossing(
    abs_s = set_units(0.5),
    g_sw = set_units(c(0.1, 1, 10), umol/m^2/s/Pa),
    leafsize = set_units(c(0.004, 0.04, 0.4), m),
    logit_sr = set_units(c(-10, 0, 10)),
    S_sw = set_units(c(100, 500, 1000), W/m^2),
    T_air = set_units(seq(273.15, 313.15, 10), K),
    wind = set_units(c(0.01, 0.1, 1, 10), m/s)
  )
  p$T_sky <- p$T_air - set_units(20, K) * p$S_sw / set_units(1000, W / m ^ 2)
  
  p$T_leaf <- numeric(nrow(p))
  
  # Create benchmarks (last updated: 2019-04-06)
  # for (i in 1:nrow(p)) {
  #   lp$g_sw <- set_units(p$g_sw[i], umol/m^2/s/Pa)
  #   ep$T_air <- set_units(p$T_air[i], K)
  #   tl <- tleaf(lp, ep, cs, unitless = TRUE)
  #   p$T_leaf[i] <- tl$T_leaf
  # }
  # readr::write_csv(p, "tests/testthat/benchmarks.csv")

  p <- read.csv("benchmarks.csv")
  for (i in sample(nrow(p), 5)) { # 1:nrow(p)) {
    lp$g_sw <- set_units(p$g_sw[i], umol/m^2/s/Pa)
    ep$T_air <- set_units(p$T_air[i], K)
    tl <- tleaf(lp, ep, cs, set_units = TRUE)
    expect_equal(round(tl$T_leaf, 3), set_units(round(p$T_leaf[i], 3), K))
  }
  
})
