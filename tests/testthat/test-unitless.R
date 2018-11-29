context("unitless values match unit-ed values")
library(tealeaves)

test_that("unitless values match unit-ed values", {
  
  cs <- make_constants()
  lp <- leaf_par(list(
    abs_l = set_units(runif(1)),
    abs_s = set_units(runif(1)),
    g_sw = set_units(runif(1, 0, 10), "umol/m^2/s/Pa"),
    g_uw = set_units(runif(1), "umol/m^2/s/Pa"),
    leafsize = set_units(runif(1), "m"),
    logit_sr = set_units(runif(1, -10, 10))
  ))
  ep <- enviro_par(list(
    P = set_units(101.3246, "kPa"),
    RH = set_units(runif(1)),
    S_lw = set_units(runif(1, 0, 1000), "W/m^2"),
    S_sw = set_units(runif(1, 0, 2000), "W/m^2"),
    T_air = set_units(runif(1, 273.15, 313.15), "K"),
    wind = set_units(runif(1, 0, 20), "m/s")
  ))
  T_leaf <- set_units(runif(1, 273.15, 313.15), "K")
  pars1 <- c(cs, lp, ep)
  pars2 <- purrr::map_if(pars1, function(x) is(x, "units"), drop_units)

  ar1 <- drop_units(Ar(T_leaf, pars1, unitless = FALSE))
  ar2 <- Ar(drop_units(T_leaf), pars2, unitless = TRUE)
  expect_equal(ar1, ar2)

  dwv1 <- drop_units(.get_dwv(T_leaf, pars1, unitless = FALSE))
  dwv2 <- .get_dwv(drop_units(T_leaf), pars2, unitless = TRUE)
  expect_equal(dwv1, dwv2)

  dx1 <- drop_units(.get_Dx(pars1$D_w0, T_leaf, pars1$eT, pars1$P, unitless = FALSE))
  dx2 <- .get_Dx(pars2$D_w0, drop_units(T_leaf), pars2$eT, pars2$P, unitless = TRUE)
  expect_equal(dx1, dx2)
  
  gbw1 <- drop_units(.get_gbw(T_leaf, "lower", pars1, unitless = FALSE))
  gbw2 <- .get_gbw(drop_units(T_leaf), "lower", pars2, unitless = TRUE)
  expect_equal(gbw1, gbw2)

  gbw3 <- drop_units(.get_gbw(T_leaf, "upper", pars1, unitless = FALSE))
  gbw4 <- .get_gbw(drop_units(T_leaf), "upper", pars2, unitless = TRUE)
  expect_equal(gbw3, gbw4)
  
  gh1 <- drop_units(.get_gh(T_leaf, "lower", pars1, unitless = FALSE))
  gh2 <- .get_gh(drop_units(T_leaf), "lower", pars2, unitless = TRUE)
  expect_equal(gh1, gh2)

  gh3 <- drop_units(.get_gh(T_leaf, "upper", pars1, unitless = FALSE))
  gh4 <- .get_gh(drop_units(T_leaf), "upper", pars2, unitless = TRUE)
  expect_equal(gh3, gh4)
  
  gr1 <- drop_units(.get_gr(T_leaf, pars1, unitless = FALSE))
  gr2 <- .get_gr(drop_units(T_leaf), pars2, unitless = TRUE)
  expect_equal(gr1, gr2)

  gtw1 <- drop_units(.get_gtw(T_leaf, pars1, unitless = FALSE))
  gtw2 <- .get_gtw(drop_units(T_leaf), pars2, unitless = TRUE)
  expect_equal(gtw1, gtw2)

  H1 <- drop_units(.get_H(T_leaf, pars1, unitless = FALSE))
  H2 <- .get_H(drop_units(T_leaf), pars2, unitless = TRUE)
  expect_equal(H1, H2)
  
  hvap1 <- drop_units(.get_hvap(T_leaf, unitless = FALSE))
  hvap2 <- .get_hvap(drop_units(T_leaf), unitless = TRUE)
  expect_equal(hvap1, hvap2)

  L1 <- drop_units(.get_L(T_leaf, pars1, unitless = FALSE))
  L2 <- .get_L(drop_units(T_leaf), pars2, unitless = TRUE)
  expect_equal(L1, L2)
  
  nu1 <- drop_units(.get_nu(T_leaf, "lower", pars1, unitless = FALSE))
  nu2 <- .get_nu(drop_units(T_leaf), "lower", pars2, unitless = TRUE)
  expect_equal(nu1, nu2)
  
  nu3 <- drop_units(.get_nu(T_leaf, "upper", pars1, unitless = FALSE))
  nu4 <- .get_nu(drop_units(T_leaf), "upper", pars2, unitless = TRUE)
  expect_equal(nu3, nu4)
  
  Pa1 <- drop_units(.get_Pa(T_leaf, pars1, unitless = FALSE))
  Pa2 <- .get_Pa(drop_units(T_leaf), pars2, unitless = TRUE)
  expect_equal(Pa1, Pa2)
  
  ps1 <- drop_units(.get_ps(T_leaf, pars1$P, unitless = FALSE))
  ps2 <- .get_ps(drop_units(T_leaf), pars2$P, unitless = TRUE)
  expect_equal(ps1, ps2)
  
  Rabs1 <- drop_units(.get_Rabs(pars1))
  Rabs2 <- .get_Rabs(pars2)
  expect_equal(Rabs1, Rabs2)
  
  re1 <- drop_units(.get_re(T_leaf, pars1, unitless = FALSE))
  re2 <- .get_re(drop_units(T_leaf), pars2, unitless = TRUE)
  expect_equal(re1, re2)

  sh1 <- drop_units(.get_sh(T_leaf, "lower", pars1, unitless = FALSE))
  sh2 <- .get_sh(drop_units(T_leaf), "lower", pars2, unitless = TRUE)
  expect_equal(sh1, sh2)
  
  sh3 <- drop_units(.get_sh(T_leaf, "upper", pars1, unitless = FALSE))
  sh4 <- .get_sh(drop_units(T_leaf), "upper", pars2, unitless = TRUE)
  expect_equal(sh3, sh4)
  
  Sr1 <- drop_units(.get_Sr(T_leaf, pars1))
  Sr2 <- .get_Sr(drop_units(T_leaf), pars2)
  expect_equal(Sr1, Sr2)
  
  eb <- energy_balance(T_leaf, lp, ep, cs, quiet = TRUE, components = FALSE, unitless = FALSE)
  eb <- energy_balance(T_leaf, lp, ep, cs, quiet = TRUE, components = TRUE, unitless = FALSE)
  
  tl1 <- tleaf(lp, ep, cs, TRUE, FALSE)
  tl2 <- tleaf(lp, ep, cs, TRUE, TRUE)
  expect_equal(tl1$T_leaf, tl2$T_leaf)
  
})
