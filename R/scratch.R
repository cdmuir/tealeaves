library(tealeaves)
library(tidyverse)
lp <- make_leafpar()
ep <- make_enviropar()
cs <- make_constants()

pars <- c(lp, ep, cs) %>%
  map_if(~ is(.x, "units"), drop_units)

.get_hc <- function(pars) {
  
  # nu_a: Kinematic viscosity of air (m^2 s-1). Eqn B19
  # Equal to to D_m when P = 101.3246 kPa
  # Why does nu_a not depend on P?
  # Assumes that T_air is in K
  nu_a <- 9e-8 * pars$T_air - 1.13e-5
  
  # Re: Reynold's number (unitless).eqn B11. 
  # Close to .get_Re() in some examples I tried
  # Assumes that wind in m/s, leafsize in m
  Re <- pars$wind * pars$leafsize / nu_a
  
  # Nu: Nusselt Number (unitless). Eqn B13
  # Assumes forced convection
  Re_critical <- 4000 # this should be a parameter (I assume 4000, but Schymanksi uses 3000)
  Pr <- 0.71 # Prandtl number. Should go in parameters
  C_2 <- (Re + Re_critical - abs(Re_critical - Re)) / 2
  C_1 <- 0.037 * C_2 ^ 0.8 - 0.664 * C_2 ^ 0.5
  Nu <- (0.037 * Re ^ 0.8 - C_1) * Pr

  # k_a: Thermal conductivity of dry air (J/K/m/s). Eqn B18
  # Assumes T_air in K
  k_a <- 6.84e-5 * pars$T_air + 5.62e-3
  
  # h_C: Average one-sided convective transfer coefficient (J/K/m^2/s). Eqn B10
  # Assumes leafsize in m
  h_C <- k_a * Nu / pars$leafsize
  
  h_C
  
}

.get_ch <- function(pars) {
  
  # c_H: Sensible heaf transfer coefficient = a_sH * h_C (J/K/m^2/s). pg. 688, right column, second paragraph)
  a_sH <- 2 # 2 for planar leaf, 1 for soil. Should be leaf parameter
  c_H <- 2 * .get_hc(pars)
  
  c_H
  
}


.get_ce <- function(pars) {
  
  # M_w: molar mass of water (kg/mol)
  # Add to constants? Just to convert lambda_E from mol to kg
  M_w <- 0.018
  
  # g_bw: boundary layer conductance to water vapor (m/s). Eqn B2
  a_s <- 2 # for two-sided transpiring surface
  h_C <- .get_hc(pars)
  # P_a = rho_a, density of dry air
  # don't need to convert to kg/m^3 because c_p is in g rather than kg
  P_a <- tealeaves:::.get_Pa(pars$T_air, pars, TRUE)
  
  # D_h = alpha_a: Thermal diffusivity of dry air (m^2/s). Eqn B17
  # basically equivalent to .get_Dx, but not adjusted for P and linear rather than exponential
  # tealeaves:::.get_Dx(pars$D_h0, pars$T_air, pars$eT, pars$P, TRUE)
  # Assumes T_air in K
  D_h <- 1.32e-7 * pars$T_air - 1.73e-5

  # D_hw = D_va: Binary diffusion coefficient of water vapor in air (m^2/s). Eqn B16
  # basically equivalent to .get_Dx, but not adjusted for P and linear rather than exponential
  # tealeaves:::.get_Dx(pars$D_w0, pars$T_air, pars$eT, pars$P, TRUE)
  # Assumes T_air in K
  D_w <- 1.49e-7 * pars$T_air - 1.96e-5
  
  # Lewis number (unitless). Eqn B3
  Le <- D_h / D_w
  
  g_bw <- (a_s * h_C) / (P_a * pars$c_p * Le ^ (2/3))
  
  # g_tw: total conductance to water vapor (m/s). Eqn 6
  # g_sw needs to be converted from umol/m^2/s/Pa to m/s
  # can get indentical result: convert_conductance(lp$g_sw, ep$T_air, ep$P)$`m/s`
  g_sw1 <- pars$g_sw * pars$R * pars$T_air / 1000000
  
  g_tw <- 1 / (1 / g_sw1 + 1 / g_bw)
  
  # Convert g_tw (m/s) to g_twmol (mol/m^2/s). Eqn B8
  # convert_conductance(set_units(2, m/s), pars$T_air, pars$P) # can get equivalent results
  # P should be in Pa (tealeaves defaults to kPa, hence 1000)
  # T_air should be in K
  g_twmol <- g_tw * (1000 * pars$P) / (pars$R * pars$T_air)
  
  # c_E: Latent heat transfer coefficient (J/Pa/m^2/s). Eqn 14
  # P should be in Pa (tealeaves defaults to kPa, hence 1000)
  # lambda_E: latent heat of vaporization in J/kg
  # similar to h_vap, but h_vap is J/mol and depends on Temp
  lambda_E <- 2.45e6
  c_E <- M_w * lambda_E * g_twmol / (1000 * pars$P)
  
  c_E
  
}

P_a <- tealeaves:::.get_Pa(pars$T_air, pars, TRUE)
g_h <- sum(tealeaves:::.get_gh(pars$T_air, "lower", pars, TRUE), tealeaves:::.get_gh(pars$T_air, "upper", pars, TRUE))
P_a * pars$c_p * g_h
.get_ch(pars)

eq27 <- function(pars) {

  c_H <- .get_ch(pars)
  c_E <- .get_ce(pars)
  
  # delta_Ta: Slope of saturation vapour pressure at air temperature (Pa/K). Eqn B27 (approx)
  # See notes on M_w and lambda_E in .get_ce function
  M_w <- 0.018
  lambda_E <- 2.45e6
  delta_Ta <- (611 * lambda_E * M_w * exp(lambda_E * M_w / pars$R * (1 / 273 - 1 / pars$T_air))) / (pars$R * pars$T_air ^ 2)
  
  # p_air = P_wa: vapour pressure in the atmosphere (Pa)
  # p_sat = P_was: saturation vapour pressure in the atmosphere (Pa)
  # .get_ps return pressure in kPa, hence conversion by 1000x to Pa
  p_sat <- 1000 * tealeaves:::.get_ps(pars$T_air, pars$P, TRUE)
  p_air <- p_sat * pars$RH
  
  # Original equation 27 (pg 690)
  # (R_s + c_H * T_air + c_E * (delta_Ta * T_air + p_air - p_sat) + a_sh * e_l * s * (3 * T_air ^ 4 + T_w ^ 4) / (c_H + c_E * delta_Ta + 4 * a_sh * abs_l * s * T_air ^ 3)
  #
  # I assume:
  # a_sh = 2
  # T_air = T_w
  R_s <- 0 # calculated from input
  
  
  T_leaf <- (R_s + c_H * pars$T_air + c_E * (delta_Ta * pars$T_air + p_air - p_sat) + 2 * pars$abs_l * pars$s * (4 * pars$T_air ^ 4)) / (c_H + c_E * delta_Ta + 4 * 2 * pars$abs_l * pars$s * pars$T_air ^ 3)
  
  T_leaf # in K
  
}
