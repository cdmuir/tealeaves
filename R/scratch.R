library(tealeaves)
library(tidyverse)
lp <- make_leafpar()
ep <- make_enviropar()
cs <- make_constants()

pars <- c(lp, ep, cs) %>%
  map_if(~ is(.x, "units"), drop_units)

.get_ch <- function(pars) {
  
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
  
  # c_H: Sensible heaf transfer coefficient = a_sH * h_C (J/K/m^2/s). pg. 688, right column, second paragraph)
  a_sH <- 2 # 2 for planar leaf, 1 for soil. Should be leaf parameter
  c_H <- 2 * h_C
  
  c_H
  
}

P_a <- tealeaves:::.get_Pa(pars$T_air, pars, TRUE)
g_h <- sum(tealeaves:::.get_gh(pars$T_air, "lower", pars, TRUE), tealeaves:::.get_gh(pars$T_air, "upper", pars, TRUE))
P_a * pars$c_p * g_h
c_H

.get_Ce <- function(pars) {
  
  
}

eq27 <- function(pars) {

  # Original equation 27 (pg 690)
  # (R_s + c_H * T_air + c_E * (delta_Ta * T_air + P_wa - P_was) + a_sh * e_l * s * (3 * T_air ^ 4 + T_w ^ 4) / (c_H + c_E * delta_Ta + 4 * a_sh * e_l * s * T_air ^ 3)
  #
  # I assume:
  # a_sH = 2
  # T_air = T_w
  (R_s + c_H * T_air + c_E * (delta_Ta * T_air + P_wa - P_was) + 4 * T_air ^ 4 * e_l * s) / (c_H + c_E * delta_Ta + 4 * e_l * s * T_air ^ 3)
  
  # c_H = a_sH ?
  # Sensible heat transfer coefficient: J Pa^-1 m^-2 s^-1
  
  # Notes:
  #
  # R_abs - S_r = R_s - R_ll
  tealeaves:::.get_Rabs
}