#' Find leaf temperature
#'
#' @param leaf_par A list of leaf parameters. This can be generated using the \code{make_leafpar} function.
#' @param enviro_par A list of environmental parameters. This can be generated using the \code{make_enviropar} function.
#' @param constants A list of physical constants. This can be generated using the \code{make_constants} function.
#' @param ... Additional arguments passed to \code{optim}
#'
#' @return An object of class \code{tealeaf}
#' 
#' @examples 
#' 
#' leaf_par <- make_leafpar()
#' enviro_par <- make_enviropar()
#' constants <- make_constants()
#' pars <- c(leaf_par, enviro_par, constants)
#' T_leaf <- tleaf(leaf_par, enviro_par, constants)
#' T_leaf
#' 
#' @export
#'

tleaf <- function(leaf_par, enviro_par, constants) {

  # Balance energy fluxes -----
  enviro_par$T_air %<>% set_units("K") # convert T_air to Kelvin before dropping units
  
  soln <- stats::optim(drop_units(enviro_par$T_air), engery_balance, leaf_par = leaf_par,
                       enviro_par = enviro_par, constants = constants,
                       abs_val = TRUE, quiet = TRUE, method = "Brent",
                       lower = drop_units(enviro_par$T_air - set_units(30, "K")),
                       upper = drop_units(enviro_par$T_air + set_units(30, "K")))

  # Check or report on convergence?? -----
  # ?

  # Return -----
  # return evaporation?
  T_leaf <- soln$par

}

#' Calculate leaf energy balance
#'
#' @inheritParams tleaf
#' @param T_leaf Leaf temperature in Kelvin. If input is numeric, it will be automatically converted to \code{units}.
#' @param quiet Logical. Should a message appear about conversion from \code{numeric} to \code{units}?
#' @param abs_val Return absolute value? Useful for finding leaf temperature that balances heat transfer.
#'
#' @export
#'

engery_balance <- function(T_leaf, leaf_par, enviro_par, constants, 
                           quiet = FALSE, abs_val = FALSE) {

  # Checks -----
  warning("implement checks in energy_balance")
  #traits <- .missing_traits(character(0))
  #check_leafpar(leaf_par, traits)
  #check_enviropar(enviro_par)
  #check_constants(constants)

  ## Convert T_leaf to units and message
  if (!is(T_leaf, "units")) {
    if (!quiet) {
      glue::glue("T_leaf converted from numeric to {X} K", X = T_leaf) %>%
        message()
    }
    T_leaf %<>% set_units("K")
  }
  pars <- c(leaf_par, enviro_par, constants)

  # R_abs: total absorbed radiation (W m^-2) -----
  R_abs <- .get_Rabs(pars) %>% drop_units()

  # S_r: longwave re-radiation (W m^-2) -----
  S_r <- .get_Sr(pars) %>% drop_units()

  # H: sensible heat flux density (W m^-2) -----
  H <- .get_H(T_leaf, pars) %>% drop_units()

  # L: latent heat flux density (W m^-2) -----
  L <- .get_L(T_leaf, pars) %>% drop_units()

  # Return -----
  if (abs_val) return(abs(R_abs - (S_r + H + L)))
  R_abs - (S_r + H + L)

}

#' R_abs: total absorbed radiation (W / m^2)
#'
#' @param pars Concatenated parameters (\code{leaf_par}, \code{enviro_par}, and \code{constants})
#' 
#' @return Value in W / m\eqn{^2} of class \code{units}
#' 
#' @details
#' 
#' \deqn{R_\text{abs} = \alpha_\text{s} S_\text{sw} + \alpha_\text{l} S_\text{lw}}{R_abs = \alpha_s * S_sw + \alpha_l * S_lw}
#' 
#' \tabular{lllll}{
#' \emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \eqn{\alpha_\text{s}}{\alpha_s} \tab \code{abs_s} \tab absortivity of shortwave radiation (0.3 - 4 \eqn{\mu}m) \tab none \tab 0.80\cr
#' \eqn{\alpha_\text{l}}{\alpha_l} \tab \code{abs_l} \tab absortivity of longwave radiation (4 - 80 \eqn{\mu}m) \tab none \tab 0.97\cr
#' \eqn{S_\text{sw}}{S_sw} \tab \code{S_sw} \tab incident short-wave (solar) radiation flux density \tab W / m\eqn{^2} \tab 1000\cr
#' \eqn{S_\text{lw}}{S_lw} \tab \code{S_lw} \tab incident long-wave radiation flux density \tab W / m\eqn{^2} \tab 825
#' }

.get_Rabs <- function(pars) {
  R_abs <- with(pars, abs_s * S_sw + abs_l * S_lw)
  R_abs
}

#' S_r: longwave re-radiation (W / m^2)
#'
#' @inheritParams .get_Rabs
#' 
#' @return Value in W / m\eqn{^2} of class \code{units}
#' 
#' @details
#' 
#' \deqn{S_\text{r} = \sigma \alpha_\text{l} T_\text{air} ^ 4}{S_r = \sigma \alpha_l T_air ^ 4 }
#' 
#' \tabular{lllll}{
#' \emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \eqn{\alpha_\text{l}}{\alpha_l} \tab \code{abs_l} \tab absortivity of longwave radiation (4 - 80 \eqn{\mu}m) \tab none \tab 0.97\cr
#' \eqn{T_\text{air}}{T_air} \tab \code{T_air} \tab air temperature \tab K \tab 298.15\cr
#' \eqn{\sigma} \tab \code{s} \tab Stephan-Boltzmann constant \tab W / (m\eqn{^2} K\eqn{^4}) \tab 5.67e-08
#' }
#' 
#' Note that leaf absortivity is the same value as leaf emissivity

.get_Sr <- function(pars) pars$s * pars$abs_l * pars$T_air ^ 4

#' H: sensible heat flux density (W / m^2)
#'
#' @inheritParams .get_Sr
#' @param T_leaf Leaf temperature in Kelvin
#' 
#' @return Value in W / m\eqn{^2} of class \code{units}
#' 
#' @details 
#' 
#' \deqn{H = P_\text{a} c_p g_\text{h} (T_\text{leaf} - T_\text{air})}{H = P_a c_p g_h * (T_leaf - T_air)}
#' 
#' \tabular{lllll}{
#' \emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \eqn{c_p} \tab \code{c_p} \tab heat capacity of air \tab J / (g K) \tab 1.01\cr
#' \eqn{g_\text{h}}{g_h} \tab \code{g_h} \tab boundary layer conductance to heat \tab m / s \tab \link[=.get_gh]{calculated}\cr
#' \eqn{P_\text{a}}{P_a} \tab \code{P_a} \tab density of dry air \tab g / m^3 \tab \link[=.get_Pa]{calculated}\cr
#' \eqn{T_\text{air}}{T_air} \tab \code{T_air} \tab air temperature \tab K \tab 298.15\cr
#' \eqn{T_\text{leaf}}{T_leaf} \tab \code{T_leaf} \tab leaf temperature \tab K \tab input
#' }
#' 
#' @seealso \code{\link{.get_gh}}, \code{\link{.get_Pa}} 

.get_H <- function(T_leaf, pars) {

  # Density of dry air
  P_a <- .get_Pa(T_leaf, pars)

  # Boundary layer conductance to heat
  g_h <- sum(.get_gh(T_leaf, "lower", pars), .get_gh(T_leaf, "upper", pars))

  H <- P_a * pars$c_p * g_h * (T_leaf - pars$T_air)
  H %<>% set_units("W / m ^ 2")
  H
  
}

#' P_a: density of dry air (g / m^3)
#'
#' @inheritParams .get_H
#' 
#' @return Value in g / m\eqn{^3} of class \code{units}
#' 
#' @details 
#' 
#' \deqn{P_\text{a} = P / (R_\text{air} (T_\text{leaf} - T_\text{air}) / 2)}{P_a = P / (R_air (T_leaf - T_air) / 2)}
#' 
#' \tabular{lllll}{
#' \emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \eqn{P} \tab \code{P} \tab atmospheric pressure \tab kPa \tab 101.3246\cr
#' \eqn{R_\text{air}}{R_air} \tab \code{R_air} \tab specific gas constant for dry air \tab J / (kg K) \tab 287.058\cr
#' \eqn{T_\text{air}}{T_air} \tab \code{T_air} \tab air temperature \tab K \tab 298.15\cr
#' \eqn{T_\text{leaf}}{T_leaf} \tab \code{T_leaf} \tab leaf temperature \tab K \tab input
#' }
#' 

.get_Pa <- function(T_leaf, pars) {
  P_a <- pars$P / (pars$R_air * (pars$T_air + T_leaf) / 2)
  P_a %<>% set_units("g / m^3")
}

#' g_h: boundary layer conductance to heat (m / s)
#'
#' @inheritParams .get_H
#' @param surface Leaf surface (lower or upper)
#' 
#' @return Value in m/s of class \code{units}
#' 
#' @details 
#' 
#' \deqn{g_\text{h} = D_\text{h} Nu / d}{g_h = D_h Nu / d}
#' 
#' \tabular{lllll}{
#' \emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \eqn{d} \tab \code{leafsize} \tab Leaf characteristic dimension in meters \tab m \tab 0.1\cr
#' \eqn{D_\text{h}}{D_h} \tab \code{D_h} \tab diffusion coefficient for heat in air \tab m\eqn{^2} / s \tab \link[=.get_Dx]{calculated}\cr
#' \eqn{Nu} \tab \code{Nu} \tab Nusselt number \tab none \tab \link[=.get_nu]{calculated}
#' }
#' 

.get_gh <- function(T_leaf, surface, pars) {

  surface %<>% match.arg(c("lower", "upper"))
  
  # Calculate diffusion coefficient to heat
  D_h <- .get_Dx(pars$D_h0, (pars$T_air + T_leaf) / 2, pars$eT, pars$P)

  # Calculate Nusselt numbers
  Nu <- .get_nu(T_leaf, surface, pars)

  D_h * Nu / pars$leafsize

}

#' D_x: Calculate diffusion coefficient for a given temperature and pressure
#'
#' @param D_0 Diffusion coefficient at 273.15 K (0 degree C) and 101.3246 kPa
#' @param Temp Temperature in Kelvin
#' @param eT Exponent for temperature dependence of diffusion
#' @param P Atmospheric pressure in kPa
#' 
#' @return Value in m\eqn{^2}/s of class \code{units}
#' 
#' @details 
#' 
#' \deqn{D = D_\text{0} (T / 273.15) ^ {eT} (101.3246 / P)}{D = D_0 [(T / 273.15) ^ eT] (101.3246 / P)}
#' 
#' @references 
#' 
#' Monteith JL, Unsworth MH. 2013. Principles of Environmental Physics. 4th edition. Academic Press, London.
#' 

.get_Dx <- function(D_0, Temp, eT, P) {

  # See Eq. 3.10 in Monteith & Unger ed. 4
  D_0 * 
    drop_units((set_units(Temp, "K") / set_units(273.15, "K"))) ^ drop_units(eT) * 
    drop_units((set_units(101.3246, "kPa") / set_units(P, "kPa")))

}

#' Gr: Grashof number
#'
#' @inheritParams .get_H
#' @inheritParams .get_gh
#' 
#' @return A unitless number of class \code{units} 
#' 
#' @details 
#' 
#' \deqn{Gr = t_\text{air} G d ^ 3 |T_\text{v,leaf} - T_\text{v,air}| / D_\text{m} ^ 2}{Gr = t_air G d ^ 3 abs(Tv_leaf - Tv_air) / D_m ^ 2}
#' 
#' \tabular{lllll}{
#' \emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \eqn{d} \tab \code{leafsize} \tab Leaf characteristic dimension in meters \tab m \tab 0.1\cr
#' \eqn{D_\text{m}}{D_m} \tab \code{D_m} \tab diffusion coefficient of momentum in air \tab m\eqn{^2} / s \tab \link[=.get_Dx]{calculated}\cr
#' \eqn{G} \tab \code{G} \tab gravitational acceleration \tab m / s\eqn{^2} \tab 9.8\cr
#' \eqn{t_\text{air}}{t_air} \tab \code{t_air} \tab coefficient of thermal expansion of air \tab 1 / K \tab 3.66e-3\cr
#' \eqn{T_\text{v,air}}{Tv_air} \tab \code{Tv_air} \tab virtual air temperature \tab K \tab \link[=.get_Tv]{calculated}\cr
#' \eqn{T_\text{v,leaf}}{Tv_leaf} \tab \code{Tv_leaf} \tab virtual leaf temperature \tab K \tab \link[=.get_Tv]{calculated}
#' }
#' 

.get_gr <- function(T_leaf, pars) {

  # Calculate virtual temperature
  Tv_leaf <- .get_Tv(T_leaf, .get_ps(T_leaf, pars$P), pars$P)
  Tv_air <-	.get_Tv(pars$T_air, pars$RH * .get_ps(pars$T_air, pars$P), pars$P)
  D_m <- .get_Dx(pars$D_m0, (pars$T_air + T_leaf) / 2, pars$eT, pars$P)
  Gr <- pars$t_air * pars$G * pars$leafsize ^ 3 * abs(Tv_leaf - Tv_air) / D_m ^ 2

  Gr

}

#' Calculate virtual temperature
#'
#' @inheritParams .get_Dx
#' @param p_air Saturation water vapour pressure of air in kPa
#' 
#' @return Value in K of class \code{units}
#' 
#' @details 
#' 
#' \deqn{T_\text{v} = T / [1 - 0.388 (p_\text{air} / P)]}{T_v = T / [1 - 0.388 (p_air / P)]}
#' 
#' \tabular{lllll}{
#' \emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \eqn{p_\text{air}}{p_air} \tab \code{p_air} \tab saturation water vapour pressure of air \tab kPa \tab \link[=.get_ps]{calculated}\cr
#' \eqn{P} \tab \code{P} \tab atmospheric pressure \tab kPa \tab 101.3246
#' }

.get_Tv <- function(Temp, p_air, P) {

  set_units(Temp, "K") / 
    (set_units(1) - (set_units(p_air, "kPa") / set_units(P, "kPa")) * 0.388)

}

#' Saturation water vapour pressure (kPa)
#'
#' @inheritParams .get_Dx
#'
#' @return Value in kPa of class \code{units}
#' 
#' @details 
#' 
#' Goff-Gratch equation (see http://cires1.colorado.edu/~voemel/vp.html) \cr
#' \cr
#' This assumes P = 1 atm = 101.3246 kPa, otherwise boiling temperature needs to change \cr
#' \cr
#' This returns p_s in hPa, which is converted to kPa: \cr
#' \cr
#' \deqn{p_\text{s} = 10 ^ (-7.90298 * (373.16 / Temp - 1) + 5.02808 * log_{10}(373.16 / Temp) - 1.3816e-7 * (10 ^ (11.344 * (1 - Temp / 373.16) - 1)) + 8.1328e-3 * (10 ^ (-3.49149 * (373.16 / Temp - 1)) - 1) + log_{10}(P))}{p_s = 10 ^ (-7.90298 * (373.16 / Temp - 1) + 5.02808 * log10(373.16 / Temp) - 1.3816e-7 * (10 ^ (11.344 * (1 - Temp / 373.16) - 1)) + 8.1328e-3 * (10 ^ (-3.49149 * (373.16 / Temp - 1)) - 1) + log10(P))}
#' 
#' @references \url{http://cires1.colorado.edu/~voemel/vp.html}
#' 

.get_ps <- function(Temp, P) {

  # Goff-Gratch equation (see http://cires1.colorado.edu/~voemel/vp.html)
  # This assumes P = 1 atm = 101.3246 kPa, otherwise boiling temperature needs to change
  # This returns p_s in hPa
  Temp %<>% set_units("K") %>% drop_units()
  P %<>% set_units("hPa") %>% drop_units()
  p_s <- 10 ^ (-7.90298 * (373.16 / Temp - 1) +
                 5.02808 * log10(373.16 / Temp) -
                 1.3816e-7 * (10 ^ (11.344 * (1 - Temp / 373.16) - 1)) +
                 8.1328e-3 * (10 ^ (-3.49149 * (373.16 / Temp - 1)) - 1) +
                 log10(P))

  # Convert to kPa
  p_s %<>% set_units("kPa")
  p_s

}

#' Re: Reynolds number
#'
#' @inheritParams .get_H
#' @inheritParams .get_gh
#'
#' @return A unitless number of class \code{units} 
#' 
#' @details 
#' 
#' \deqn{Re = u d / D_\text{m}}{Re = u d / D_m}
#' 
#' \tabular{lllll}{
#' \emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \eqn{d} \tab \code{leafsize} \tab Leaf characteristic dimension in meters \tab m \tab 0.1\cr
#' \eqn{D_\text{m}}{D_m} \tab \code{D_m} \tab diffusion coefficient of momentum in air \tab m\eqn{^2} / s \tab \link[=.get_Dx]{calculated}\cr
#' \eqn{u} \tab \code{wind} \tab windspeed \tab m / s \tab 2
#' }
#' 

.get_re <- function(T_leaf, pars) {

  D_m <- .get_Dx(pars$D_m0, (pars$T_air + T_leaf) / 2, pars$eT, pars$P)
  Re <- pars$wind * pars$leafsize / D_m

  Re

}

#' Nu: Nusselt number
#'
#' @inheritParams .get_H
#' @inheritParams .get_gh
#'
#' @return A unitless number of class \code{units} 
#' 
#' @details 
#' 
#' The Nusselt number depends on whether convection is free or forced. According to Nobel (2009, pg. 344), free convection dominates when \eqn{Ar < 0.1}, forced convection dominates when \eqn{Ar > 10}, and mixed convection when \eqn{0.1 > Ar > 10}. \eqn{Ar} is Archemides number \eqn{Ar = Gr / Re ^ 2}.\cr 
#' \cr
#' Free convection (\eqn{Ar < 0.1}) \cr
#' \cr
#' \deqn{Nu = a Re ^ b}
#' \cr
#' Forced convection (\eqn{Ar > 10}) \cr
#' \cr
#' \deqn{Nu = c Gr ^ d}
#' \cr
#' Mixed convection (\eqn{0.1 > Ar > 10}) \cr
#' \cr
#' \deqn{Nu = (a Re ^ b) ^ 3.5 + (c Gr ^ d) ^ 3.5) ^ (1 / 3.5)}
#' \cr
#' \tabular{lllll}{
#' \emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \eqn{a, b, c, d} \tab \code{a, b, c, d} \tab empirical coefficients \tab none \tab \link[=make_constants]{calculated}\cr
#' \eqn{Gr} \tab \code{Gr} \tab Grashof number \tab none \tab \link[=.get_gr]{calculated}\cr
#' \eqn{Re} \tab \code{Re} \tab Reynolds number \tab none \tab \link[=.get_re]{calculated}
#' }
#' 
#' @references Nobel PS. 2009. Physicochemical and Environmental Plant Physiology. 4th Edition. Academic Press.

.get_nu <- function(T_leaf, surface, pars) {

  surface %<>% match.arg(c("lower", "upper"))
  
  Gr <- .get_gr(T_leaf, pars)
  Re <- .get_re(T_leaf, pars)

  # Archemides number
  Ar <- Gr / Re ^ 2

  # Forced or free convection? Cutoffs based on Nobel (2009) pg.344
  if (Ar < set_units(0.1)) {
    type <- "forced"
    cons <- pars$nu_constant(Re, type, pars$T_air, T_leaf, surface)
    Nu <- cons$a * drop_units(Re) ^ cons$b
    Nu %<>% set_units()
    return(Nu)
  }

  if (Ar >= set_units(0.1) & Ar <= set_units(10)) {
    type <- "forced"
    cons <- pars$nu_constant(Re, type, pars$T_air, T_leaf, surface)
    Nu_forced <- cons$a * drop_units(Re) ^ cons$b

    type <- "free"
    cons <- pars$nu_constant(Re, type, pars$T_air, T_leaf, surface)
    Nu_free <- cons$a * drop_units(Gr) ^ cons$b

    Nu <- (Nu_forced ^ 3.5 + Nu_free ^ 3.5) ^ (1 / 3.5)
    Nu %<>% set_units()
    return(Nu)
  }

  if (Ar > set_units(10)) {
    type <- "free"
    cons <- pars$nu_constant(Re, type, pars$T_air, T_leaf, surface)
    Nu <- cons$a * drop_units(Gr) ^ cons$b
    Nu %<>% set_units()
    return(Nu)
  }

}

#' L: Latent heat flux density (W / m^2)
#'
#' @inheritParams .get_H
#' 
#' @return Value in W / m^2 of class \code{units}
#' 
#' @details 
#' 
#' \deqn{L = h_\text{vap} g_\text{tw} d_\text{wv}}{L = h_vap g_tw d_wv}
#' 
#' \bold{Heat of vaporization:} The heat of vaporization (\eqn{h_\text{vap}}{h_vap}) is a function of temperature. I used data from on temperature and \eqn{h_\text{vap}}{h_vap} from Nobel (2009, Appendix 1) to estimate a linear regression. See Examples. \cr
#' \cr
#' \bold{Total conductance to water vapor:} The total conductance to water vapor (\eqn{g_\text{tw}}{g_tw}) is the sum of the parallel lower (abaxial) and upper (adaxial) conductances:
#' 
#' \deqn{g_\text{tw} = g_\text{w,lower} + g_\text{w,upper}}{g_tw = gw_lower + gw_upper}
#' 
#' The conductance to water vapor on each surface is a function of parallel stomatal (\eqn{g_\text{sw}}{g_sw}) and cuticular (\eqn{g_\text{uw}}{g_uw}) conductances in series with the boundary layer conductance (\eqn{g_\text{bw}}{g_bw}). The stomatal, cuticular, and boundary layer conductance on the lower surface are:
#' 
#' \deqn{g_\text{sw,lower} = g_\text{sw} (1 - sr) R (T_\text{leaf} + T_\text{air}) / 2}{gsw_lower = g_sw (1 - sr) R (T_leaf + T_air) / 2}
#' \deqn{g_\text{uw_lower} = g_\text{uw} / 2 R (T_\text{leaf} + T_\text{air}) / 2}{guw_lower = g_uw / 2 R (T_leaf + T_air) / 2}
#' \cr
#' See \code{\link{.get_gbw}} for details on calculating boundary layer conductance. The equations for the upper surface are:
#' 
#' \deqn{g_\text{sw,upper} = g_\text{sw} sr R (T_\text{leaf} + T_\text{air}) / 2}{gsw_upper = g_sw sr R (T_leaf + T_air) / 2}
#' \deqn{g_\text{uw_upper} = g_\text{uw} / 2 R (T_\text{leaf} + T_\text{air}) / 2}{guw_upper = g_uw / 2 R (T_leaf + T_air) / 2}
#' \cr
#' Note that the stomatal and cuticular conductances are given in units of (mol H2O) / (m\eqn{^2} s) (see \code{\link{make_leafpar}}) and converted to m/s using the ideal gas law. The total leaf stomtal (\eqn{g_\text{sw}}{g_sw}) and cuticular (\eqn{g_\text{uw}}{g_uw}) conductances are partitioned across lower and upper surfaces. The stomatal conductance on each surface depends on stomatal ratio (sr); the cuticular conductance is assumed identical on both surfaces. 
#' 
#' \bold{Water vapour gradient:} The water vapour pressure differential from inside to outside of the leaf is the saturation water vapor pressure inside the leaf (\eqn{p_\text{leaf}}{p_leaf}) minus the water vapor pressure of the air (\eqn{p_\text{air}}{p_air}):
#' 
#' \deqn{d_\text{wv} = p_\text{leaf} / (R T_\text{leaf}) - RH p_\text{air} / (R T_\text{air})}{d_wv = p_leaf / (R T_leaf) - RH p_air / (R T_air)}
#' 
#' Note that water vapor pressure is converted from kPa to mol / m^3 using ideal gas law. \cr
#' 
#' \tabular{lllll}{
#' \emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \eqn{h_\text{vap}}{h_vap} \tab \code{h_vap} \tab latent heat of vaporization \tab J / mol \tab calculated \cr
#' \eqn{g_\text{sw}}{g_sw} \tab \code{g_sw} \tab stomatal conductance to H2O \tab (\eqn{\mu}mol H2O) / (m\eqn{^2} s Pa) \tab 5\cr
#' \eqn{g_\text{tw}}{g_tw} \tab \code{g_tw} \tab total conductance to H2O \tab (\eqn{\mu}mol H2O) / (m\eqn{^2} s Pa) \tab calculated\cr
#' \eqn{g_\text{uw}}{g_uw} \tab \code{g_uw} \tab cuticular conductance to H2O \tab (\eqn{\mu}mol H2O) / (m\eqn{^2} s Pa) \tab 0.1\cr
#' \eqn{p_\text{air}}{p_air} \tab \code{p_air} \tab saturation water vapour pressure of air \tab kPa \tab \link[=.get_ps]{calculated}\cr
#' \eqn{p_\text{leaf}}{p_leaf} \tab \code{p_leaf} \tab saturation water vapour pressure inside the leaf \tab kPa \tab \link[=.get_ps]{calculated}\cr
#' \eqn{R} \tab \code{R} \tab ideal gas constant \tab J / (mol K) \tab 8.3144598\cr
#' \eqn{\text{RH}}{RH} \tab \code{RH} \tab relative humidity \tab \% \tab 0.50\cr
#' \eqn{sr} \tab \code{sr} \tab stomatal ratio \tab none \tab 0 = logit(0.5)\cr
#' \eqn{T_\text{air}}{T_air} \tab \code{T_air} \tab air temperature \tab K \tab 298.15\cr
#' \eqn{T_\text{leaf}}{T_leaf} \tab \code{T_leaf} \tab leaf temperature \tab K \tab input
#' }
#'
#' @examples 
#' # Part 1. Heat of vaporization and temperature
#' ## data from Nobel (2009)
#' 
#' T_K <- 273.15 + c(0, 10, 20, 25, 30, 40, 50, 60)
#' h_vap <- 1e3 * c(45.06, 44.63, 44.21, 44.00, 
#'                  43.78, 43.35, 42.91, 42.47) # (in J / mol)
#'                  
#' fit <- lm(h_vap ~ T_K)
#' 
#' ## coefficients are 56847.68250 J / mol and 43.12514 J / (mol K)
#' 
#' coef(fit) 
#' 
#' T_leaf <- 298.15
#' h_vap <- set_units(56847.68250, "J / mol") - 
#'            set_units(43.12514, "J / mol / K") * set_units(T_leaf, "K")
#'
#' ## h_vap at 298.15 K is 43989.92 [J/mol]
#' 
#' set_units(h_vap, "J / mol")
#' 
#' # Part 2. Total conductance to water vapor
#' 
#' ## Hypostomatous leaf; default parameters
#' leaf_par <- make_leafpar(replace = list(sr = set_units(-Inf)))
#' enviro_par <- make_enviropar()
#' constants <- make_constants()
#' pars <- c(leaf_par, enviro_par, constants)
#' T_leaf <- set_units(300, "K")
#' 
#' ## Fixing boundary layer conductance rather than calculating
#' gbw_lower <- set_units(0.1, "m/s")
#' gbw_upper <- set_units(0.1, "m/s")
#' 
#' # Lower surface ----
#' ## Note that pars$sr is logit-transformed! Use stats::plogis() to convert to proportion.
#' gsw_lower <- set_units(pars$g_sw * (set_units(1) - stats::plogis(pars$sr)) * pars$R * 
#'                          ((T_leaf + pars$T_air) / 2), "m / s")
#' guw_lower <- set_units(pars$g_uw * 0.5 * pars$R * ((T_leaf + pars$T_air) / 2), "m / s")
#' gtw_lower <- 1 / (1 / (gsw_lower + guw_lower) + 1 / gbw_lower)
#' 
#' # Upper surface ----
#' gsw_upper <- set_units(pars$g_sw * stats::plogis(pars$sr) * pars$R * 
#'                          ((T_leaf + pars$T_air) / 2), "m / s")
#' guw_upper <- set_units(pars$g_uw * 0.5 * pars$R * ((T_leaf + pars$T_air) / 2), "m / s")
#' gtw_upper <- 1 / (1 / (gsw_upper + guw_upper) + 1 / gbw_upper)
#' 
#' ## Lower and upper surface are in parallel
#' g_tw <- gtw_lower + gtw_upper
#' 
#' # Part 3. Water vapour gradient: 
#' 
#' leaf_par <- make_leafpar()
#' enviro_par <- make_enviropar()
#' constants <- make_constants()
#' pars <- c(leaf_par, enviro_par, constants)
#' T_leaf <- set_units(300, "K")
#' T_air <- set_units(298.15, "K")
#' p_leaf <- set_units(35.31683, "kPa")
#' p_air <- set_units(31.65367, "kPa")
#' 
#' d_wv <- p_leaf / (pars$R * T_leaf) - pars$RH * p_air / (pars$R * T_air)
#' 
#' # Part 4. Putting it all together:
#' 
#' L <- set_units(h_vap * g_tw * d_wv, "W / m ^ 2")
#' L
#' 
#' @references Nobel PS. 2009. Physicochemical and Environmental Plant Physiology. 4th Edition. Academic Press.

.get_L <- function(T_leaf, pars) {

  # Equation from Foster and Smith 1986 seems to be off:
  # h_vap <- 4.504e4 - 41.94 * T_leaf
  # Instead, using regression based on data from Nobel (2009, 4th Ed, Appendix 1)
  # T_K <- 273.15 + c(0, 10, 20, 25, 30, 40, 50, 60)
  # h_vap <- 1e3 * c(45.06, 44.63, 44.21, 44, 43.78, 43.35, 42.91, 42.47) # (in J / mol)
  # fit <- lm(h_vap ~ T_K)
  h_vap <- set_units(56847.68250, "J / mol") - 
    set_units(43.12514, "J / mol / K") * set_units(T_leaf, "K")
  h_vap %<>% set_units("J / mol")
  
  # Lower surface ----
  gbw_lower <- .get_gbw(T_leaf, "lower", pars)
  
  # Convert stomatal and cuticular conductance from molar to 'engineering' units
  # See email from Tom Buckley (July 4, 2017)
  gsw_lower <- set_units(pars$g_sw * (set_units(1) - stats::plogis(pars$sr)) * pars$R * 
                           ((T_leaf + pars$T_air) / 2), "m / s")
  guw_lower <- set_units(pars$g_uw * 0.5 * pars$R * ((T_leaf + pars$T_air) / 2), "m / s")
  gtw_lower <- 1 / (1 / (gsw_lower + guw_lower) + 1 / gbw_lower)

  # Upper surface ----
  gbw_upper <- .get_gbw(T_leaf, "upper", pars)
  
  # Convert stomatal and cuticular conductance from molar to 'engineering' units
  # See email from Tom Buckley (July 4, 2017)
  gsw_upper <- set_units(pars$g_sw * stats::plogis(pars$sr) * pars$R * 
                           ((T_leaf + pars$T_air) / 2), "m / s")
  guw_upper <- set_units(pars$g_uw * 0.5 * pars$R * ((T_leaf + pars$T_air) / 2), "m / s")
  gtw_upper <- 1 / (1 / (gsw_upper + guw_upper) + 1 / gbw_upper)
  
  # Lower and upper surface are in parallel
  g_tw <- gtw_lower + gtw_upper
    
  # Water vapour differential converted from kPa to mol m ^ -3 using ideal gas law
  d_wv <- .get_ps(T_leaf, pars$P) / (pars$R * T_leaf) - 
    pars$RH * .get_ps(pars$T_air, pars$P) / (pars$R * pars$T_air)
  d_wv %<>% set_units("mol / m ^ 3")

  L <- h_vap * g_tw * d_wv
  L %<>% set_units("W / m ^ 2")
  L

}

#' g_bw: Boundary layer conductance to water vapour (m / s)
#'
#' @inheritParams .get_H
#' @inheritParams .get_gh
#' 
#' @return Value in m / s of class \code{units}
#' 
#' @details 
#' 
#' \deqn{g_\text{bw} = D_\text{w} Sh / d}{g_bw = D_w Sh / d}
#' 
#' \tabular{lllll}{
#' \emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \eqn{d} \tab \code{leafsize} \tab Leaf characteristic dimension in meters \tab m \tab 0.1\cr
#' \eqn{D_\text{w}}{D_w} \tab \code{D_w} \tab diffusion coefficient for water vapour \tab m\eqn{^2} / s \tab \link[=.get_Dx]{calculated}\cr
#' \eqn{Sh} \tab \code{Sh} \tab Sherwood number \tab none \tab \link[=.get_sh]{calculated}
#' }
#' 
.get_gbw <- function(T_leaf, surface, pars) {

  surface %<>% match.arg(c("lower", "upper"))
  
  D_w <- .get_Dx(pars$D_w0, (pars$T_air + T_leaf) / 2, pars$eT, pars$P)

  # Calculate Sherwood numbers
  Sh <- .get_sh(T_leaf, surface, pars)

  D_w * Sh / pars$leafsize

}

#' Sh: Sherwood number
#'
#' @inheritParams .get_H
#' @inheritParams .get_gh
#'
#' @return A unitless number of class \code{units} 
#' 
#' @details 
#' 
#' The Sherwood number depends on whether convection is free or forced. According to Nobel (2009, pg. 344), free convection dominates when \eqn{Ar < 0.1}, forced convection dominates when \eqn{Ar > 10}, and mixed convection when \eqn{0.1 > Ar > 10}. \eqn{Ar} is Archemides number \eqn{Ar = Gr / Re ^ 2}.\cr 
#' \cr
#' Free convection (\eqn{Ar < 0.1}) \cr
#' \cr
#' \deqn{Sh = a Re ^ b}
#' \cr
#' Forced convection (\eqn{Ar > 10}) \cr
#' \cr
#' \deqn{Sh = c Gr ^ d}
#' \cr
#' Mixed convection (\eqn{0.1 > Ar > 10}) \cr
#' \cr
#' \deqn{Sh = (a Re ^ b) ^ 3.5 + (c Gr ^ d) ^ 3.5) ^ (1 / 3.5)}
#' \cr
#' \tabular{lllll}{
#' \emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \eqn{a, b, c, d} \tab \code{a, b, c, d} \tab empirical coefficients \tab none \tab \link[=make_constants]{calculated}\cr
#' \eqn{Gr} \tab \code{Gr} \tab Grashof number \tab none \tab \link[=.get_gr]{calculated}\cr
#' \eqn{Re} \tab \code{Re} \tab Reynolds number \tab none \tab \link[=.get_re]{calculated}
#' }
#' 
#' @references Nobel PS. 2009. Physicochemical and Environmental Plant Physiology. 4th Edition. Academic Press.
#' 

.get_sh <- function(T_leaf, surface, pars) {

  surface %<>% match.arg(c("lower", "upper"))
  
  Gr <- .get_gr(T_leaf, pars)
  Re <- .get_re(T_leaf, pars)

  # Archemides number
  Ar <- Gr / Re ^ 2

  D_h <- .get_Dx(pars$D_h0, (pars$T_air + T_leaf) / 2, pars$eT, pars$P)
  D_w <- .get_Dx(pars$D_w0, (pars$T_air + T_leaf) / 2, pars$eT, pars$P)

  # Forced or free convection? Cutoffs based on Nobel (2009) pg.344
  if (Ar < set_units(0.1)) {
    type <- "forced"
    cons <- pars$nu_constant(Re, type, pars$T_air, T_leaf, surface)
    Nu <- cons$a * drop_units(Re) ^ cons$b
    Sh <- Nu * drop_units(D_h / D_w) ^ drop_units(pars$sh_constant(type))
    Nu %<>% set_units()
    return(Sh)
  }

  if (Ar >= set_units(0.1) & Ar <= set_units(10)) {
    type <- "forced"
    cons <- pars$nu_constant(Re, type, pars$T_air, T_leaf, surface)
    Nu_forced <- cons$a * drop_units(Re) ^ cons$b
    Sh_forced <- Nu_forced * drop_units(D_h / D_w) ^ drop_units(pars$sh_constant(type))

    type <- "free"
    cons <- pars$nu_constant(Re, type, pars$T_air, T_leaf, surface)
    Nu_free <- cons$a * drop_units(Gr) ^ cons$b
    Sh_free <- Nu_free * drop_units(D_h / D_w) ^ drop_units(pars$sh_constant(type))

    warning("check on exponents in mixed convection Sherwood equation in .get_sh")
    Sh <- (Sh_forced ^ 3.5 + Sh_free ^ 3.5) ^ (1 / 3.5)
    Sh %<>% set_units()
    return(Sh)
  }

  if (Ar > set_units(10)) {
    type <- "free"
    cons <- pars$nu_constant(Re, type, pars$T_air, T_leaf, surface)
    Nu <- cons$a * drop_units(Gr) ^ cons$b
    Sh <- Nu * drop_units(D_h / D_w) ^ drop_units(pars$sh_constant(type))
    Sh %<>% set_units()
    return(Sh)
  }

}
