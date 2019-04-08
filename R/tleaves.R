#' \code{tleaves}: find leaf temperatures for multiple parameter sets
#' 
#' @param leaf_par A list of leaf parameters. This can be generated using the \code{make_leafpar} function.
#' 
#' @param enviro_par A list of environmental parameters. This can be generated using the \code{make_enviropar} function.
#' 
#' @param constants A list of physical constants. This can be generated using the \code{make_constants} function.
#' 
#' @param progress Logical. Should a progress bar be displayed?
#' 
#' @param quiet Logical. Should messages be displayed?
#'
#' @param set_units Logical. Should \code{units} be set? The function is faster when FALSE, but input must be in correct units or else results will be incorrect without any warning.
#' 
#' @param parallel Logical. Should parallel processing be used via \code{\link[furrr]{future_map}}?
#' 
#' @return 
#' 
#' \code{tleaves}: \cr
#' \cr
#' A tibble with the following \code{units} columns \cr
#'
#' \tabular{ll}{
#' \bold{Input:} \tab \cr
#' \code{abs_l} \tab Absorbtivity of longwave radiation (unitless) \cr
#' \code{abs_s} \tab Absorbtivity of shortwave radiation (unitless) \cr
#' \code{g_sw} \tab Stomatal conductance to H2O (\eqn{\mu}mol H2O / (m^2 s Pa)) \cr
#' \code{g_uw} \tab Cuticular conductance to H2O (\eqn{\mu}mol H2O / (m^2 s Pa)) \cr
#' \code{leafsize} Leaf characteristic dimension \tab (m) \cr
#' \code{logit_sr} \tab Stomatal ratio (logit transformed; unitless) \cr
#' \code{P} \tab Atmospheric pressure (kPa) \cr
#' \code{RH} \tab Relative humidity (unitless) \cr
#' \code{S_lw} \tab incident long-wave radiation flux density (W / m^2) \cr
#' \code{S_sw} \tab incident short-wave (solar) radiation flux density (W / m^2) \cr
#' \code{T_air} \tab Air temperature (K) \cr
#' \code{wind} \tab Wind speed (m / s) \cr
#' \bold{Output:} \tab \cr
#' \code{T_leaf} \tab Equilibrium leaf tempearture (K) \cr
#' \code{value} \tab Leaf energy balance (W / m^2) at \code{tleaf} \cr
#' \code{convergence} \tab Convergence code (0 = converged) \cr
#' \code{R_abs} \tab Total absorbed radiation (W / m^2; see \code{\link{.get_Rabs}}) \cr
#' \code{S_r} \tab Longwave re-radiation (W / m^2; see \code{\link{.get_Sr}}) \cr
#' \code{H} \tab Sensible heat flux density (W / m^2; see \code{\link{.get_H}}) \cr
#' \code{L} \tab Latent heat flux density (W / m^2; see \code{\link{.get_L}}) \cr
#' \code{E} \tab Evapotranspiration (mol H2O/ (m^2 s))
#' }
#' 
#' \code{tleaf}: \cr
#' \cr
#' A data.frame with the following numeric columns: \cr
#' 
#' \tabular{ll}{
#' \code{T_leaf} \tab Equilibrium leaf tempearture (K) \cr
#' \code{value} \tab Leaf energy balance (W / m^2) at \code{tleaf} \cr
#' \code{convergence} \tab Convergence code (0 = converged) \cr
#' \code{R_abs} \tab Total absorbed radiation (W / m^2; see \code{\link{.get_Rabs}}) \cr
#' \code{S_r} \tab Longwave re-radiation (W / m^2; see \code{\link{.get_Sr}}) \cr
#' \code{H} \tab Sensible heat flux density (W / m^2; see \code{\link{.get_H}}) \cr
#' \code{L}\tab Latent heat flux density (W / m^2; see \code{\link{.get_L}}) \cr
#' \code{E} \tab Evapotranspiration (mol H2O/ (m^2 s))
#' }
#' 
#' @examples 
#' # tleaf for single parameter set:
#' 
#' leaf_par <- make_leafpar()
#' enviro_par <- make_enviropar()
#' constants <- make_constants()
#' tleaf(leaf_par, enviro_par, constants)
#' 
#' # tleaves for multiple parameter set:
#' 
#' enviro_par <- make_enviropar(
#'   replace = list(
#'     T_air = set_units(c(293.15, 298.15), "K")
#'   )
#' )
#' tleaves(leaf_par, enviro_par, constants)
#' 
#' @export
#'

tleaves <- function(leaf_par, enviro_par, constants, progress = TRUE, 
                    quiet = FALSE, set_units = TRUE, parallel = FALSE) {
  
  if (set_units) {
    
  }
  
  pars <- c(leaf_par, enviro_par)
  par_units <- purrr::map(pars, units) %>%
    magrittr::set_names(names(pars))
  
  pars %<>% make_parameter_sets(par_units)
  
  soln <- find_tleaves(pars, constants, progress, quiet, set_units = FALSE,
                       parallel)
  
  pars %<>% purrr::map_dfr(purrr::flatten_dfr)
  
  colnames(pars) %>%
    glue::glue("units(pars${x}) <<- par_units${x}", x = .) %>%
    parse(text = .) %>%
    eval()
  
  pars %<>% dplyr::bind_cols(soln)
  units(pars$T_leaf) <- "K"
  units(pars$E) <- "mol/m^2/s"
  units(pars$R_abs) <- "W/m^2"
  units(pars$S_r) <- "W/m^2"
  units(pars$H) <- "W/m^2"
  units(pars$L) <- "W/m^2"

  pars
  
}

#' \code{tleaf}: find leaf temperatures for a single parameter set
#' @rdname tleaves
#' @export

tleaf <- function(leaf_par, enviro_par, constants, quiet = FALSE, 
                  unitless = FALSE) {
  
  leaf_par %<>% leaf_par()
  enviro_par %<>% enviro_par()
  constants %<>% constants()
  
  soln <- find_tleaf(leaf_par, enviro_par, constants, quiet, unitless)
  
  # Check results -----
  if (soln$convergence == 1) {
    "stats::uniroot did not converge, NA returned. Inspect parameters carefully." %>%
      crayon::red() %>%
      message()
  }
  
  # Energy balance components -----
  components <- suppressWarnings(
    if (is.na(soln$T_leaf)) {
      list(R_abs = NA, S_r = NA, H = NA, L = NA, E = NA)
    } else {
      soln %>%
      dplyr::pull("T_leaf") %>%
      set_units("K") %>%
      energy_balance(leaf_par = leaf_par, enviro_par = enviro_par, 
                     constants = constants, quiet = TRUE, components = TRUE,
                     unitless = FALSE) %>%
      magrittr::use_series("components")
    }
  )
  
  soln %<>% dplyr::bind_cols(components)
  units(soln$T_leaf) <- "K"
  if (is.na(soln$T_leaf)) {
    soln %<>% dplyr::bind_cols(data.frame(Ar = NA, Gr = NA, Re = NA))
  } else {
    soln %<>% dplyr::bind_cols(Ar(soln$T_leaf, c(leaf_par, enviro_par, constants)))
  }
  
  # Return -----
  soln
  
}

#' Calculate leaf energy balance
#'
#' @inheritParams tleaves
#' 
#' @param tleaf Leaf temperature in Kelvin. If input is numeric, it will be automatically converted to \code{units}.
#' 
#' @param quiet Logical. Should a message appear about conversion from \code{numeric} to \code{units}? Useful for finding leaf temperature that balances heat transfer using \code{\link[stats]{uniroot}}. 
#'
#' @param components Logical. Should leaf energy components be returned? Transpiration (in mol / (m^2 s)) also returned.
#' 
#' @param check Logical. Should all parameter sets be checked? TRUE is safer, but FALSE is faster.
#' 
#' @return A numeric value in W / m^2. Optionally, a named list of energy balance components in W / m^2 and transpiration in mol / (m^2 s).
#' 
#' @export
#'

energy_balance <- function(tleaf, leaf_par, enviro_par, constants, 
                           quiet = FALSE, components = FALSE, unitless = FALSE,
                           check = TRUE) {

  # Checks -----
  if (check) {
    leaf_par %<>% leaf_par()
    enviro_par %<>% enviro_par()
    constants %<>% constants()
  }
  stopifnot(length(quiet) == 1L & is.logical(quiet))
  stopifnot(length(components) == 1L & is.logical(components))
  stopifnot(length(unitless) == 1L & is.logical(unitless))
  
  ## Convert tleaf to units and message
  if (!is(tleaf, "units") & !unitless) {
    if (!quiet) {
      glue::glue("tleaf converted from numeric to {X} K", X = tleaf) %>%
        message()
    }
    tleaf %<>% set_units("K")
  }
  
  pars <- c(leaf_par, enviro_par, constants)

  if (unitless) pars %<>% purrr::map_if(function(x) is(x, "units"), drop_units)
  
  # R_abs: total absorbed radiation (W m^-2) -----
  R_abs <- .get_Rabs(pars, unitless)

  # S_r: longwave re-radiation (W m^-2) -----
  S_r <- .get_Sr(tleaf, pars)

  # H: sensible heat flux density (W m^-2) -----
  H <- .get_H(tleaf, pars, unitless)

  # L: latent heat flux density (W m^-2) -----
  L <- .get_L(tleaf, pars, unitless)
  
  # Return -----
  if (components) {
    
    # E: transpiration (mol / (m^2 s))
    pars %<>% purrr::map_if(function(x) is(x, "units"), drop_units)
    if (is(tleaf, "units")) tleaf %<>% drop_units()

    E <- .get_gtw(tleaf, pars, unitless = TRUE) * 
      .get_dwv(tleaf, pars, unitless = TRUE)
    E %<>% set_units("mol/m^2/s")
    ret <- list(
      energy_balance = R_abs - (S_r + H + L),
      components = list(R_abs = R_abs, S_r = S_r, H = H, L = L, E = E)
      )
    return(ret)
    
  }
  
  R_abs - (S_r + H + L)

}

#' R_abs: total absorbed radiation (W / m^2)
#'
#' @param pars Concatenated parameters (\code{leaf_par}, \code{enviro_par}, and \code{constants})
#' @inheritParams tleaves
#' 
#' @return Value in W / m\eqn{^2} of class \code{units}
#' 
#' @details
#' 
#' The following treatment follows Okajima et al. (2012):
#' 
#' \deqn{R_\mathrm{abs} = \alpha_\mathrm{s} (1 + r) S_\mathrm{sw} + \alpha_\mathrm{l} \sigma (T_\mathrm{sky} ^ 4 + T_\mathrm{air} ^ 4)}{R_abs = \alpha_s (1 + r) S_sw + \alpha_l \sigma (T_sky ^ 4 + T_air ^ 4)}
#' 
#' The incidient longwave (aka thermal infrared) radiation is modeled from sky and air temperature \eqn{\sigma (T_\mathrm{sky} ^ 4 + T_\mathrm{air} ^ 4)}{\sigma (T_sky ^ 4 + T_air ^ 4)} where \eqn{T_\mathrm{sky}}{T_sky} is function of the air temperature and incoming solar shortwave radiation:
#' 
#' \deqn{T_\mathrm{sky} = T_\mathrm{air} - 20 S_\mathrm{sw} / 1000}{T_sky = T_air - 20 S_sw / 1000}
#' 
#' \tabular{lllll}{
#' \emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \eqn{\alpha_\mathrm{s}}{\alpha_s} \tab \code{abs_s} \tab absorbtivity of shortwave radiation (0.3 - 4 \eqn{\mu}m) \tab none \tab 0.80\cr
#' \eqn{\alpha_\mathrm{l}}{\alpha_l} \tab \code{abs_l} \tab absorbtivity of longwave radiation (4 - 80 \eqn{\mu}m) \tab none \tab 0.97\cr
#' \eqn{r} \tab \code{r} \tab reflectance for shortwave irradiance (albedo) \tab none \tab 0.2 \cr
#' \eqn{\sigma} \tab \code{s} \tab Stephan-Boltzmann constant \tab W / (m\eqn{^2} K\eqn{^4}) \tab 5.67e-08 \cr
#' \eqn{S_\mathrm{sw}}{S_sw} \tab \code{S_sw} \tab incident short-wave (solar) radiation flux density \tab W / m\eqn{^2} \tab 1000 \cr
#' \eqn{S_\mathrm{lw}}{S_lw} \tab \code{S_lw} \tab incident long-wave radiation flux density \tab W / m\eqn{^2} \tab calculated \cr
#' \eqn{T_\mathrm{air}}{T_air} \tab \code{T_air} \tab air temperature \tab K \tab 298.15 \cr
#' \eqn{T_\mathrm{sky}}{T_sky} \tab \code{T_sky} \tab sky temperature \tab K \tab calculated
#' }
#' 
#' @references 
#' 
#' Okajima Y, H Taneda, K Noguchi, I Terashima. 2012. Optimum leaf size predicted by a novel leaf energy balance model incorporating dependencies of photosynthesis on light and temperature. Ecological Research 27: 333-46.
#' 

.get_Rabs <- function(pars, unitless) {
  
  if (unitless) {
    T_sky <- pars$T_air - 20 * pars$S_sw / 1000
    R_abs <- pars$abs_s * (1 + pars$r) * pars$S_sw + pars$abs_l * pars$s * (T_sky ^ 4 + pars$T_air ^ 4)
    
  } else {
    T_sky <- pars$T_air - set_units(20, "K") * pars$S_sw / set_units(1000, "W/m^2")
    R_abs <- pars$abs_s * (set_units(1) + pars$r) * pars$S_sw + pars$abs_l * pars$s * (T_sky ^ 4 + pars$T_air ^ 4)
    R_abs %<>% set_units("W/m^2")
  }

  R_abs
  
}

#' S_r: longwave re-radiation (W / m^2)
#'
#' @inheritParams .get_Rabs
#' @param T_leaf Leaf temperature in Kelvin
#'
#' @return Value in W / m\eqn{^2} of class \code{units}
#' 
#' @details
#' 
#' \deqn{S_\mathrm{r} = 2 \sigma \alpha_\mathrm{l} T_\mathrm{air} ^ 4}{S_r = 2 \sigma \alpha_l T_air ^ 4 }
#' 
#' The factor of 2 accounts for re-radiation from both leaf surfaces (Foster and Smith 1986). \cr
#' \cr
#' \tabular{lllll}{
#' \emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \eqn{\alpha_\mathrm{l}}{\alpha_l} \tab \code{abs_l} \tab absorbtivity of longwave radiation (4 - 80 \eqn{\mu}m) \tab none \tab 0.97\cr
#' \eqn{T_\mathrm{air}}{T_air} \tab \code{T_air} \tab air temperature \tab K \tab 298.15\cr
#' \eqn{\sigma} \tab \code{s} \tab Stephan-Boltzmann constant \tab W / (m\eqn{^2} K\eqn{^4}) \tab 5.67e-08
#' }
#' 
#' Note that leaf absorbtivity is the same value as leaf emissivity
#' 
#' @references 
#' 
#' Foster JR, Smith WK. 1986. Influence of stomatal distribution on transpiration in low-wind environments. Plant, Cell \& Environment 9: 751-9.
#' 

.get_Sr <- function(T_leaf, pars) 2 * pars$s * pars$abs_l * T_leaf ^ 4

#' H: sensible heat flux density (W / m^2)
#'
#' @inheritParams tleaves
#' @inheritParams .get_Sr
#' 
#' @return Value in W / m\eqn{^2} of class \code{units}
#' 
#' @details 
#' 
#' \deqn{H = P_\mathrm{a} c_p g_\mathrm{h} (T_\mathrm{leaf} - T_\mathrm{air})}{H = P_a c_p g_h * (T_leaf - T_air)}
#' 
#' \tabular{lllll}{
#' \emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \eqn{c_p} \tab \code{c_p} \tab heat capacity of air \tab J / (g K) \tab 1.01\cr
#' \eqn{g_\mathrm{h}}{g_h} \tab \code{g_h} \tab boundary layer conductance to heat \tab m / s \tab \link[=.get_gh]{calculated}\cr
#' \eqn{P_\mathrm{a}}{P_a} \tab \code{P_a} \tab density of dry air \tab g / m^3 \tab \link[=.get_Pa]{calculated}\cr
#' \eqn{T_\mathrm{air}}{T_air} \tab \code{T_air} \tab air temperature \tab K \tab 298.15\cr
#' \eqn{T_\mathrm{leaf}}{T_leaf} \tab \code{T_leaf} \tab leaf temperature \tab K \tab input
#' }
#' 
#' @seealso \code{\link{.get_gh}}, \code{\link{.get_Pa}} 

.get_H <- function(T_leaf, pars, unitless) {

  # Density of dry air
  P_a <- .get_Pa(T_leaf, pars, unitless)

  # Boundary layer conductance to heat
  g_h <- sum(.get_gh(T_leaf, "lower", pars, unitless), 
             .get_gh(T_leaf, "upper", pars, unitless))

  H <- P_a * pars$c_p * g_h * (T_leaf - pars$T_air)
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
#' \deqn{P_\mathrm{a} = P / (R_\mathrm{air} (T_\mathrm{leaf} - T_\mathrm{air}) / 2)}{P_a = P / (R_air (T_leaf - T_air) / 2)}
#' 
#' \tabular{lllll}{
#' \emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \eqn{P} \tab \code{P} \tab atmospheric pressure \tab kPa \tab 101.3246\cr
#' \eqn{R_\mathrm{air}}{R_air} \tab \code{R_air} \tab specific gas constant for dry air \tab J / (kg K) \tab 287.058\cr
#' \eqn{T_\mathrm{air}}{T_air} \tab \code{T_air} \tab air temperature \tab K \tab 298.15\cr
#' \eqn{T_\mathrm{leaf}}{T_leaf} \tab \code{T_leaf} \tab leaf temperature \tab K \tab input
#' }
#' 

.get_Pa <- function(T_leaf, pars, unitless) {
  
  P_a <- pars$P / (pars$R_air * (pars$T_air + T_leaf) / 2)
  
  if (unitless) {
    P_a %<>% magrittr::multiply_by(1e6)
  } else {
    P_a %<>% set_units("g / m^3")
  }
  
  P_a
  
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
#' \deqn{g_\mathrm{h} = D_\mathrm{h} Nu / d}{g_h = D_h Nu / d}
#' 
#' \tabular{lllll}{
#' \emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \eqn{d} \tab \code{leafsize} \tab Leaf characteristic dimension in meters \tab m \tab 0.1\cr
#' \eqn{D_\mathrm{h}}{D_h} \tab \code{D_h} \tab diffusion coefficient for heat in air \tab m\eqn{^2} / s \tab \link[=.get_Dx]{calculated}\cr
#' \eqn{Nu} \tab \code{Nu} \tab Nusselt number \tab none \tab \link[=.get_nu]{calculated}
#' }
#' 

.get_gh <- function(T_leaf, surface, pars, unitless) {

  surface %<>% match.arg(c("lower", "upper"))
  
  # Calculate diffusion coefficient to heat
  D_h <- .get_Dx(pars$D_h0, (pars$T_air + T_leaf) / 2, pars$eT, pars$P, unitless)

  # Calculate Nusselt numbers
  Nu <- .get_nu(T_leaf, surface, pars, unitless)

  D_h * Nu / pars$leafsize

}

#' D_x: Calculate diffusion coefficient for a given temperature and pressure
#'
#' @param D_0 Diffusion coefficient at 273.15 K (0 °C) and 101.3246 kPa
#' @param Temp Temperature in Kelvin
#' @param eT Exponent for temperature dependence of diffusion
#' @param P Atmospheric pressure in kPa
#' @inheritParams tleaves
#' 
#' @return Value in m\eqn{^2}/s of class \code{units}
#' 
#' @details 
#' 
#' \deqn{D = D_\mathrm{0} (T / 273.15) ^ {eT} (101.3246 / P)}{D = D_0 [(T / 273.15) ^ eT] (101.3246 / P)}
#' \cr
#' According to Montieth & Unger (2013), eT is generally between 1.5 and 2. Their data in Appendix 3 indicate \eqn{eT = 1.75} is reasonble for environmental physics.
#' 
#' @references 
#' 
#' Monteith JL, Unsworth MH. 2013. Principles of Environmental Physics. 4th edition. Academic Press, London.
#' 

.get_Dx <- function(D_0, Temp, eT, P, unitless) {

  # See Eq. 3.10 in Monteith & Unger ed. 4
  if (unitless) {
    
    Dx <- D_0 * (Temp / 273.15) ^ eT * (101.3246 / P)
    
  } else {
    
    Dx <- D_0 * 
      drop_units((set_units(Temp, "K") / set_units(273.15, "K"))) ^ drop_units(eT) * 
      drop_units((set_units(101.3246, "kPa") / set_units(P, "kPa")))
    
  }
  
  Dx
  
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
#' \deqn{Gr = t_\mathrm{air} G d ^ 3 |T_\mathrm{v,leaf} - T_\mathrm{v,air}| / D_\mathrm{m} ^ 2}{Gr = t_air G d ^ 3 abs(Tv_leaf - Tv_air) / D_m ^ 2}
#' 
#' \tabular{lllll}{
#' \emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \eqn{d} \tab \code{leafsize} \tab Leaf characteristic dimension in meters \tab m \tab 0.1\cr
#' \eqn{D_\mathrm{m}}{D_m} \tab \code{D_m} \tab diffusion coefficient of momentum in air \tab m\eqn{^2} / s \tab \link[=.get_Dx]{calculated}\cr
#' \eqn{G} \tab \code{G} \tab gravitational acceleration \tab m / s\eqn{^2} \tab 9.8\cr
#' \eqn{t_\mathrm{air}}{t_air} \tab \code{t_air} \tab coefficient of thermal expansion of air \tab 1 / K \tab 1 / Temp \cr
#' \eqn{T_\mathrm{v,air}}{Tv_air} \tab \code{Tv_air} \tab virtual air temperature \tab K \tab \link[=.get_Tv]{calculated}\cr
#' \eqn{T_\mathrm{v,leaf}}{Tv_leaf} \tab \code{Tv_leaf} \tab virtual leaf temperature \tab K \tab \link[=.get_Tv]{calculated}
#' }
#' 

.get_gr <- function(T_leaf, pars, unitless) {

  # Calculate virtual temperature
  # Assumes inside of leaf is 100% RH
  Tv_leaf <- .get_Tv(T_leaf, .get_ps(T_leaf, pars$P, unitless), pars$P, 
                     pars$epsilon, unitless)
  Tv_air <-	.get_Tv(pars$T_air, pars$RH * .get_ps(pars$T_air, pars$P, unitless), pars$P,
                    pars$epsilon, unitless)
  D_m <- .get_Dx(pars$D_m0, (pars$T_air + T_leaf) / 2, pars$eT, pars$P, unitless)
  if (unitless) {
    
    Gr <- (1 / pars$T_air) * pars$G * pars$leafsize ^ 3 * 
      abs(Tv_leaf - Tv_air) / D_m ^ 2
    
  } else {
    
    Gr <- (set_units(1) / pars$T_air) * pars$G * pars$leafsize ^ 3 * 
      abs(Tv_leaf - Tv_air) / D_m ^ 2
    
  }
  
  Gr

}

#' Calculate virtual temperature
#'
#' @inheritParams .get_Dx
#' @param p water vapour pressure in kPa
#' @param epsilon ratio of water to air molar masses (unitless)
#' 
#' @return Value in K of class \code{units}
#' 
#' @details 
#' 
#' \deqn{T_\mathrm{v} = T / [1 - (1 - \epsilon) (p / P)]}{T_v = T / [1 - (1 - epsilon) (p / P)]}
#' 
#' Eq. 2.35 in Monteith & Unsworth (2013) \cr
#' \cr
#' \tabular{lllll}{
#' \emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \eqn{\epsilon} \tab \code{epsilon} \tab ratio of water to air molar masses \tab unitless \tab 0.622 \cr
#' \eqn{p} \tab \code{p} \tab water vapour pressure \tab kPa \tab \link[=.get_ps]{calculated}\cr
#' \eqn{P} \tab \code{P} \tab atmospheric pressure \tab kPa \tab 101.3246
#' }
#' 
#' @references 
#' 
#' Monteith JL, Unsworth MH. 2013. Principles of Environmental Physics. 4th edition. Academic Press, London.
#' 

.get_Tv <- function(Temp, p, P, epsilon, unitless) {

  if (unitless) {
    
    Tv <- Temp / (1 - (1 - epsilon) * (p / P))
    
  } else {
    
    Tv <- set_units(Temp, "K") / 
      (set_units(1) - (set_units(1) - epsilon) * 
         (set_units(p, "kPa") / set_units(P, "kPa")))
    
  }
  
  Tv
  
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
#' This equation assumes P = 1 atm = 101.3246 kPa, otherwise boiling temperature needs to change \cr
#' 
#' @references \url{http://cires1.colorado.edu/~voemel/vp.html}
#' 

.get_ps <- function(Temp, P, unitless) {

  # Goff-Gratch equation (see http://cires1.colorado.edu/~voemel/vp.html)
  # This assumes P = 1 atm = 101.3246 kPa, otherwise boiling temperature needs to change
  # This returns p_s in hPa
  if (unitless) {
    P %<>% magrittr::multiply_by(10)
  } else {
    Temp %<>% set_units("K") %>% drop_units()
    P %<>% set_units("hPa") %>% drop_units()
  }
  
  p_s <- 10 ^ (-7.90298 * (373.16 / Temp - 1) +
                 5.02808 * log10(373.16 / Temp) -
                 1.3816e-7 * (10 ^ (11.344 * (1 - Temp / 373.16) - 1)) +
                 8.1328e-3 * (10 ^ (-3.49149 * (373.16 / Temp - 1)) - 1) +
                 log10(P))

  # Convert to kPa
  if (unitless) {
    p_s %<>% magrittr::multiply_by(0.1)
  } else {
    p_s %<>% 
      set_units("hPa") %>%
      set_units("kPa")
  }
  
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
#' \deqn{Re = u d / D_\mathrm{m}}{Re = u d / D_m}
#' 
#' \tabular{lllll}{
#' \emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \eqn{d} \tab \code{leafsize} \tab Leaf characteristic dimension in meters \tab m \tab 0.1\cr
#' \eqn{D_\mathrm{m}}{D_m} \tab \code{D_m} \tab diffusion coefficient of momentum in air \tab m\eqn{^2} / s \tab \link[=.get_Dx]{calculated}\cr
#' \eqn{u} \tab \code{wind} \tab windspeed \tab m / s \tab 2
#' }
#' 

.get_re <- function(T_leaf, pars, unitless) {

  D_m <- .get_Dx(pars$D_m0, (pars$T_air + T_leaf) / 2, pars$eT, pars$P, unitless)
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
#' The Nusselt number depends on a combination how much free or forced convection predominates. For mixed convection: \cr 
#' \cr
#' \deqn{Nu = (a Re ^ b) ^ {3.5} + (c Gr ^ d) ^ {3.5}) ^ {1 / 3.5}}{Nu = (a Re ^ b) ^ 3.5 + (c Gr ^ d) ^ 3.5) ^ (1 / 3.5)}
#' \cr
#' \tabular{lllll}{
#' \emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \eqn{a, b, c, d} \tab \code{a, b, c, d} \tab empirical coefficients \tab none \tab \link[=make_constants]{calculated}\cr
#' \eqn{Gr} \tab \code{Gr} \tab Grashof number \tab none \tab \link[=.get_gr]{calculated}\cr
#' \eqn{Re} \tab \code{Re} \tab Reynolds number \tab none \tab \link[=.get_re]{calculated}
#' }
#' 

.get_nu <- function(T_leaf, surface, pars, unitless) {

  surface %<>% match.arg(c("lower", "upper"))
  
  Gr <- .get_gr(T_leaf, pars, unitless)
  Re <- .get_re(T_leaf, pars, unitless)

  # Archemides number
  # Ar <- Gr / Re ^ 2
  
  cons <- pars$nu_constant(Re, "forced", pars$T_air, T_leaf, surface, unitless)
  if (unitless) {
    Nu_forced <- cons$a * Re ^ cons$b
  } else {
    Nu_forced <- cons$a * drop_units(Re) ^ cons$b
  }
  
  cons <- pars$nu_constant(Re, "free", pars$T_air, T_leaf, surface, unitless)
  if (unitless) {
    Nu_free <- cons$a * Gr ^ cons$b
  } else {
    Nu_free <- cons$a * drop_units(Gr) ^ cons$b
  }
  
  Nu <- (Nu_forced ^ 3.5 + Nu_free ^ 3.5) ^ (1 / 3.5)
  if (!unitless) Nu %<>% set_units()
  
  Nu
  
}

#' L: Latent heat flux density (W / m^2)
#'
#' @inheritParams .get_H
#' 
#' @return Value in W / m^2 of class \code{units}
#' 
#' @details 
#' 
#' \deqn{L = h_\mathrm{vap} g_\mathrm{tw} d_\mathrm{wv}}{L = h_vap g_tw d_wv}
#' 
#' \tabular{lllll}{
#' \emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \eqn{d_\mathrm{wv}}{d_wv} \tab \code{d_wv} \tab water vapour gradient \tab mol / m ^ 3 \tab \link[=.get_dwv]{calculated} \cr
#' \eqn{h_\mathrm{vap}}{h_vap} \tab \code{h_vap} \tab latent heat of vaporization \tab J / mol \tab \link[=.get_hvap]{calculated} \cr
#' \eqn{g_\mathrm{tw}}{g_tw} \tab \code{g_tw} \tab total conductance to H2O \tab (\eqn{\mu}mol H2O) / (m\eqn{^2} s Pa) \tab \link[=.get_gtw]{calculated}
#' }
#'

.get_L <- function(T_leaf, pars, unitless) {

  h_vap <- .get_hvap(T_leaf, unitless)
  g_tw <- .get_gtw(T_leaf, pars, unitless)
  d_wv <- .get_dwv(T_leaf, pars, unitless)
    
  L <- h_vap * g_tw * d_wv
  if (!unitless) L %<>% set_units("W / m ^ 2")
  L

}

#' d_wv: water vapour gradient (mol / m ^ 3)
#' 
#' @inheritParams .get_H
#' 
#' @return Value in mol / m^3 of class \code{units}
#' 
#' @details 
#' 
#' \bold{Water vapour gradient:} The water vapour pressure differential from inside to outside of the leaf is the saturation water vapor pressure inside the leaf (p_leaf) minus the water vapor pressure of the air (p_air):
#' 
#' \deqn{d_\mathrm{wv} = p_\mathrm{leaf} / (R T_\mathrm{leaf}) - RH p_\mathrm{air} / (R T_\mathrm{air})}{d_wv = p_leaf / (R T_leaf) - RH p_air / (R T_air)}
#' 
#' Note that water vapor pressure is converted from kPa to mol / m^3 using ideal gas law. \cr
#' 
#' \tabular{lllll}{
#' \emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \eqn{p_\mathrm{air}}{p_air} \tab \code{p_air} \tab saturation water vapour pressure of air \tab kPa \tab \link[=.get_ps]{calculated}\cr
#' \eqn{p_\mathrm{leaf}}{p_leaf} \tab \code{p_leaf} \tab saturation water vapour pressure inside the leaf \tab kPa \tab \link[=.get_ps]{calculated}\cr
#' \eqn{R} \tab \code{R} \tab ideal gas constant \tab J / (mol K) \tab 8.3144598\cr
#' \eqn{\mathrm{RH}}{RH} \tab \code{RH} \tab relative humidity \tab \% \tab 0.50\cr
#' \eqn{T_\mathrm{air}}{T_air} \tab \code{T_air} \tab air temperature \tab K \tab 298.15\cr
#' \eqn{T_\mathrm{leaf}}{T_leaf} \tab \code{T_leaf} \tab leaf temperature \tab K \tab input
#' }
#'
#' @examples 
#' 
#' # Water vapour gradient: 
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

.get_dwv <- function(T_leaf, pars, unitless) {
  
  # Water vapour differential converted from kPa to mol m ^ -3 using ideal gas law
  d_wv <- .get_ps(T_leaf, pars$P, unitless) / (pars$R * T_leaf) - 
    pars$RH * .get_ps(pars$T_air, pars$P, unitless) / (pars$R * pars$T_air)
  
  if (unitless) {
    d_wv %<>% magrittr::multiply_by(1e3)
  } else {
    d_wv %<>% set_units("mol / m ^ 3")
  }
  
  d_wv
  
}

#' g_tw: total conductance to water vapour (m/s)
#' 
#' @inheritParams .get_H
#' 
#' @return Value in m/s of class \code{units}
#' 
#' @details 
#' 
#' \bold{Total conductance to water vapor:} The total conductance to water vapor (\eqn{g_\mathrm{tw}}{g_tw}) is the sum of the parallel lower (abaxial) and upper (adaxial) conductances:
#' 
#' \deqn{g_\mathrm{tw} = g_\mathrm{w,lower} + g_\mathrm{w,upper}}{g_tw = gw_lower + gw_upper}
#' 
#' The conductance to water vapor on each surface is a function of parallel stomatal (\eqn{g_\mathrm{sw}}{g_sw}) and cuticular (\eqn{g_\mathrm{uw}}{g_uw}) conductances in series with the boundary layer conductance (\eqn{g_\mathrm{bw}}{g_bw}). The stomatal, cuticular, and boundary layer conductance on the lower surface are:
#' 
#' \deqn{g_\mathrm{sw,lower} = g_\mathrm{sw} (1 - sr) R (T_\mathrm{leaf} + T_\mathrm{air}) / 2}{gsw_lower = g_sw (1 - sr) R (T_leaf + T_air) / 2}
#' \deqn{g_\mathrm{uw,lower} = g_\mathrm{uw} / 2 R (T_\mathrm{leaf} + T_\mathrm{air}) / 2}{guw_lower = g_uw / 2 R (T_leaf + T_air) / 2}
#' \cr
#' See \code{\link{.get_gbw}} for details on calculating boundary layer conductance. The equations for the upper surface are:
#' 
#' \deqn{g_\mathrm{sw,upper} = g_\mathrm{sw} sr R (T_\mathrm{leaf} + T_\mathrm{air}) / 2}{gsw_upper = g_sw sr R (T_leaf + T_air) / 2}
#' \deqn{g_\mathrm{uw,upper} = g_\mathrm{uw} / 2 R (T_\mathrm{leaf} + T_\mathrm{air}) / 2}{guw_upper = g_uw / 2 R (T_leaf + T_air) / 2}
#' \cr
#' Note that the stomatal and cuticular conductances are given in units of (\eqn{\mu}mol H2O) / (m\eqn{^2} s Pa) (see \code{\link{make_leafpar}}) and converted to m/s using the ideal gas law. The total leaf stomtal (\eqn{g_\mathrm{sw}}{g_sw}) and cuticular (\eqn{g_\mathrm{uw}}{g_uw}) conductances are partitioned across lower and upper surfaces. The stomatal conductance on each surface depends on stomatal ratio (sr); the cuticular conductance is assumed identical on both surfaces. 
#'
#' \tabular{lllll}{
#' \emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \eqn{g_\mathrm{sw}}{g_sw} \tab \code{g_sw} \tab stomatal conductance to H2O \tab (\eqn{\mu}mol H2O) / (m\eqn{^2} s Pa) \tab 5\cr
#' \eqn{g_\mathrm{uw}}{g_uw} \tab \code{g_uw} \tab cuticular conductance to H2O \tab (\eqn{\mu}mol H2O) / (m\eqn{^2} s Pa) \tab 0.1\cr
#' \eqn{R} \tab \code{R} \tab ideal gas constant \tab J / (mol K) \tab 8.3144598\cr
#' \eqn{\mathrm{logit}(sr)}{logit(sr)} \tab \code{logit_sr} \tab stomatal ratio (logit transformed) \tab none \tab 0 = logit(0.5)\cr
#' \eqn{T_\mathrm{air}}{T_air} \tab \code{T_air} \tab air temperature \tab K \tab 298.15\cr
#' \eqn{T_\mathrm{leaf}}{T_leaf} \tab \code{T_leaf} \tab leaf temperature \tab K \tab input
#' }
#' 
#' @examples 
#' 
#' # Total conductance to water vapor
#' 
#' ## Hypostomatous leaf; default parameters
#' leaf_par <- make_leafpar(replace = list(logit_sr = set_units(-Inf)))
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
#' ## Note that pars$logit_sr is logit-transformed! Use stats::plogis() to convert to proportion.
#' gsw_lower <- set_units(pars$g_sw * (set_units(1) - stats::plogis(pars$logit_sr)) * pars$R * 
#'                          ((T_leaf + pars$T_air) / 2), "m / s")
#' guw_lower <- set_units(pars$g_uw * 0.5 * pars$R * ((T_leaf + pars$T_air) / 2), "m / s")
#' gtw_lower <- 1 / (1 / (gsw_lower + guw_lower) + 1 / gbw_lower)
#' 
#' # Upper surface ----
#' gsw_upper <- set_units(pars$g_sw * stats::plogis(pars$logit_sr) * pars$R * 
#'                          ((T_leaf + pars$T_air) / 2), "m / s")
#' guw_upper <- set_units(pars$g_uw * 0.5 * pars$R * ((T_leaf + pars$T_air) / 2), "m / s")
#' gtw_upper <- 1 / (1 / (gsw_upper + guw_upper) + 1 / gbw_upper)
#' 
#' ## Lower and upper surface are in parallel
#' g_tw <- gtw_lower + gtw_upper
#' 

.get_gtw <- function(T_leaf, pars, unitless) {
 
  # Lower surface ----
  gbw_lower <- .get_gbw(T_leaf, "lower", pars, unitless)
  
  # Convert stomatal and cuticular conductance from molar to 'engineering' units
  # See email from Tom Buckley (July 4, 2017)
  if (unitless) {
    
    gsw_lower <- (pars$g_sw * (1 - stats::plogis(pars$logit_sr)) * pars$R * 
      ((T_leaf + pars$T_air) / 2)) %>%
      magrittr::divide_by(1e6)
    guw_lower <- (pars$g_uw * 0.5 * pars$R * ((T_leaf + pars$T_air) / 2)) %>%
      magrittr::divide_by(1e6)
    
  } else {
    
    gsw_lower <- set_units(pars$g_sw * (set_units(1) - stats::plogis(pars$logit_sr)) *
                             pars$R * ((T_leaf + pars$T_air) / 2), "m / s")
    guw_lower <- set_units(pars$g_uw * 0.5 * pars$R * ((T_leaf + pars$T_air) / 2), "m / s")
    
  }
  
  gtw_lower <- 1 / (1 / (gsw_lower + guw_lower) + 1 / gbw_lower)
  
  # Upper surface ----
  gbw_upper <- .get_gbw(T_leaf, "upper", pars, unitless)
  
  # Convert stomatal and cuticular conductance from molar to 'engineering' units
  # See email from Tom Buckley (July 4, 2017)
  if (unitless) {
    
    gsw_upper <- (pars$g_sw * stats::plogis(pars$logit_sr) * pars$R * 
                             ((T_leaf + pars$T_air) / 2)) %>%
      magrittr::divide_by(1e6)
    guw_upper <- (pars$g_uw * 0.5 * pars$R * ((T_leaf + pars$T_air) / 2)) %>%
                    magrittr::divide_by(1e6)
    
  } else {
    
    gsw_upper <- set_units(pars$g_sw * stats::plogis(pars$logit_sr) * pars$R * 
                             ((T_leaf + pars$T_air) / 2), "m / s")
    guw_upper <- set_units(pars$g_uw * 0.5 * pars$R * ((T_leaf + pars$T_air) / 2), "m / s")
    
  }
  
  gtw_upper <- 1 / (1 / (gsw_upper + guw_upper) + 1 / gbw_upper)
  
  # Lower and upper surface are in parallel
  g_tw <- gtw_lower + gtw_upper
  
  g_tw
  
}

#' h_vap: heat of vaporization (J / mol)
#' 
#' @inheritParams .get_H
#' 
#' @return Value in J/mol of class \code{units}
#' 
#' @details 
#' 
#' \bold{Heat of vaporization:} The heat of vaporization (\eqn{h_\mathrm{vap}}{h_vap}) is a function of temperature. I used data from on temperature and \eqn{h_\mathrm{vap}}{h_vap} from Nobel (2009, Appendix 1) to estimate a linear regression. See Examples.
#'
#' @examples 
#' 
#' # Heat of vaporization and temperature
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
#' @references Nobel PS. 2009. Physicochemical and Environmental Plant Physiology. 4th Edition. Academic Press.
#' 

.get_hvap <- function(T_leaf, unitless) {
  
  # Equation from Foster and Smith 1986 seems to be off:
  # h_vap <- 4.504e4 - 41.94 * T_leaf
  # Instead, using regression based on data from Nobel (2009, 4th Ed, Appendix 1)
  # T_K <- 273.15 + c(0, 10, 20, 25, 30, 40, 50, 60)
  # h_vap <- 1e3 * c(45.06, 44.63, 44.21, 44, 43.78, 43.35, 42.91, 42.47) # (in J / mol)
  # fit <- lm(h_vap ~ T_K)
  if (unitless) {
    h_vap <- 56847.68250 - 43.12514 * T_leaf
  } else {
    h_vap <- set_units(56847.68250, "J / mol") - 
      set_units(43.12514, "J / mol / K") * set_units(T_leaf, "K")
    h_vap %<>% set_units("J / mol")
  }
  
  h_vap
  
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
#' \deqn{g_\mathrm{bw} = D_\mathrm{w} Sh / d}{g_bw = D_w Sh / d}
#' 
#' \tabular{lllll}{
#' \emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \eqn{d} \tab \code{leafsize} \tab Leaf characteristic dimension in meters \tab m \tab 0.1\cr
#' \eqn{D_\mathrm{w}}{D_w} \tab \code{D_w} \tab diffusion coefficient for water vapour \tab m\eqn{^2} / s \tab \link[=.get_Dx]{calculated}\cr
#' \eqn{Sh} \tab \code{Sh} \tab Sherwood number \tab none \tab \link[=.get_sh]{calculated}
#' }
#' 
.get_gbw <- function(T_leaf, surface, pars, unitless) {

  surface %<>% match.arg(c("lower", "upper"))

  D_w <- .get_Dx(pars$D_w0, (pars$T_air + T_leaf) / 2, pars$eT, pars$P, unitless)

  # Calculate Sherwood numbers
  Sh <- .get_sh(T_leaf, surface, pars, unitless)

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
#' The Sherwood number depends on a combination how much free or forced convection predominates. For mixed convection: \cr 
#' \cr
#' \deqn{Sh = (a Re ^ b) ^ {3.5} + (c Gr ^ d) ^ {3.5}) ^ {1 / 3.5}}{Sh = (a Re ^ b) ^ 3.5 + (c Gr ^ d) ^ 3.5) ^ (1 / 3.5)}
#' \cr
#' \tabular{lllll}{
#' \emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \eqn{a, b, c, d} \tab \code{a, b, c, d} \tab empirical coefficients \tab none \tab \link[=make_constants]{calculated}\cr
#' \eqn{Gr} \tab \code{Gr} \tab Grashof number \tab none \tab \link[=.get_gr]{calculated}\cr
#' \eqn{Re} \tab \code{Re} \tab Reynolds number \tab none \tab \link[=.get_re]{calculated}
#' }
#' 

.get_sh <- function(T_leaf, surface, pars, unitless) {

  surface %<>% match.arg(c("lower", "upper"))
  
  Gr <- .get_gr(T_leaf, pars, unitless)
  Re <- .get_re(T_leaf, pars, unitless)

  # Archemides number
  # Ar <- Gr / Re ^ 2

  D_h <- .get_Dx(pars$D_h0, (pars$T_air + T_leaf) / 2, pars$eT, pars$P, unitless)
  D_w <- .get_Dx(pars$D_w0, (pars$T_air + T_leaf) / 2, pars$eT, pars$P, unitless)

  cons <- pars$nu_constant(Re, "forced", pars$T_air, T_leaf, surface, unitless)
  if (unitless) {
    Nu_forced <- cons$a * Re ^ cons$b
    Sh_forced <- Nu_forced * (D_h / D_w) ^ pars$sh_constant("forced")
  } else {
    Nu_forced <- cons$a * drop_units(Re) ^ cons$b
    Sh_forced <- Nu_forced * drop_units(D_h / D_w) ^ pars$sh_constant("forced")
  }

  cons <- pars$nu_constant(Re, "free", pars$T_air, T_leaf, surface, unitless)
  if (unitless) {
    Nu_free <- cons$a * Gr ^ cons$b
    Sh_free <- Nu_free * (D_h / D_w) ^ pars$sh_constant("free")
  } else {
    Nu_free <- cons$a * drop_units(Gr) ^ cons$b
    Sh_free <- Nu_free * drop_units(D_h / D_w) ^ pars$sh_constant("free")
  }

  Sh <- (Sh_forced ^ 3.5 + Sh_free ^ 3.5) ^ (1 / 3.5)
  if (!unitless) Sh %<>% set_units()

  Sh

}

make_parameter_sets <- function(pars, par_units) {
  
  pars %<>%
    names() %>%
    glue::glue("{x} = pars${x}", x = .) %>%
    stringr::str_c(collapse = ", ") %>%
    glue::glue("tidyr::crossing({x})", x = .) %>%
    parse(text = .) %>%
    eval() %>%
    purrr::transpose()
  
  tidyr::crossing(i = seq_len(length(pars)),
                  par = names(pars[[1]])) %>%
    dplyr::transmute(ex = glue::glue("units(pars[[{i}]]${par}) <<- par_units${par}", 
                                     i = .data$i, par = .data$par)) %>%
    dplyr::pull("ex") %>%
    parse(text = .) %>%
    eval()
  
  pars
  
}

find_tleaves <- function(par_sets, constants, progress, quiet, unitless, parallel) {
  
  if (!quiet) {
    glue::glue("\nSolving for T_leaf from {n} parameter set{s}...", 
               n = length(par_sets), 
               s = dplyr::if_else(length(par_sets) > 1, "s", "")) %>%
      crayon::green() %>%
      message(appendLF = FALSE)
  }
  
  if (parallel) future::plan("multiprocess")

  if (progress & !parallel) pb <- dplyr::progress_estimated(length(par_sets))
  
  soln <- suppressWarnings(
    par_sets %>%
      furrr::future_map_dfr(~{
        
        ret <- tleaf(leaf_par(.x), enviro_par(.x), constants, quiet = TRUE,
                     set_units = FALSE)
        if (progress & !parallel) pb$tick()$print()
        ret
        
      }, .progress = progress)
  )
  
  soln
  
}

find_tleaf <- function(leaf_par, enviro_par, constants, quiet, unitless) {
  
  # Balance energy fluxes -----
  enviro_par$T_air %<>% set_units("K") # convert T_air to Kelvin before dropping units
  init <- drop_units(enviro_par$T_air)
  
  if (!quiet) {
    "\nSolving for T_leaf ..." %>%
      crayon::green() %>%
      message(appendLF = FALSE)
  }
  
  .f <- function(T_leaf, ...) {
    eb <- energy_balance(T_leaf, ...)
    if (is(eb, "units")) eb %<>% drop_units()
    eb
  }
  
  fit <- safely_uniroot(
    f = .f, leaf_par = leaf_par, enviro_par = enviro_par, constants = constants, 
    quiet = TRUE, unitless = unitless, check = FALSE,
    lower = drop_units(enviro_par$T_air - set_units(30, "K")), 
    upper = drop_units(enviro_par$T_air + set_units(30, "K"))
  )
  
  if (is.null(fit$result)) {
    fit <- list(root = NA, f.root = NA, convergence = 1)
  } else {
    fit <- fit$result
  }
  
  soln <- data.frame(T_leaf = fit$root, value = fit$f.root, 
                     convergence = dplyr::if_else(is.null(fit$convergence), 0, 1))
  
  if (!quiet) {
    " done" %>%
      crayon::green() %>%
      message()
  }
  
  soln
  
}

safely_uniroot <- purrr::safely(uniroot)

#' Evaporation (mol / (m^2 s))
#' 
#' @inheritParams .get_Sr
#' @inheritParams tleaves
#' 
#' @return 
#' \code{unitless = TRUE}: A value in units of mol / (m ^ 2 / s) number of class \code{numeric} 
#' \code{unitless = FALSE}: A value in units of mol / (m ^ 2 / s) of class \code{units} 
#' 
#' @details 
#' The leaf evaporation rate is the product of the total conductance to water vapour (m / s) and the water vapour gradient (mol / m^3):
#' 
#' \deqn{E = g_\mathrm{tw} D_\mathrm{wv}}{E = g_tw D_wv}
#' 
#' If \code{unitless = TRUE}, \code{T_leaf} is assumed in degrees K without checking.
#' 
#' @export

E <- function(T_leaf, pars, unitless) {
  
  if (unitless & is(T_leaf, "units")) T_leaf %<>% drop_units() 
  if (!unitless) T_leaf %<>% set_units("K")
  E <- .get_gtw(T_leaf, pars, unitless) * .get_dwv(T_leaf, pars, unitless)
  if (!unitless) E %<>% set_units("mol/m^2/s")
  E
  
}
