#' Make lists of parameters of leaf, environmental, or constant parameters
#'
#' @inheritParams tleaf
#' @param replace A named list of parameters to replace defaults. If \code{NULL}, defaults will be used.
#'
#' @name make_parameters

NULL

#' make_leafpar
#' 
#' @rdname make_parameters
#'
#' @return 
#' 
#' \code{make_leafpar}: An object inheriting from class \code{\link{leaf_par}}\cr
#' \code{make_enviropar}: An object inheriting from class \code{\link{enviro_par}}\cr
#' \code{make_constants}: An object inheriting from class \code{\link{constants}}
#'
#' @details
#'
#' \bold{Leaf parameters:}
#'
#' \tabular{lllll}{
#' \emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \eqn{d} \tab \code{leafsize} \tab Leaf characteristic dimension \tab m \tab 0.1 \cr
#' \eqn{\alpha_\mathrm{s}}{\alpha_s} \tab \code{abs_s} \tab absorbtivity of shortwave radiation (0.3 - 4 \eqn{\mu}m) \tab none \tab 0.80 \cr
#' \eqn{\alpha_\mathrm{l}}{\alpha_l} \tab \code{abs_l} \tab absorbtivity of longwave radiation (4 - 80 \eqn{\mu}m) \tab none \tab 0.97 \cr
#' \eqn{g_\mathrm{sw}}{g_sw} \tab \code{g_sw} \tab stomatal conductance to H2O \tab (\eqn{\mu}mol H2O) / (m\eqn{^2} s Pa) \tab 5 \cr
#' \eqn{g_\mathrm{uw}}{g_uw} \tab \code{g_uw} \tab cuticular conductance to H2O \tab (\eqn{\mu}mol H2O) / (m\eqn{^2} s Pa) \tab 0.1 \cr
#' \eqn{\mathrm{logit}(sr)}{logit(sr)} \tab \code{logit_sr} \tab stomatal ratio (logit transformed) \tab none \tab 0 = logit(0.5)
#' }
#'
#' \bold{Environment parameters:}
#'
#' \tabular{lllll}{
#' \emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \eqn{P} \tab \code{P} \tab atmospheric pressure \tab kPa \tab 101.3246 \cr
#' \eqn{r} \tab \code{r} \tab reflectance for shortwave irradiance (albedo) \tab none \tab 0.2 \cr
#' \eqn{\mathrm{RH}}{RH} \tab \code{RH} \tab relative humidity \tab none \tab 0.50 \cr
#' \eqn{S_\mathrm{sw}}{S_sw} \tab \code{S_sw} \tab incident short-wave (solar) radiation flux density \tab W / m\eqn{^2} \tab 1000 \cr
#' \eqn{S_\mathrm{lw}}{S_lw} \tab \code{S_lw} \tab incident long-wave radiation flux density \tab W / m\eqn{^2} \tab \link[=.get_Rabs]{calculated} \cr
#' \eqn{T_\mathrm{air}}{T_air} \tab \code{T_air} \tab air temperature \tab K \tab 298.15 \cr
#' \eqn{u} \tab \code{wind} \tab windspeed \tab m / s \tab 2
#' }
#'
#' \bold{Constants:}
#' \tabular{lllll}{
#' \emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \eqn{c_p} \tab \code{c_p} \tab heat capacity of air \tab J / (g K) \tab 1.01 \cr
#' \eqn{D_{h,0}}{D_h0} \tab \code{D_h0} \tab diffusion coefficient for heat in air at 0 °C \tab m\eqn{^2} / s \tab 19.0e-06\cr
#' \eqn{D_{m,0}}{D_m0} \tab \code{D_m0} \tab diffusion coefficient for momentum in air at 0 °C \tab m\eqn{^2} / s \tab 13.3e-06 \cr
#' \eqn{D_{w,0}}{D_w0} \tab \code{D_w0} \tab diffusion coefficient for water vapour in air at 0 C \tab m\eqn{^2} / s \tab 21.2e-06 \cr
#' \eqn{\epsilon} \tab \code{epsilon} \tab ratio of water to air molar masses \tab none \tab 0.622 \cr
#' \eqn{eT} \tab \code{eT} \tab exponent for temperature dependence of diffusion \tab none \tab 1.75 \cr
#' \eqn{G} \tab \code{G} \tab gravitational acceleration \tab m / s\eqn{^2} \tab 9.8 \cr
#' \eqn{Nu} \tab \code{Nu} \tab Nusselt number \tab none \tab \link[=.get_nu]{calculated} \cr
#' \eqn{R} \tab \code{R} \tab ideal gas constant \tab J / (mol K) \tab 8.3144598 \cr
#' \eqn{R_\mathrm{air}}{R_air} \tab \code{R_air} \tab specific gas constant for dry air \tab J / (kg K) \tab 287.058\cr
#' \eqn{\sigma} \tab \code{s} \tab Stephan-Boltzmann constant \tab W / (m\eqn{^2} K\eqn{^4}) \tab 5.67e-08 \cr
#' \eqn{Sh} \tab \code{Sh} \tab Sherwood number \tab none \tab \link[=.get_sh]{calculated}
#' }
#'
#' @export
#' 

make_leafpar <- function(replace = NULL) {

  # Default parameters -----
  obj <- list(
    abs_s = set_units(0.8),
    abs_l = set_units(0.97),
    g_sw = set_units(5, "umol / (m^2 * s * Pa)"),
    g_uw = set_units(0.1, "umol / (m^2 * s * Pa)"),
    leafsize = set_units(0.1, "m"),
    logit_sr = set_units(0)
  )
  
  # Replace defaults -----
  obj %<>% replace_defaults(replace)

  # Assign class and return -----
  obj %<>% leaf_par()

  obj

}

#' make_enviropar
#' @rdname make_parameters
#' @export

make_enviropar <- function(replace = NULL) {

  # Default parameters -----
  obj <- list(
    P = set_units(101.3246, "kPa"),
    RH = set_units(0.50),
    r = set_units(0.2),
    S_sw = set_units(1000, "W / m^2"),
    T_air = set_units(298.15, "K"),
    wind = set_units(2, "m / s")
  ) 
  
  # Replace defaults -----

  obj %<>% replace_defaults(replace)

  # Assign class and return -----
  obj %<>% enviro_par()
  
  obj

}

#' make_constants
#' @rdname make_parameters
#' @export

make_constants <- function(replace = NULL) {

  # Defaults parameters -----
  obj <- list(
    c_p = set_units(1.01, "J / (g * K)"),
    D_h0 = set_units(1.9e-5, "m ^ 2 / s"),
    D_m0 = set_units(13.3e-6, "m ^ 2 / s"),
    D_w0 = set_units(21.2e-6, "m ^ 2 / s"),
    epsilon = set_units(0.622),
    eT = set_units(1.75),
    G = set_units(9.8, "m / s ^ 2"),
    nu_constant = function(Re, type, T_air, T_leaf, surface, unitless) {
      
      if (!unitless) {
        stopifnot(units(T_air)$numerator == "K" & 
                    length(units(T_air)$denominator) == 0L)
        stopifnot(units(T_leaf)$numerator == "K" & 
                    length(units(T_leaf)$denominator) == 0L)
      }
      
      type %<>% match.arg(c("free", "forced"))
      
      if (identical(type, "forced")) {
        if (unitless) {
          # laminar flow
          if (Re <= 4000) ret <- list(a = 0.6, b = 0.5)
          # turbulent flow
          if (Re > 4000) ret <- list(a = 0.032, b = 0.8)
        } else {
          # laminar flow
          if (Re <= set_units(4000)) ret <- list(a = 0.6, b = 0.5)
          # turbulent flow
          if (Re > set_units(4000)) ret <- list(a = 0.032, b = 0.8)
        }
        return(ret)
      }
      
      if (identical(type, "free")) {
        surface %<>% match.arg(c("lower", "upper"))
        if ((surface == "upper" & T_leaf > T_air) |
            (surface == "lower" & T_leaf < T_air)) {
          ret <- list(a = 0.5, b = 0.25)
        } else {
          ret <- list(a = 0.23, b = 0.25)
        }
        return(ret)
      }
      
    },
    R = set_units(8.3144598, "J / (mol * K)"),
    R_air = set_units(287.058, "J / (kg * K)"),
    s = set_units(5.67e-08, "W / (m ^ 2 * K ^ 4)"),
    sh_constant = function(type, unitless) {
      
      type %>%
        match.arg(c("free", "forced")) %>%
        switch(forced = 0.33, free = 0.25)
      
    }
  )

  # Replace defaults -----
  if ("nu_constant" %in% names(replace)) {
    stopifnot(is.function(replace$nu_constant))
    obj$nu_constant <- replace$nu_constant
    replace$nu_constant <- NULL
  }
  
  if ("sh_constant" %in% names(replace)) {
    stopifnot(is.function(replace$sh_constant))
    obj$sh_constant <- replace$sh_constant
    replace$sh_constant <- NULL
  }
  
  obj %<>% replace_defaults(replace)

  # Assign class and return -----
  obj %<>% constants()
  
  obj

}

#' Replace default parameters
#'
#' @param obj List of default values
#' @param replace List of replacement values
#'

replace_defaults <- function(obj, replace) {

  if (!is.null(replace)) {

    stopifnot(is.list(replace))
    stopifnot(all(sapply(replace, inherits, what = "units")))
    stopifnot(all(sapply(replace, is.numeric)))
    x <- names(replace)
    if (any(!x %in% names(obj))) {
      warning(sprintf("The following parameters in 'replace' were not recognized:\n%s", paste0(x[!x %in% names(obj)], collapse = "\n")))
      x %<>% .[. %in% names(obj)]
    }
    obj[x] <- replace[x]

  }

  obj

}
