#' Ar: Archimedes number
#'
#' @inheritParams .get_H
#' @inheritParams .get_gh
#' 
#' @return 
#' \code{unitless = TRUE}: A unitless number of class \code{numeric} 
#' \code{unitless = FALSE}: A unitless number of class \code{units} 
#' Also returns Reynolds and Grashof numbers
#' 
#' @details 
#' 
#' The Archimedes number is a dimensionless number that describes when free or forced convection dominates.
#' 
#' \deqn{Ar = Gr / Re ^ 2}
#' \cr
#' \tabular{lllll}{
#' \emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \eqn{Gr} \tab \code{Gr} \tab Grashof number \tab none \tab \link[=.get_gr]{calculated}\cr
#' \eqn{Re} \tab \code{Re} \tab Reynolds number \tab none \tab \link[=.get_re]{calculated}
#' }
#' 
#' @examples 
#' cs <- make_constants()
#' ep <- make_enviropar()
#' lp <- make_leafpar()
#' pars <- c(cs, lp, ep)
#' T_leaf <- set_units(298.15, "K")
#' 
#' Ar(T_leaf, pars)
#' 
#' @export
#' 

Ar <- function(T_leaf, pars, unitless = FALSE) {
  
  Gr <- .get_gr(T_leaf, pars, unitless)
  Re <- .get_re(T_leaf, pars, unitless)
  
  Ar <- Gr / Re ^ 2

  data.frame(Ar = Ar, Gr = Gr, Re = Re) 
  
}