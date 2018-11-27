#' Ar: Archimedes number
#'
#' @inheritParams .get_H
#' @inheritParams .get_gh
#'
#' @return A unitless number of class \code{units} 
#' 
#' @details 
#' 
#' The Archimedes number is a dimensionless number that describes when free or forced convection dominates. depends on a combination how much free or forced convection predominates.
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
#' lp <- make_enviropar()
#' ep <- make_leafpar()
#' pars <- c(cs, lp, ep)
#' T_leaf <- set_units(298.15, "K")
#' 
#' Ar(T_leaf, "lower", pars)
#' Ar(T_leaf, "upper", pars)
#' 
#' @export
#' 

Ar <- function(T_leaf, surface, pars) {
  
  surface %<>% match.arg(c("lower", "upper"))
  
  Gr <- .get_gr(T_leaf, pars)
  Re <- .get_re(T_leaf, pars)
  
  Ar <- Gr / Re ^ 2
  
  Ar
  
}