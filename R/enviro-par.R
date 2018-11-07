#' S3 class enviro_par
#' @exportClass enviro_par
#

#' @param .x A list to be constructed into \strong{enviro_par}.
#' 
#' @description 
#' 
#' Constructor function for enviro_par class. This function ensures that environmental parameter inputs are properly formatted.
#' 
#' @export

enviro_par <- function(.x) {
  
  which <- "enviro"
  nms <- parameter_names(which)
  
  stopifnot(is.list(.x))
  
  stopifnot(all(nms %in% names(.x)))
  
  .x %<>% magrittr::extract(nms)
  
  # Check values ------
  stopifnot(.x$T_air >= set_units(0, "K"))
  stopifnot(.x$RH >= set_units(0) & .x$RH <= set_units(1))
  stopifnot(.x$S_sw >= set_units(0, "W / m^2"))
  stopifnot(.x$S_lw >= set_units(0, "W / m^2"))
  stopifnot(.x$wind >= set_units(0, "m / s"))
  stopifnot(.x$P >= set_units(0, "kPa"))

  structure(.x, class = c(stringr::str_c(which, "_par"), "list"))
  
}

