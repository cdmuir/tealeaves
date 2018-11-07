#' S3 class leaf_par
#' @exportClass leaf_par
#

#' @param .x A list to be constructed into \strong{leaf_par}.
#' 
#' @description 
#' 
#' Constructor function for leaf_par class. This function ensures that leaf parameter inputs are properly formatted.
#' 
#' @export

leaf_par <- function(.x) {
  
  which <- "leaf"
  nms <- parameter_names(which)
  
  stopifnot(is.list(.x))
  
  stopifnot(all(nms %in% names(.x)))
  
  .x %<>% magrittr::extract(nms)
  
  # Check values ------
  stopifnot(.x$abs_s >= set_units(0) & .x$abs_s <= set_units(1))
  stopifnot(.x$abs_l >= set_units(0) & .x$abs_l <= set_units(1))
  stopifnot(.x$g_sw >= set_units(0, "umol / (m^2 * s * Pa)"))
  stopifnot(.x$g_uw >= set_units(0, "umol / (m^2 * s * Pa)"))
  stopifnot(.x$leafsize >= set_units(0, "m"))

  structure(.x, class = c(stringr::str_c(which, "_par"), "list"))
  
}

