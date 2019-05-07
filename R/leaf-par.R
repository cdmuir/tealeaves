#' S3 class leaf_par
#' @exportClass leaf_par
#

#' @param .x A list to be constructed into \strong{leaf_par}. If units are not provided, they will be set without conversion. If units are provided, they will be checked and converted to units that tealeaves uses.
#' 
#' @description 
#' 
#' Constructor function for leaf_par class. This function ensures that leaf parameter inputs are properly formatted.
#' 
#' @export

leaf_par <- function(.x) {
  
  which <- "leaf"
  nms <- tealeaves::parameter_names(which)
  
  stopifnot(is.list(.x))
  
  if (!all(nms %in% names(.x))) {
    nms[!(nms %in% names(.x))] %>%
      stringr::str_c(collapse = ", ") %>%
      glue::glue("{x} not in parameter names required for {which}",
                 x = ., which = which) %>%
      stop()
  }
  
  .x %<>% magrittr::extract(nms)

  # Set units ----
  .x$abs_l %<>% set_units()
  .x$abs_s %<>% set_units()
  .x$g_sw %<>% set_units(umol / (m^2 * s * Pa))
  .x$g_uw %<>% set_units(umol / (m^2 * s * Pa))
  .x$leafsize %<>% set_units("m")
  .x$logit_sr %<>% set_units()
  
  # Check values ----
  stopifnot(.x$abs_l >= set_units(0) & .x$abs_l <= set_units(1))
  stopifnot(.x$abs_s >= set_units(0) & .x$abs_s <= set_units(1))
  stopifnot(.x$g_sw >= set_units(0, umol / (m^2 * s * Pa)))
  stopifnot(.x$g_uw >= set_units(0, umol / (m^2 * s * Pa)))
  stopifnot(.x$leafsize >= set_units(0, m))
  
  structure(.x, class = c(stringr::str_c(which, "_par"), "list"))
  
}

