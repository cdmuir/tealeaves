#' S3 class constants
#' @exportClass constants
#

#' @param .x A list to be constructed into \strong{constants}.
#' 
#' @description 
#' 
#' Constructor function for constants class. This function ensures that physical constant inputs are properly formatted.
#' 
#' @export

constants <- function(.x) {
  
  which <- "constants"
  nms <- parameter_names(which)
  
  stopifnot(is.list(.x))
  
  stopifnot(all(nms %in% names(.x)))
  
  .x %<>% magrittr::extract(nms)

  # Set units ----
  .x$c_p %<>% set_units("J / (g * K)")
  .x$D_h0 %<>% set_units("m ^ 2 / s")
  .x$D_m0 %<>% set_units("m ^ 2 / s")
  .x$D_w0 %<>% set_units("m ^ 2 / s")
  .x$epsilon %<>% set_units()
  .x$eT %<>% set_units()
  .x$G %<>% set_units("m / s ^ 2")
  .x$phi %<>% set_units()
  .x$R %<>% set_units("J / (mol * K)")
  .x$R_air %<>% set_units("J / (kg * K)")
  .x$s %<>% set_units("W / (m ^ 2 * K ^ 4)")
  
  # Check values ----
  stopifnot(.x$c_p >= set_units(0, "J / (g * K)"))
  stopifnot(.x$D_h0 >= set_units(0, "m ^ 2 / s"))
  stopifnot(.x$D_m0 >= set_units(0, "m ^ 2 / s"))
  stopifnot(.x$D_w0 >= set_units(0, "m ^ 2 / s"))
  stopifnot(.x$epsilon >= set_units(0))
  stopifnot(.x$eT >= set_units(0))
  stopifnot(.x$G >= set_units(0, "m / s ^ 2"))
  stopifnot(.x$phi >= set_units(0) & .x$phi <= set_units(1))
  stopifnot(.x$R >= set_units(0, "J / (mol * K)"))
  stopifnot(.x$R_air >= set_units(0, "J / (kg * K)"))
  stopifnot(.x$s >= set_units(0, "W / (m ^ 2 * K ^ 4)"))
  
  structure(.x, class = c(which, "list"))
  
}

