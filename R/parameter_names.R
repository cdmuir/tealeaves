#' Get vector of parameter names
#' 
#' @param which A character string indicating which parameter names to retreive, "leaf", "enviro", or "constants". Partial matching allowed.

parameter_names <- function(which) {
  
  which %>% 
    match.arg(c("leaf", "enviro", "constants")) %>%
    switch(
         leaf = c("abs_l", "abs_s", "g_sw", "g_uw", "leafsize", "logit_sr"),
         enviro = c("P", "RH", "S_lw", "S_sw", "T_air", "wind"),
         constants = c("c_p","D_h0", "D_m0", "D_w0", "epsilon", "eT", "G", 
                       "nu_constant", "phi", "R", "R_air", "s", "sh_constant")
  )
  
}
