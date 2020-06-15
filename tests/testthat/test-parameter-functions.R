context("parameter functions")
library(tealeaves)

test_that("some parameters can be defined as functions", {

  .x <- list(
    c_p = 1,
    D_h0 = 1,
    D_m0 = 1,
    D_w0 = 1,
    epsilon = 1,
    eT = 1,
    G = 1,
    R = 1,
    R_air = 1,
    s = 1,
    nu_constant = 1,
    sh_constant = 1
  )

  cs <- constants(.x)
  expect_s3_class(cs, c("constants", "list"))
  expect_length(cs, length(parameter_names("constants")))

  parameter_function <- c("nu_constant", "sh_constant")
  parameter_numeric <- setdiff(parameter_names("constants"), parameter_function)

  error_message <- 'no applicable method for \'set_units\' applied to an object of class "function"'
  blank_function <- function() {}

  .x1 <- .x
  .x1[[sample(parameter_numeric, 1)]] <- blank_function

  expect_error(constants(.x1), regexp = error_message)

  .x1 <- .x
  for (i in parameter_function) .x1[[i]] <- blank_function
  expect_error(constants(.x1), regexp = NA)

})
