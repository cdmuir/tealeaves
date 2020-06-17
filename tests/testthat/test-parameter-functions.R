context("parameter functions")
library(tealeaves)

test_that("some parameters can be defined as functions", {

  error_message <- 'no applicable method for \'set_units\' applied to an object of class "function"'
  blank_function <- function() {}
  
  # Constants ----
  nms <- parameter_names("constants")
  .x <- as.vector(setNames(rep(1, length(nms)), nms), mode = "list")

  cs <- constants(.x)
  expect_s3_class(cs, c("constants", "list"))

  parameter_function <- c("nu_constant", "sh_constant")
  parameter_numeric <- setdiff(nms, parameter_function)

  .x1 <- .x
  .x1[[sample(parameter_numeric, 1)]] <- blank_function

  expect_error(constants(.x1), regexp = error_message)

  .x1 <- .x
  for (i in parameter_function) .x1[[i]] <- blank_function
  expect_error(constants(.x1), regexp = NA)

  # Environmental parameters ----
  nms <- parameter_names("enviro")
  .x <- as.vector(setNames(rep(1, length(nms)), nms), mode = "list")
  
  ep <- enviro_par(.x)
  expect_s3_class(ep, c("enviro_par", "list"))
  
  parameter_function <- c("T_sky")
  parameter_numeric <- setdiff(nms, parameter_function)
  
  .x1 <- .x
  .x1[[sample(parameter_numeric, 1)]] <- blank_function
  
  expect_error(enviro_par(.x1), regexp = error_message)
  
  .x1 <- .x
  for (i in parameter_function) .x1[[i]] <- blank_function
  expect_error(enviro_par(.x1), regexp = NA)

  # Leaf parameters ----
  nms <- parameter_names("leaf")
  .x <- as.vector(setNames(rep(1, length(nms)), nms), mode = "list")
  
  lp <- leaf_par(.x)
  expect_s3_class(lp, c("leaf_par", "list"))
  
  parameter_function <- NULL
  parameter_numeric <- setdiff(nms, parameter_function)
  
  .x1 <- .x
  .x1[[sample(parameter_numeric, 1)]] <- blank_function
  
  expect_error(leaf_par(.x1), regexp = error_message)
  
  .x1 <- .x
  for (i in parameter_function) .x1[[i]] <- blank_function
  expect_error(leaf_par(.x1), regexp = NA)
  
})

test_that("parameter function replacements must be length 1L", {

  # Revisit this test once I have vetted replacing defaults for nu_constant and sh_constant  
  # x <- .parameter_functions("constants")
  # for (i in x) {
  #   
  #   replace <- list(x = c(function() {}, function() {}))
  #   names(replace) <- i
  #   expect_error(make_constants(replace = replace))
  #   
  # }

  x <- .parameter_functions("enviro")
  for (i in x) {
    
    replace <- list(x = c(function() {}, function() {}))
    names(replace) <- i
    expect_error(make_enviro(replace = replace))
    
  }

  x <- .parameter_functions("leaf")
  for (i in x) {
    
    replace <- list(x = c(function() {}, function() {}))
    names(replace) <- i
    expect_error(make_leaf(replace = replace))
    
  }
  
})
