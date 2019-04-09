#' \code{tealeaves} package
#'
#' Solve for leaf temperature using energy balance
#'
#' See the README on
#' \href{https://github.com/cdmuir/tealeaves}{GitHub}
#'
#' @docType package
#' @name tealeaves
#' @importFrom magrittr %>% %<>%
#' @importFrom methods is
#' @importFrom rlang .data
#' @importFrom units drop_units set_units
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

## quiets concerns of R CMD check re: units
utils::globalVariables(c("K"))