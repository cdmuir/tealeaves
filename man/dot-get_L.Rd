% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tleaves.R
\name{.get_L}
\alias{.get_L}
\title{L: Latent heat flux density (W / m^2)}
\usage{
.get_L(T_leaf, pars, unitless)
}
\arguments{
\item{T_leaf}{Leaf temperature in Kelvin}

\item{pars}{Concatenated parameters (\code{leaf_par}, \code{enviro_par}, and \code{constants})}

\item{unitless}{Logical. Should function use parameters with \code{units}? The function is faster when FALSE, but input must be in correct units or else results will be incorrect without any warning.}
}
\value{
Value in W / m^2 of class \code{units}
}
\description{
L: Latent heat flux density (W / m^2)
}
\details{
\deqn{L = h_\mathrm{vap} g_\mathrm{tw} d_\mathrm{wv}}{L = h_vap g_tw d_wv}

\tabular{lllll}{
\emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
\eqn{d_\mathrm{wv}}{d_wv} \tab \code{d_wv} \tab water vapour gradient \tab mol / m ^ 3 \tab \link[=.get_dwv]{calculated} \cr
\eqn{h_\mathrm{vap}}{h_vap} \tab \code{h_vap} \tab latent heat of vaporization \tab J / mol \tab \link[=.get_hvap]{calculated} \cr
\eqn{g_\mathrm{tw}}{g_tw} \tab \code{g_tw} \tab total conductance to H2O \tab (\eqn{\mu}mol H2O) / (m\eqn{^2} s Pa) \tab \link[=.get_gtw]{calculated}
}
}
\examples{

library(tealeaves)

cs <- make_constants()
ep <- make_enviropar()
lp <- make_leafpar()

T_leaf <- set_units(298.15, K)

tealeaves:::.get_L(T_leaf, c(cs, ep, lp), FALSE)

}
