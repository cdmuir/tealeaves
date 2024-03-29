% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tleaves.R
\name{.get_Sr}
\alias{.get_Sr}
\title{S_r: longwave re-radiation (W / m^2)}
\usage{
.get_Sr(T_leaf, pars)
}
\arguments{
\item{T_leaf}{Leaf temperature in Kelvin}

\item{pars}{Concatenated parameters (\code{leaf_par}, \code{enviro_par}, and \code{constants})}
}
\value{
Value in W / m\eqn{^2} of class \code{units}
}
\description{
S_r: longwave re-radiation (W / m^2)
}
\details{
\deqn{S_\mathrm{r} = 2 \sigma \alpha_\mathrm{l} T_\mathrm{air} ^ 4}{S_r = 2 \sigma \alpha_l T_air ^ 4 }

The factor of 2 accounts for re-radiation from both leaf surfaces (Foster and Smith 1986). \cr
\cr
\tabular{lllll}{
\emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
\eqn{\alpha_\mathrm{l}}{\alpha_l} \tab \code{abs_l} \tab absorbtivity of longwave radiation (4 - 80 \eqn{\mu}m) \tab none \tab 0.97\cr
\eqn{T_\mathrm{air}}{T_air} \tab \code{T_air} \tab air temperature \tab K \tab 298.15\cr
\eqn{\sigma} \tab \code{s} \tab Stefan-Boltzmann constant \tab W / (m\eqn{^2} K\eqn{^4}) \tab 5.67e-08
}

Note that leaf absorbtivity is the same value as leaf emissivity
}
\examples{

library(tealeaves)

cs <- make_constants()
ep <- make_enviropar()
lp <- make_leafpar()

T_leaf <- set_units(298.15, K)

tealeaves:::.get_Sr(T_leaf, c(cs, ep, lp))

}
\references{
Foster JR, Smith WK. 1986. Influence of stomatal distribution on transpiration in low-wind environments. Plant, Cell & Environment 9: 751-9.
}
