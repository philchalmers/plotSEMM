#' Graphing nonlinear relations among latent variables from Structural Equation Mixture Models
#'
#' Contains functions \code{\link{plotSEMM_setup}}, \code{\link{plotSEMM_contour}},
#' and \code{\link{plotSEMM_probability}}.
#' Creates plots which accompany Bauers (2005) semiparametric method of modeling
#' Structural Equation Mixture Models (SEMMs) by allowing researchers to visualize
#' potential nonlinear relationships between a latent predictor and outcome. Additionally,
#' a graphical user interface (GUI) is available for interactive use and is found in the function
#' \code{\link{plotSEMM_GUI}}.
#'
#' @name plotSEMM
#' @docType package
#' @useDynLib plotSEMM
#' @title Graphing nonlinear relations among latent variables from Structural Equation Mixture Models
#' @author Bethany Kok and Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#' @references
#' Bauer, D.J. (2005). A semiparametric approach to modeling nonlinear relations among latent variables.
#' Structural Equation Modeling: A Multidisciplinary Journal, 12(4), 513-535.
#'
#' Pek, J. & Chalmers, R. P. (in press). Confidence Envelopes for a Semiparametric
#' Approach to Modeling Bivariate Nonlinear Relations among Latent Variables.
#'
#' Pek, J., Losardo, D., & Bauer, D. J. (2011). Confidence intervals for a semiparametric
#' approach to modeling nonlinear relations among latent variables. Structural Equation
#' Modeling, 18 , 537-553.
#'
#' Pek, J., Sterba, S. K., Kok, B. E., & Bauer, D. J. (2009). Estimating and visualizing non-linear
#' relations among latent variables: A semiparametric approach. Multivariate
#' Behavioral Research, 44 , 407-436.
#' @keywords package
# @importFrom stats anova residuals
# @importFrom MASS ginv
#' @import shiny MplusAutomation plotrix Rcpp plyr stats
# @exportMethod anova
# @exportMethod residuals
# @exportMethod summary
NULL
