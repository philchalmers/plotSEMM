#' Graphing Nonlinear Relations Among Latent Variables from Structural Equation Mixture Models
#'
#' Contains a graphical user interface to generate the
#' diagnostic plots proposed by Bauer (2005) and Pek & Chalmers (2015) to investigate
#' nonlinear latent variable interactions in latent regression models.
#'
#' Creates plots which accompany Bauers (2005) semiparametric method of modeling
#' Structural Equation Mixture Models (SEMMs) by allowing researchers to visualize
#' potential nonlinear relationships between a latent predictor and outcome. Additionally,
#' a graphical user interface (GUI) is available for interactive use and is found in the function
#' \code{\link{plotSEMM_GUI}}.
#'
#' @name plotSEMM
#' @docType package
#' @useDynLib plotSEMM
#' @title Graphing Nonlinear Relations Among Latent Variables from Structural Equation Mixture Models
#' @author Bethany Kok and Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#' @references
#' Pek, J. & Chalmers, R. P. (2015). Diagnosing Nonlinearity With Confidence Envelopes for a
#' Semiparametric Approach to Modeling Bivariate Nonlinear Relations Among Latent Variables.
#' \emph{Structural Equation Modeling, 22}, 288-293. \doi{10.1080/10705511.2014.937790}
#'
#' Pek, J., Chalmers, R. P., Kok B. E., & Losardo, D. (2015). Visualizing Confidence Bands for
#' Semiparametrically Estimated Nonlinear Relations among Latent Variables.
#' \emph{Journal of Educational and Behavioral Statistics, 40}, 402-423. \doi{10.3102/1076998615589129}
#' @keywords package
#' @importFrom methods is
#' @importFrom utils read.table
#' @import shiny MplusAutomation plotrix Rcpp plyr stats graphics
NULL
