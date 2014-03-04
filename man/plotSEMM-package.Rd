\name{plotSEMM-package}
\alias{plotSEMM-package}
\alias{plotSEMM}
\docType{package}
\title{
Graphing nonlinear relations among latent variables from Structural Equation Mixture Models
}
\description{
Contains functions \code{\link{plotSEMM_setup}}, \code{\link{plotSEMM_contour}}, \code{\link{plotSEMM_probability}} and \code{\link{plotSEMM_legend}}.  Creates plots which accompany Bauers (2005) semiparametric method of modeling Structural Equation Mixture Models (SEMMs) by allowing researchers to visualize potential nonlinear relationships between a latent predictor and outcome.
}
\details{
\tabular{ll}{
Package: \tab plotSEMM\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2008-03-11\cr
License: \tab GPL (>= 2)\cr
}
Contains four functions:\cr
	\code{\link{plotSEMM_setup}} takes and organizes user input generated from SEMM software such as Mplus (Muthen & Muthen, 2007), Mx (Neale, Boker, Xie & Maes, 2004) or MECOSA (Arminger, Wittenberg, & Schepers, 1996) in Gauss. The user needs to specify 6 vectors of model estimates, each containing K elements where K is the number of classes estimated. These vectors are specified to include (1) the marginal class probabilities, \code{pi} (2) the class means for the latent predictor, \code{alpha1}; (3) the intercepts and (4) slopes from the within-class regression of the latent outcome on the latent predictor, \code{alpha2} and \code{beta21}; (5) the within class variances of the latent predictor, \code{psi11}; and (6) the within-class residual variances of the latent outcome, \code{psi22}.\cr 
	\code{\link{plotSEMM_contour}} generates (a) the potential nonlinear regression function; (b) bivariate distribution of the latent variables; (c) marginal distributions of the latent variables; (d) within class linear regression functions; and (e) within class marginal distributions for the latent variables.\cr
	\code{\link{plotSEMM_probability}} generates a plot which expresses the mixing probabilities for each latent class conditioned on the exogenous latent variable, working from the data structure provided by \code{plotSEMM_setup}.\cr
	\code{\link{plotSEMM_legend}} generates an optional legend to accompany the above plots.\cr
}
\author{
Bethany E. Kok, Jolynn Pek, Sonya Sterba and Dan Bauer\cr

Maintainer: Bethany E. Kok \email{bethanyk@unc.edu}
}
\references{
Bauer, D.J. (2005). A semiparametric approach to modeling nonlinear relations among latent variables. Structural Equation Modeling: A Multidisciplinary Journal, 12(4), 513-535.\cr

Pek, J., Sterba, S., Kok, B.E. & Bauer, D.J. (in prep). Visualizing nonlinear relations among latent variables: An online plotting utility and R package. \cr

\url{http://www.bethanykok.com/plotSEMM.html}
}

\keyword{ package }

