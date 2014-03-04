\name{plotSEMM_legend}
\alias{plotSEMM_legend}
\title{ Legend function for plotSEMM plots}
\description{
  Requires \code{plotSEMM_setup} be run first. Generates a legend that identifies different classes and the composite between-class estimate for the \code{plotSEMM_contour} and \code{plotSEMM_probability} plots.  Can be used in place of the automatically-generated legends included in \code{plotSEMM_contour} and \code{plotSEMM_probability}.
}
\usage{
plotSEMM_legend(lnty, lncol)
}
\arguments{
  \item{lnty}{ Determines the line types used for the class lines.  If no value is provided, defaults to 3.  See \code{\link{par}} for information about line type.}
  \item{lncol}{ Determines the line colors used for the class lines.  If no value is provided, defaults to 1.  See \code{\link{par}} for information about line type. }
}
\value{
  None. 
}
\references{ \url{http://www.bethanykok.com/plotSEMM.html} }
\author{ Bethany E. Kok, Jolynn Pek, Sonya Sterba and Dan Bauer\cr

Maintainer: Bethany E. Kok <bethanyk@unc.edu> }

\seealso{\code{\link{plotSEMM_setup}}, \code{\link{plotSEMM_contour}}, \code{\link{plotSEMM_probability}} }
\examples{
\dontrun{
## 2 class empirical example on positive emotions and heuristic processing in Pek, Sterba, Kok & Bauer (XXXX)
 
pi <- c(0.602, 0.398)

alpha1 <- c(3.529, 2.317)

alpha2 <- c(0.02, 0.336)

beta21 <- c(0.152, 0.053)

psi11 <- c(0.265, 0.265)

psi22 <- c(0.023, 0.023)

 
plotSEMM_setup(pi, alpha1, alpha2, beta21, psi11, psi22)


plotSEMM_legend()

plotSEMM_legend(6,2)
}
}
\keyword{ device }
\keyword{ misc }
