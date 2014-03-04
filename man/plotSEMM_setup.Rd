\name{plotSEMM_setup}
\alias{plotSEMM_setup}
\title{Set up function for plotSEMM}
\description{
  Takes user input generated from SEMM software such as Mplus (Muthen & Muthen, 2007), Mx (Neale, Boker, Xie & Maes, 2004) or MECOSA (Arminger, Wittenberg, & Schepers, 1996) in Gauss and generates model predicted data for processing in graphing functions \code{plotSEMM_contour} and \code{plotSEMM_probability}.
}
\usage{
plotSEMM_setup(pi, alpha1, alpha2, beta21, psi11, psi22)
}
\arguments{
  \item{pi}{ Vector: \emph{K} marginal class probabilities. }
  \item{alpha1}{ Vector: \emph{K} means of the latent predictor. }
  \item{alpha2}{ Vector: \emph{K} inercepts slopes from the within-class regression of the latent outcome on the latent predictor. }
  \item{beta21}{ Vector: \emph{K} slopes from the within-class regression of the latent outcome on the latent predictor.}
  \item{psi11}{ Vector: \emph{K} within-class variances of the latent predictor. }
  \item{psi22}{ Vector: \emph{K} within-class variances of the latent outcome.}
}
\details{
  All the parameter estimates required by the arguments are generated from software with the capability of estimating SEMMs. 

}
\value{
  None.
}
\references{ \url{http://www.bethanykok.com/plotSEMM.html} }
\author{ Bethany E. Kok, Jolynn Pek, Sonya Sterba and Dan Bauer }

\seealso{ \code{\link{plotSEMM_contour}},\code{\link{plotSEMM_probability}}}
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



}
}
\keyword{ dplot }
\keyword{ data }
\keyword{ manip }
\keyword{ array }

