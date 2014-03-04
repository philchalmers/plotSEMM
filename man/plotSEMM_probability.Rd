\name{plotSEMM_probability}
\alias{plotSEMM_probability}
\title{ Probability plot }
\description{
  Requires \code{plotSEMM_setup} be run first. Generates a plot which expresses the mixing probabilities for each latent class conditioned on the latent predictor.  
}
\usage{
plotSEMM_probability(EtaName, lnty, lncol, title, leg)
}
\arguments{
  \item{EtaName}{ Label of the latent predictor.  If no value is provided, defaults to Eta1. }
  \item{lnty}{ Determines the line types used for the class lines.  If no value is provided, defaults to 3.  See \code{\link{par}} for information about line type. }
  \item{lncol}{ Determines the line colors used for the class lines.  If no value is provided, defaults to 1.  See \code{\link{par}} for information about line type. }
  \item{title}{ Titles the graph. }
  \item{leg}{ Logical variable.  If TRUE, a legend accompanies the graph.  If FALSE, no legend appears.  Defaults to TRUE. }
}
\value{
  None.
}
\references{  \url{http://www.bethanykok.com/plotSEMM.html} }
\author{ Bethany E. Kok, Jolynn Pek, Sonya Sterba and Dan Bauer }
\seealso{ \code{\link{plotSEMM_setup}},\code{\link{plotSEMM_contour}} }
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

plotSEMM_probability()

plotSEMM_probability(EtaName = "Latent Predictor", lnty = 2, title = "Probability")

}
}

\keyword{ hplot }
\keyword{ color }
