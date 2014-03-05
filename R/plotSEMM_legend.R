#' Legend function for plotSEMM plots
#' 
#' Requires \code{plotSEMM_setup} be run first. Generates a legend that identifies
#' different classes and the composite between-class estimate for the 
#' \code{plotSEMM_contour} and \code{plotSEMM_probability} plots.  
#' Can be used in place of the automatically-generated legends included in 
#' \code{plotSEMM_contour} and \code{plotSEMM_probability}.
#' 
#' @aliases plotSEMM_legend
#' @param SEMLIdatapks object returned from \code{\link{plotSEMM_setup}}
#' @param lnty Determines the line types used for the class lines.  
#'   If no value is provided, defaults to 3.  See \code{\link{par}} for information
#'   about line type.
#' @param lncol Determines the line colors used for the class lines.  If no value is 
#'   provided, defaults to 1.  See \code{\link{par}} for information about line type. 
#' @author Bethany Kok and Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#' @keywords device misc
#' @export plotSEMM_legend
#' @seealso \code{\link{plotSEMM_setup}}, \code{\link{plotSEMM_contour}}, 
#'   \code{\link{plotSEMM_probability}} 
#' @examples 
#' \dontrun{ 
#' ## code for latent variables with two classes
#' # 2 class empirical example on positive emotions and heuristic processing 
#' #   in Pek, Sterba, Kok & Bauer (XXXX)
#'
#' pi <- c(0.602, 0.398)
#' 
#' alpha1 <- c(3.529, 2.317)
#' 
#' alpha2 <- c(0.02, 0.336)
#' 
#' beta21 <- c(0.152, 0.053)
#' 
#' psi11 <- c(0.265, 0.265)
#' 
#' psi22 <- c(0.023, 0.023)
#' 
#' 
#' plotobj <- plotSEMM_setup(pi, alpha1, alpha2, beta21, psi11, psi22)
#' 
#' 
#' plotSEMM_legend(plotobj)
#' 
#' plotSEMM_legend(plotobj,6,2)
#' }
plotSEMM_legend <- function(SEMLIdatapks, lnty = 3, lncol = 1) {
    
    def.par <- par(no.readonly = TRUE)  # save default, for resetting... 
    
    classes <- SEMLIdatapks$classes[1]
    
    par(cex = 0.75)
    
    xcoords <- c(0:classes)
    
    plot(xcoords, xcoords, type = "n", axes = FALSE, xlab = "", ylab = "")
    
    text <- vector(mode = "character", length = classes + 1)
    text[1] <- "Estimate"
    
    lwd1 <- vector(mode = "numeric", length = (classes + 1))
    lwd1[1] <- 2
    
    for (i in 2:(classes + 1)) {
        text[i] <- paste("Class", (i - 1), sep = " ")
        lwd1[i] <- 1
    }
    
    lty1 <- vector(mode = "numeric", length = (classes + 1))
    
    col1 <- vector(mode = "numeric", length = (classes + 1))
    
    col1[1] <- 1
    lty1[1] <- 1
    for (i in 2:(classes + 1)) {
        col1[i] <- (i + lncol - 1)
        lty1[i] <- (i + lnty - 1)
    }
    
    legend(0, classes, legend = text, horiz = TRUE, lwd = lwd1, lty = lty1, col = col1, , bty = "n")
    
    par(def.par)  #reset to default 
}
 
