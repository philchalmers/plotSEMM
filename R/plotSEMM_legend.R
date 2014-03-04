plotSEMM_legend <- function(lnty = 3, lncol = 1) {
    
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
 
