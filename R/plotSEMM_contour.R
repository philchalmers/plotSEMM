plotSEMM_contour <- function(EtaN2 = Eta2, EtaN1 = Eta1, classinfo = TRUE, lnty = 3, lncol = 1, title = "", 
    leg = TRUE) {
    
    # This is to make all 3 graphs on 1 panel for contour
    def.par <- par(no.readonly = TRUE)  # save default, for resetting... 
    nf <- layout(matrix(c(1, 0, 2, 3), 2, 2, byrow = TRUE), c(3, 1), c(1, 3), TRUE)
    
    xlabel = deparse(substitute(EtaN1))
    if (substring(xlabel, 1, 1) == "\"") {
        xlabel = substring(xlabel, 2, nchar(xlabel) - 1)
    }
    
    ylabel = deparse(substitute(EtaN2))
    if (substring(ylabel, 1, 1) == "\"") {
        ylabel = substring(ylabel, 2, nchar(ylabel) - 1)
    }
    
    maintitle = deparse(substitute(title))
    if (substring(maintitle, 1, 1) == "\"") {
        maintitle = substring(maintitle, 2, nchar(maintitle) - 1)
    }
    
    
    # plot1 Exogenous
    par(mar = c(0, 4, 1, 1))
    plot(SEMLIdatapks$Ksi, SEMLIdatapks$denKsi, type = "l", xlab = "", ylab = "", main = maintitle, axes = FALSE)
    if (classinfo == TRUE) {
        for (i in 1:SEMLIdatapks$classes[1]) {
            lines(SEMLIdatapks$Ksi, SEMLIdatapks$pKsi[, i], lwd = 1, lty = (i + lnty), col = (i + lncol))
        }
    }
    
    # legend
    if (leg == TRUE) {
        classes <- SEMLIdatapks$classes[1]
        
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
        
        legend(x = "topright", legend = text, horiz = FALSE, lwd = lwd1, lty = lty1, col = col1, , bty = "n")
    }
    
    # plot3 contour
    par(mar = c(4, 4, 0, 0))
    plot(SEMLIdatapks$Ksi, SEMLIdatapks$Eta, type = "n", xlab = xlabel, ylab = ylabel, main = "")
    contour(SEMLIdatapks$Ksi, SEMLIdatapks$Eta, SEMLIdatapks$z, drawlabels = TRUE, add = TRUE, nlevels = 20)
    lines(SEMLIdatapks$Ksi, SEMLIdatapks$etah_, lwd = 2, lty = 1)
    if (classinfo == TRUE) {
        for (i in 1:SEMLIdatapks$classes[1]) {
            lines(SEMLIdatapks$Ksi, SEMLIdatapks$etahmat[, i], lwd = 1, lty = (i + lnty), col = (i + lncol))
        }
    }
    
    # plot Endogenous
    par(mar = c(4, 0, 1, 1))
    plot(SEMLIdatapks$denEta, SEMLIdatapks$Eta, type = "l", xlab = "", ylab = "", main = "", axes = FALSE)
    if (classinfo == TRUE) {
        for (i in 1:SEMLIdatapks$classes[1]) {
            lines(SEMLIdatapks$pEta[, i], SEMLIdatapks$Eta, lwd = 1, lty = (i + lnty), col = (i + lncol))
        }
    }
    
    
    
    par(def.par)  #reset to default
}
 
