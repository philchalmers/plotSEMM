plotSEMM_probability <- function(EtaName = Eta1, lnty = 3, lncol = 1, title = "", leg = TRUE) {
    
    # plot 2 probabilities and exogenous variable
    def.par <- par(no.readonly = TRUE)  # save default, for resetting... 
    nf <- layout(matrix(c(1, 2), 2, 1, byrow = TRUE), c(3, 1), c(1, 3), TRUE)
    layout.show(nf)  #make sure it's the format we want
    
    maintitle = deparse(substitute(title))
    if (substring(maintitle, 1, 1) == "\"") {
        maintitle = substring(maintitle, 2, nchar(maintitle) - 1)
    }
    
    # plot1 Exogenous
    par(mar = c(0, 4, 1, 1))
    plot(SEMLIdatapks$Ksi, SEMLIdatapks$denKsi, type = "l", xlab = "", ylab = "", main = maintitle, axes = FALSE)
    for (i in 1:SEMLIdatapks$classes[1]) {
        lines(SEMLIdatapks$Ksi, SEMLIdatapks$pKsi[, i], lwd = 1, lty = (i + lnty), col = (i + lncol))
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
    
    # plot2 Conditional Probabilities
    par(mar = c(5, 4, 0, 0))
    xlabel = deparse(substitute(EtaName))
    if (substring(xlabel, 1, 1) == "\"") {
        xlabel = substring(xlabel, 2, nchar(xlabel) - 1)
    }
    
    ylabel = paste(paste("Probability(Class|", xlabel, sep = ""), ")", sep = "")
    
    plot(SEMLIdatapks$Ksi, SEMLIdatapks$post[, 1], type = "n", xlab = xlabel, ylab = ylabel, main = "")
    for (i in 1:SEMLIdatapks$classes[1]) {
        lines(SEMLIdatapks$Ksi, SEMLIdatapks$post[, i], lwd = 1, lty = (i + lnty), col = (i + lncol))
    }
    
    
    par(def.par)  #reset to default 
}
 
