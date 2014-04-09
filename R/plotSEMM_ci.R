plotSEMM_ci <- function(SEMLIdatapks, linesearch, lnty = 3, lncol = 1) {
    
    #requires setup from plotSEMM_setup2
    if(!SEMLIdatapks$setup2[1L]) 
        stop('plotSEMM_ci requires a setup model that included the parameter ACOV matrix')
    def.par <- par(no.readonly = TRUE)
    
    # plot(SEMLIdatapks$x,SEMLIdatapks$y,type='n',xlab='Latent Predictor', ylab='Latent Outcome')
    plot(SEMLIdatapks$Ksi, SEMLIdatapks$Eta, type = "n", xlab = "Latent Predictor", ylab = "Latent Outcome")
    lines(SEMLIdatapks$x, SEMLIdatapks$etah_, col = 1, lwd = 2)
    points(SEMLIdatapks$x, SEMLIdatapks$lo_, col = 2, lwd = 1.5, lty = 2)
    points(SEMLIdatapks$x, SEMLIdatapks$hi_, col = 2, lwd = 1.5, lty = 2)
    lines(SEMLIdatapks$x, SEMLIdatapks$slo_, col = 2, lwd = 1.5, lty = 2)
    lines(SEMLIdatapks$x, SEMLIdatapks$shi_, col = 2, lwd = 1.5, lty = 2)
    points(SEMLIdatapks$x, SEMLIdatapks$LCLall_, col = 4, lwd = 1.5, lty = 3, pch = 4)
    points(SEMLIdatapks$x, SEMLIdatapks$UCLall_, col = 4, lwd = 1.5, lty = 3, pch = 4)
    legend("bottomleft", legend = c("Aggregate Function", "Delta Method 90% Confidence Interval", "Delta Method 90% Confidence Envelope", 
                                    "Bootstrap 90% Confidence Interval"), lwd = c(2, 1, 1, 1), lty = c(1, 0, 2, 0), 
           pch = c(NA, 1, NA, 4), col = c(1, 2, 2, 4), bty = "n")   
    
    if(linesearch){
        browser()
    }
}