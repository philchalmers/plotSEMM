plotSEMM_ci <- function(SEMLIdatapks, linesearch, lnty = 3, lncol = 1) {
    
    #requires setup from plotSEMM_setup2
    if(!SEMLIdatapks$setup2[1L]) 
        stop('plotSEMM_ci requires a setup model that included the parameter ACOV matrix')
    def.par <- par(no.readonly = TRUE)
    pick <- SEMLIdatapks$denKsi > 0.02
    SEMLIdatapks <- SEMLIdatapks[pick, ]
    
    # plot(SEMLIdatapks$x,SEMLIdatapks$y,type='n',xlab='Latent Predictor', ylab='Latent Outcome')
    plot(SEMLIdatapks$Ksi, SEMLIdatapks$Eta, type = "n", xlab = "Latent Predictor", ylab = "Latent Outcome")
    lines(SEMLIdatapks$x, SEMLIdatapks$etah_, col = 1, lwd = 2)
    points(SEMLIdatapks$x, SEMLIdatapks$lo_, col = 2, lwd = 1.5, lty = 2)
    points(SEMLIdatapks$x, SEMLIdatapks$hi_, col = 2, lwd = 1.5, lty = 2)
    lines(SEMLIdatapks$x, SEMLIdatapks$slo_, col = 2, lwd = 1.5, lty = 2)
    lines(SEMLIdatapks$x, SEMLIdatapks$shi_, col = 2, lwd = 1.5, lty = 2)
    points(SEMLIdatapks$x, SEMLIdatapks$LCLall_, col = 4, lwd = 1.5, lty = 3, pch = 4)
    points(SEMLIdatapks$x, SEMLIdatapks$UCLall_, col = 4, lwd = 1.5, lty = 3, pch = 4)
    legend = c("Aggregate Function", "Delta Method 95% Confidence Interval", "Delta Method 95% Confidence Envelope", 
               "Bootstrap 95% Confidence Interval")
    lwd = c(2, 1, 1, 1)
    lty = c(1, 0, 2, 0) 
    pch = c(NA, 1, NA, 4)
    col = c(1, 2, 2, 4)
    if(SEMLIdatapks$boot[1]){
        points(SEMLIdatapks$x, SEMLIdatapks$bs_lo, col = 5, lwd = 1.5, lty = 3, pch = 4)
        points(SEMLIdatapks$x, SEMLIdatapks$bs_high, col = 5, lwd = 1.5, lty = 3, pch = 4)        
        legend <- c(legend, 'Bootstrap 95% Confidence Envelope')
        lwd <- c(lwd, 1); lty <- c(lty, 2); pch = c(pch, 4); col = c(col, 5)
    }
    legend("bottomleft", legend = legend, lwd = lwd, lty = lty, 
           pch = pch, col = col, bty = "n")
    if(linesearch){
        search <- attr(SEMLIdatapks, 'search')
        if(search$linearity == 1){
            x <- SEMLIdatapks$x
            lines(c(x[1], x[length(x)]), c(search$y1, search$y2), col = 'green', lwd=4)
            if(SEMLIdatapks$boot[1]){
                search <- attr(SEMLIdatapks, 'search.bs')
                lines(c(x[1], x[length(x)]), c(search$y1, search$y2), col = 'yellow', lwd=4)
            }
        } else {
            #probably should add text or something to the plot saying search didn't find anything
        }
    }
}