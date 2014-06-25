plotSEMM_ci <- function(SEMLIdatapks, linesearch, lnty = 3, lncol = 1, deltaci=TRUE, 
                        deltace=TRUE, bsci=TRUE, ninty_five = TRUE) {
    
    #requires setup from plotSEMM_setup2
    if(!SEMLIdatapks$setup2[1L]) 
        stop('plotSEMM_ci requires a setup model that included the parameter ACOV matrix')
    def.par <- par(no.readonly = TRUE)
    pick <- SEMLIdatapks$denKsi > 0.02
    SEMLIdatapks <- SEMLIdatapks[pick, ]
    
    # plot(SEMLIdatapks$x,SEMLIdatapks$y,type='n',xlab='Latent Predictor', ylab='Latent Outcome')
    plot(SEMLIdatapks$Ksi, SEMLIdatapks$Eta, type = "n", xlab = "Latent Predictor", ylab = "Latent Outcome")
    lines(SEMLIdatapks$x, SEMLIdatapks$etah_, col = 1, lwd = 2)
    if(deltaci){
        points(SEMLIdatapks$x, SEMLIdatapks$lo_, col = 2, lwd = 1.5, lty = 2)
        points(SEMLIdatapks$x, SEMLIdatapks$hi_, col = 2, lwd = 1.5, lty = 2)
    }
    if(deltace){
        lines(SEMLIdatapks$x, SEMLIdatapks$slo_, col = 2, lwd = 1.5, lty = 2)
        lines(SEMLIdatapks$x, SEMLIdatapks$shi_, col = 2, lwd = 1.5, lty = 2)
    }
    if(bsci){
        points(SEMLIdatapks$x, SEMLIdatapks$LCLall_, col = 4, lwd = 1.5, lty = 3, pch = 4)
        points(SEMLIdatapks$x, SEMLIdatapks$UCLall_, col = 4, lwd = 1.5, lty = 3, pch = 4)
    }
    if(ninty_five){
        legend = c("Aggregate Function", "Delta Method 95% Confidence Interval", "Delta Method 95% Confidence Envelope", 
                   "Bootstrap 95% Confidence Interval")
    } else {
        legend = c("Aggregate Function", "Delta Method 90% Confidence Interval", "Delta Method 90% Confidence Envelope", 
                   "Bootstrap 90% Confidence Interval")
    }
    lwd = c(2, 1, 1, 1)
    lty = c(1, 0, 2, 0) 
    pch = c(NA, 1, NA, 4)
    col = c(1, 2, 2, 4)
    if(SEMLIdatapks$boot[1]){
        lines(SEMLIdatapks$x, SEMLIdatapks$bs_lo, col = 4, lwd = 1.5, lty = 3, pch = 4)
        lines(SEMLIdatapks$x, SEMLIdatapks$bs_high, col = 4, lwd = 1.5, lty = 3, pch = 4)        
        if(ninty_five) legend <- c(legend, 'Bootstrap 95% Confidence Envelope')
        else legend <- c(legend, 'Bootstrap 90% Confidence Envelope')
        lwd <- c(lwd, 1); lty <- c(lty, 2); pch = c(pch, NA); col = c(col, 4)
    }
    legend("bottomleft", legend = legend, lwd = lwd, lty = lty, 
           pch = pch, col = col, bty = "n")
    if(linesearch){
        found <- FALSE
        search <- attr(SEMLIdatapks, 'search')
        if(length(search) && deltace){
            found <- TRUE
            nc <- ncol(search)
            lines(c(search[1,1], search[1,nc]), c(search[2,1], search[2,nc]), col = 'green', lwd=4)
        }
        if(SEMLIdatapks$boot[1]){            
            search <- attr(SEMLIdatapks, 'search.bs')
            if(length(search)){
                found <- TRUE
                nc <- ncol(search)
                lines(c(search[1,1], search[1,nc]), c(search[2,1], search[2,nc]), col = 'yellow', lwd=4)
            }   
        }
        if(!found)
            title('Linearity algorithm test did not find a suitable line within Confidence Ellipse(s)')
    }
}