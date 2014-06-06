examplePlot <- function(){
    cex <- 2
    plot(c(0,120), c(10,50), axes=FALSE, frame.plot=FALSE, type='n', xlab='', ylab='')
    
    draw.circle(20, 35, 8)
    text(20, 35, expression(eta[1]), cex=cex)
    draw.circle(80, 35, 8)
    text(80, 35, expression(eta[2]), cex=cex)
    draw.circle(105, 35, 5)
    text(105, 35, expression(zeta), cex=cex)
    
    arrows(31,35,69,35, length=.2)
    text(47, 38, expression(beta[21]), cex=cex)
    arrows(99,35,91,35, length=.1)
    draw.arc(5, 35, 3, deg1=320, deg2=20)
    arrows(112.7, 35.98, 112.55, 35.92, length=.15)
    arrows(112.7, 34, 112.55, 34.06, length=.15)
    text(5, 38, expression(psi[11]), cex=cex)
    draw.arc(115, 35, 3, deg1=0, deg2=140)
    arrows(7.62,35.68, 7.635, 35.67, length=.15)
    arrows(7.6,34.3, 7.615, 34.31, length=.15)
    draw.arc(115, 35, 3, deg1=220, deg2=360)
    text(115, 38, expression(psi[22]), cex=cex)
    
    polygon(c(15,25,20), c(45,45,50))
    text(20,47, '1', cex=cex)
    polygon(c(15,25,20)+60, c(45,45,50))
    text(80,47, '1', cex=cex)
    arrows(20,44,20,40, length=.1)
    text(25,42, expression(alpha[1]), cex=cex)
    arrows(80,44,80,40, length=.1)
    text(85,42, expression(alpha[2]), cex=cex)
    
    polygon(c(15,25,25,15), c(15,15,20,20))
    text(20,17,expression(y[2]), cex=cex)
    polygon(c(15,25,25,15)-15, c(15,15,20,20))
    text(20-15,17,expression(y[1]), cex=cex)
    polygon(c(15,25,25,15)+15, c(15,15,20,20))
    text(20+15,17,expression(y[3]), cex=cex)
    
    polygon(c(75,85,85,75), c(15,15,20,20))
    text(80,17,expression(y[5]), cex=cex)
    polygon(c(75,85,85,75)-15, c(15,15,20,20))
    text(80-15,17,expression(y[4]), cex=cex)
    polygon(c(75,85,85,75)+15, c(15,15,20,20))
    text(80+15,17,expression(y[6]), cex=cex)
    
    arrows(20,30,20,21, length=.2)
    arrows(20,30,20-15,21, length=.2)
    arrows(20,30,20+15,21, length=.2)
    
    arrows(80,30,80,21, length=.2)
    arrows(80,30,80-15,21, length=.2)
    arrows(80,30,80+15,21, length=.2)
}

# Bootstrap ellipse function. Be very careful not to allocate huge objects to avoid memory issues
bs.CE <- function(read, x, alpha){
    ACOV <- read$tech3$paramCov.savedata
    nclass <- length(read$tech1[[1L]]) - 1L
    omitpars <- c()
    for(i in 1L:nclass){
        tmp <- read$tech1[[1L]][[i]]
        omitpars <- c(omitpars, unique(na.omit(as.numeric(tmp$theta))), 
            unique(na.omit(as.numeric(tmp$lambda))), unique(na.omit(as.numeric(tmp$nu))))
    }
    nomitpars <- sum(unique(omitpars) != 0) + nclass # also to remove latent residual vars count
    draws <- pickNdraws(ncol(ACOV) - nomitpars, alpha=alpha)
    iter <- 0
    is.variance <- logical(ncol(ACOV))
    cholL <- chol(ACOV)
    points <- 250
    
    #construct spcification model with real parameters as list
    spec <- read$tech1$parameterSpecification
    samplepars <- spec
    pars <- read$parameters$unstandardized
    opars <- colnames(spec[[1L]]$theta)
    lpars <- unique(pars[, 'param'][!(pars[, 'param'] %in% opars)])
    exo <- pars[pars[, 'param'] %in% lpars & pars[, 'paramHeader'] == 'Means' & 
                     pars[, 'LatentClass'] == 1, 'param']
    endo <- colnames(spec[[1L]]$lambda)
    endo <- endo[endo != exo]
    for(i in 1L:nclass){
        cls <- pars[pars[,'LatentClass'] == i, ]
        samplepars[[i]]$nu <- cls[cls[, 'paramHeader'] == 'Intercepts' & 
                                      cls[, 'param'] %in% opars, 'est']
        tmp <- cls[grepl('\\.BY', cls[, 'paramHeader']), ]
        endoind <- grepl(endo, tmp[,'paramHeader'])
        lambdas <- samplepars[[i]]$lambda
        tmp <- cls[grepl('\\.BY', cls[, 'paramHeader']) & 
                       cls[, 'param'] %in% opars, 'est']
        lambdas[endoind, colnames(lambdas) == endo] <- tmp[endoind]
        lambdas[!endoind, colnames(lambdas) != endo] <- tmp[!endoind]
        samplepars[[i]]$lambda <- lambdas
        diag(samplepars[[i]]$theta) <- cls[cls[, 'paramHeader'] == 'Residual.Variances' & 
                                         cls[, 'param'] %in% opars, 'est']
        samplepars[[i]]$alpha <- c(cls[cls[, 'paramHeader'] == 'Means'
                                       & cls[, 'param'] == exo, 'est'],
                                   cls[cls[, 'paramHeader'] == 'Intercepts'
                                       & cls[, 'param'] == endo, 'est'])
        samplepars[[i]]$beta[2,1] <- cls[grepl('\\.ON', cls[, 'paramHeader']), 'est']
        diag(samplepars[[i]]$psi) <- c(cls[cls[, 'paramHeader'] == 'Variances'
                                         & cls[, 'param'] == exo, 'est'],
                                       cls[cls[, 'paramHeader'] == 'Residual.Variances'
                                           & cls[, 'param'] == endo, 'est'])
    }
    samplepars[[nclass + 1L]]$alpha.c <- c(pars[!(pars[,'param'] %in% 
                                                      c(opars, exo, endo)), 'est'], 0)
    
    while(TRUE){
        
        jitter <- rnorm(ncol(ACOV)) %*% cholL
        
        #load the jittered pars
        tmpmod <- loadMplusJitter(samplepars, spec, jitter)
        redraw <- FALSE
        for(i in 1L:nclass){
            tmp <- tmpmod[[i]]$psi
            tmp[is.na(tmp)] <- 0
            if(is(try(chol(tmp), silent=TRUE), 'try-error'))
                redraw <- TRUE
        }
        if(redraw) next
        
        #perform computations
        bs.pi <- exp(tmpmod[[nclass + 1]][[1]]) / sum(exp(tmpmod[[nclass + 1]][[1]]))
        bs.phi <- matrix(NA, length(x), nclass)
        for(i in 1L:nclass)
            bs.phi[,i] <- dnorm(x, tmpmod[[i]]$alpha[1L], sqrt(tmpmod[[i]]$psi[1L,1L]))
        tmp <- t(bs.pi * t(bs.phi))
        bs.pi_ <- tmp / rowSums(tmp)
        bs.y <- numeric(length(x))
        for(i in 1L:nclass)
            bs.y <- bs.y + bs.pi_[,i] * (tmpmod[[i]]$alpha[2L] + 
                                            tmpmod[[i]]$beta[2L,1L] * x)
        if(iter == 0L){
            lb.CE <- ub.CE <- bs.y
        } else {
            lb.CE[lb.CE > bs.y] <- bs.y[lb.CE > bs.y]
            ub.CE[ub.CE < bs.y] <- bs.y[ub.CE < bs.y]
        }
        
        #increment and break
        iter <- iter + 1
        if(iter == draws) break
    }
    
    ret <- list(lb = lb.CE, ub = ub.CE)
    return(ret)
}

pickNdraws <- function(npars, alpha){ 
    #95% only for now
    if(alpha == .025){
        draws <- switch(as.character(npars), 
                        "1" = 25,
                        "2" = 85,
                        "3" = 232,
                        "4" = 577,
                        "5" = 1353,
                        "6" = 3047,
                        "7" = 6665,
                        "8" = 14255,
                        "9" = 29945,
                        "10" = 61976,
                        "11" = 126671,
                        "12" = 256130,
                        "13" = 513079,
                        "14" = 1019382,
                        "15" = 2010562,
                        "16" = 3939636,
                        "17" = 7674248,
                        "18" = 14868770,
                        "19" = 28667700,
                        "20" = 55024760,
                        "21" = 105117600,
                        "22" = 200272600,
                        "23" = 379989400,
                        "24" = 718584600,
                        "25" = 1354673000,
                        "26" = 2546395000,
                        "27" = 4773392000,
                        "28" = 8924985000,
                        "29" = 16646750000,
                        "default" = stop('bootstrap CIs only supported in models with 8 to 29 parameters'))
    } else {
        draws <- switch(as.character(npars), 
                        "1" = 12,
                        "2" = 39,
                        "3" = 98,
                        "4" = 228,
                        "5" = 504,
                        "6" = 1077,
                        "7" = 2244,   
                        "8" = 4586,
                        "9" = 9232,
                        "10" = 18352,
                        "11" = 36095,
                        "12" = 70351,
                        "13" = 136042,
                        "14" = 261257,
                        "15" = 498651,
                        "16" = 946541,
                        "17" = 1787852,
                        "18" = 3361809,
                        "19" = 6295567,
                        "20" = 11745310,
                        "21" = 21836900,
                        "22" = 40469420,
                        "23" = 74777710,
                        "24" = 137789200,
                        "25" = 253241500,
                        "26" = 464303700,
                        "27" = 849338400,
                        "28" = 1550345000,
                        "29" = 2824215000,
                        "default" = stop('bootstrap CIs only supported in models with 8 to 29 parameters'))
    }
        
    draws
}
    
loadMplusJitter <- function(samplepars, spec, jitter){
    for(g in 1:length(samplepars)){
        for(i in 1:length(samplepars[[g]])){
            tmp <- samplepars[[g]][[i]] 
            pick <- spec[[g]][[i]] != 0 & !is.na(spec[[g]][[i]])
            tmp[pick] <- tmp[pick] + jitter[spec[[g]][[i]][pick]]
            samplepars[[g]][[i]] <- tmp
        }
    }
    samplepars
}