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
    text(47, 38, expression(beta[12]), cex=cex)
    arrows(99,35,91,35, length=.1)
    draw.arc(5, 35, 3, deg1=320, deg2=20)
    text(5, 38, expression(psi[11]), cex=cex)
    draw.arc(115, 35, 3, deg1=0, deg2=140)
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

# Bootstrap function
bs.CI <- function(read){
    browser()
    ACOV <- read$tech3$paramCov.savedata
    draws <- pickNdraws(ncol(ACOV))
    
    
    #OLD CODE TO BE MODIFIED
    #Parametric bootstrap replicates
    draws <- 1425 #8 parameters requires 1425 draws
    draws_ <- draws + 100
    L <- chol(acov)
    Z <- rnorm(matrix(0,draws_,length(mean)))
    Z <- matrix(Z,draws_,length(mean))
    bs.estimates <-  Z%*%L + matrix(1,draws_,1)%*%as.matrix(mean,1,length(mean))
    
    #select bs replicates with no negative variances
    bs.estimates <- bs.estimates[bs.estimates[,8]>0,][1:draws,]
    
    #Bootstrapped aggregate function
    #mixing probabilities
    bs.c1.pi <- exp(bs.estimates[,7])/(exp(bs.estimates[,7])+exp(0))
    bs.c2.pi <- exp(0)/(exp(bs.estimates[,7])+exp(0))
    
    bs.c1.phi <-matrix(0,draws,points)
    bs.c2.phi <-matrix(0,draws,points)
    bs.D <-matrix(0,draws,points)
    bs.c1.pi_ <-matrix(0,draws,points)
    bs.c2.pi_ <-matrix(0,draws,points)
    bs.y <- matrix(0,draws,points)
    
    for(d in 1:draws){
        bs.c1.phi[d,] <- dnorm(x,mean=bs.estimates[d,1],sd=sqrt(bs.estimates[d,8]))
        bs.c2.phi[d,] <- dnorm(x,mean=bs.estimates[d,4],sd=sqrt(bs.estimates[d,8]))
        bs.D[d,] <- bs.c1.pi[d]*bs.c1.phi[d,] + bs.c2.pi[d]*bs.c2.phi[d,]
        bs.c1.pi_[d,] <- (bs.c1.pi[d]*bs.c1.phi[d,])/bs.D[d,]
        bs.c2.pi_[d,] <- (bs.c2.pi[d]*bs.c2.phi[d,])/bs.D[d,]
        
        #bootstrapped y function
        bs.y[d,] <- bs.c1.pi_[d,]*(bs.estimates[d,2]+bs.estimates[d,3]*x) +
            bs.c2.pi_[d,]*(bs.estimates[d,5]+bs.estimates[d,6]*x)  
    }
    
    #Taking min and max of each "slice" to form CE
    lb.CE <- matrix(0,1,points)
    ub.CE <- matrix(0,1,points)
    for (p in 1:points){
        lb.CE[p] <- min(bs.y[,p])
        ub.CE[p] <- max(bs.y[,p])
    }
    ret <- list(lb =lb.CE, ub = ub.CE)
    return(ret)
}

pickNdraws <- function(npars){ 
    #95% only
    draws <- switch(as.character(npars), 
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
    
    draws
}
    
