plotSEMM_setup2 <- function(setup, alpha = .025, boot = NULL){
    
    #only supports 2 or more classes 
    #requires Mplus read in file as input; setup <- read.plotSEMM_wACOV(read)

    nclass <- classes <- setup$nclass
    nparam <- setup$nparm; acov <- setup$acov; loc <- setup$loc
    c_loc <- loc$c_loc; alpha_loc <- loc$alpha_loc; beta_loc <- loc$beta_loc; 
    psi_loc <- loc$psi_loc
    locations <- c(c_loc,alpha_loc,beta_loc,psi_loc[seq(from=1, to=length(psi_loc), by=2)])
    p <- nclass*(4)+(nclass-1)  #number of variables to be used later in generating curves
    p1 <- length(unique(locations))
    pars <- setup$pars
    alphaarray <- pars$alphaarray; psiarray <- pars$psiarray; 
    gamma <- betavec <- pars$betavec; ci_v <- pars$ci_v
    means <- setup$means
        
    sum_expi <- sum(exp(ci_v))
    pi_v <- exp(ci_v) / sum_expi
    
    # ----------------------------------------------------------------------------------------------------------------------
    # code for computing I(z) taken from older plotting code
    # ----------------------------------------------------------------------------------------------------------------------
    
    # these need to be in 2x2 matrices/ 2x1 vectors for computations
    
    alphaarray2 <- array(data = NA, c(2, 1, classes))
    betaarray <- array(data = NA, c(2, 2, classes))
    psiarray2 <- array(data = NA, c(2, 2, classes))
    
    for (i in 1:classes) {
        alphaarray2[, , i] <- matrix(c(alphaarray[1, i], alphaarray[2, i]), 2, 1, byrow = TRUE)
        betaarray[, , i] <- matrix(c(0, 0, betavec[i], 0), 2, 2, byrow = TRUE)
        psiarray2[, , i] <- matrix(c(psiarray[1, i], 0, 0, psiarray[2, i]), 2, 2, byrow = TRUE)
    }
    
    IMPCOV <- array(data = NA, c(2, 2, classes))
    IMPMEAN <- array(data = NA, c(2, 2, classes))
    
    for (i in 1:classes) {
        IMPCOV[, , i] <- solve(diag(x = 1, nrow = 2, ncol = 2) - betaarray[, , i]) %*% (psiarray2[, , i]) %*% t(solve(diag(x = 1, 
            nrow = 2, ncol = 2) - betaarray[, , i]))
        IMPMEAN[, , i] <- solve(diag(x = 1, nrow = 2, ncol = 2) - betaarray[, , i]) %*% (alphaarray2[, , i])
    }
    
    
    MuEta_1 <- vector(mode = "numeric", length = classes)
    MuEta_2 <- vector(mode = "numeric", length = classes)
    VEta_1 <- vector(mode = "numeric", length = classes)
    VEta_2 <- vector(mode = "numeric", length = classes)
    COVKSIETA <- vector(mode = "numeric", length = classes)
    
    for (i in 1:classes) {
        MuEta_1[i] = IMPMEAN[1, 1, i]
        MuEta_2[i] = IMPMEAN[2, 2, i]
        VEta_1[i] = IMPCOV[1, 1, i]
        VEta_2[i] = IMPCOV[2, 2, i]
        COVKSIETA[i] = IMPCOV[1, 2, i]
    }
    # upper and lower bounds for plots
    
    muEta1 <- 0
    muEta2 <- 0
    
    for (i in 1:classes) {
        
        muEta1 <- muEta1 + pi_v[i] * MuEta_1[i]
        muEta2 <- muEta2 + pi_v[i] * MuEta_2[i]
    }
    
    vEta1 <- 0
    vEta2 <- 0
    for (i in 1:classes) {
        for (j in 1:classes) {
            if (i < j) {
                vEta1 <- vEta1 + pi_v[i] * pi_v[j] * (MuEta_1[i] - MuEta_1[j])^2
                vEta2 <- vEta2 + pi_v[i] * pi_v[j] * (MuEta_2[i] - MuEta_2[j])^2
            }
        }
    }
    for (i in 1:classes) {
        
        vEta1 <- vEta1 + pi_v[i] * VEta_1[i]
        vEta2 <- vEta2 + pi_v[i] * VEta_2[i]
    }
    
    LEta1 = muEta1 - 3 * sqrt(vEta1)
    UEta1 = muEta1 + 3 * sqrt(vEta1)
    LEta2 = muEta2 - 3 * sqrt(vEta2)
    UEta2 = muEta2 + 3 * sqrt(vEta2)
    
    LB = min(LEta1, LEta2)
    UB = max(UEta1, UEta2)
    
    
    # computations for contour plot
    
    Eta1 <- seq(LEta1, UEta1, length = 47)
    Eta2 <- seq(LEta2, UEta2, length = 47)
    
    
    r <- vector(mode = "numeric", length = classes)
    
    for (i in 1:classes) {
        r[i] <- COVKSIETA[i]/sqrt(VEta_1[i] * VEta_2[i])
    }
    
    denKE <- function(Eta1, Eta2) {
        
        placeholder <- 0
        denKE_ <- matrix(data = 0, nrow = length(Eta1), ncol = classes)
        
        for (i in 1:classes) {
            z <- ((Eta1 - MuEta_1[i])^2)/VEta_1[i] + ((Eta2 - MuEta_2[i])^2)/VEta_2[i] - 2 * r[i] * (Eta1 - MuEta_1[i]) * (Eta2 - 
                MuEta_2[i])/sqrt(VEta_1[i] * VEta_2[i])
            denKE_[, i] <- (1/(2 * 22/7 * sqrt(VEta_1[i]) * sqrt(VEta_2[i]) * sqrt(1 - r[i]^2))) * exp(-z/(2 * (1 - r[i]^2)))
        }
        
        for (i in 1:classes) {
            placeholder <- placeholder + pi_v[i] * denKE_[, i]
        }
        
        denKE <- placeholder
    }
    
    z <- outer(Eta1, Eta2, denKE)
    
    # ---------------------------------------------------------------------------------------------------------------------- End
    # of code for computing I(z)
    # ----------------------------------------------------------------------------------------------------------------------
    
    x <- seq(LEta1, UEta1, length = 47)
    x2 <- seq(LEta2, UEta2, length = 47)
    
    phi <- array(data = 0, c(47, classes))
    for (i in 1:classes) {
        phi[, i] <- dnorm(x, mean = alphaarray[1, i], sd = sqrt(psiarray[1, i]))
    }
    
    
    a_pi <- array(data = 0, c(47, classes))
    a_pi2 <- array(data = 0, c(47, classes))
    for (i in 1:classes) {
        a_pi[, i] <- pi_v[i] * dnorm(x, mean = MuEta_1[i], sd = sqrt(VEta_1[i]))
        a_pi2[, i] <- pi_v[i] * dnorm(x2, mean = MuEta_2[i], sd = sqrt(VEta_2[i]))
    }
    
    sumpi <- array(data = 0, c(47, 1))
    sumpi2 <- array(data = 0, c(47, 1))
    for (i in 1:classes) {
        sumpi[, 1] <- sumpi[, 1] + a_pi[, i]
        sumpi2[, 1] <- sumpi2[, 1] + a_pi2[, i]
    }
    
    pi <- array(data = 0, c(47, classes))
    for (i in 1:classes) {
        pi[, i] <- a_pi[, i]/sumpi[, 1]
    }
    y <- 0
    for (i in 1:classes) {
        y <- y + pi[, i] * (alphaarray[2, i] + gamma[i] * x)
    }
    
    # Derivatives for delta method CIs
    D <- 0
    for (i in 1:classes) {
        D = D + exp(ci_v[i]) * phi[, i]
    }
    
    
    dalpha <- array(data = 0, c(47, classes))
    dphi <- array(data = 0, c(47, classes))
    dc <- array(data = 0, c(47, classes - 1))
    for (i in 1:classes) {
        for (j in 1:classes) {
            if (i != j) {
                dalpha[, i] <- dalpha[, i] + ((exp(ci_v[j]) * phi[, j] * ((alphaarray[2, i] - alphaarray[2, j]) + (gamma[i] - 
                  gamma[j]) * x)))
                dphi[, i] <- dphi[, i] + ((exp(ci_v[j]) * phi[, j] * ((alphaarray[2, i] - alphaarray[2, j]) + (gamma[i] - gamma[j]) * 
                  x)))
                
            }
        }
        dalpha[, i] <- (dalpha[, i] * exp(ci_v[i]) * phi[, i] * ((x - alphaarray[1, i])/psiarray[1, i])) * (1/D^2)
        dphi[, i] <- dphi[, i] * exp(ci_v[i]) * phi[, i] * (((x - alphaarray[1, i])^2 - 1)/psiarray[1, i]) * (1/(2 * psiarray[1, 
            i])) * (1/D^2)
    }
    
    for (i in 1:(classes - 1)) {
        for (j in 1:(classes)) {
            if (i != j) {
                dc[, i] <- dc[, i] + (exp(ci_v[j]) * phi[, j] * ((alphaarray[2, i] - alphaarray[2, j]) + (gamma[i] - gamma[j]) * 
                  x))
                
                
            }
        }
        dc[, i] <- dc[, i] * exp(ci_v[i]) * phi[, i] * (1/D^2)
    }
    
    dkappa <- array(data = 0, c(47, classes))
    dgamma <- array(data = 0, c(47, classes))
    for (i in 1:classes) {
        dkappa[, i] <- exp(ci_v[i]) * phi[, i]/D
        dgamma[, i] <- exp(ci_v[i]) * phi[, i] * x/D
    }
    
    
    ct <- 0
    varordered <- c()
    for (i in 1:classes) {
        varordered <- c(varordered, alpha_loc[i + ct], alpha_loc[i + 1 + ct], beta_loc[i])
        ct <- ct + 1
    }
    
    varordered <- c(varordered, c_loc)
    
    ct <- 0
    for (i in 1:classes) {
        varordered <- c(varordered, psi_loc[i + ct])
        ct <- ct + 1
    }
    
    acovd <- acov[varordered, varordered]
    
    
    deriv <- c()
    for (i in 1:classes) {
        deriv <- cbind(deriv, dalpha[, i], dkappa[, i], dgamma[, i])
    }
    
    deriv <- c(deriv, dc, dphi)
    
    deriv <- matrix(deriv, nrow = 47, ncol = p)
    
    se <- sqrt(diag(deriv %*% acovd %*% t(deriv)))
    q <- abs(qnorm(alpha/2, mean = 0, sd = 1))
    sq <- sqrt(qchisq(1 - alpha, p))
    
    # delta method nonsimultaneous confidence intervals
    lo <- y - q * se
    hi <- y + q * se
    
    # delta method simultaneous confidence intervals
    slo <- y - sq * se
    shi <- y + sq * se
    
    
    ct <- 0
    varorder <- c()
    alpha_loc1 <- vector(mode = "numeric", length = classes)
    kappa_loc <- vector(mode = "numeric", length = classes)
    psi_loc1 <- vector(mode = "numeric", length = classes)
    phi_loc <- vector(mode = "numeric", length = classes)
    for (i in 1:classes) {
        varorder <- c(varorder, alpha_loc[i + ct])
        alpha_loc1[i] <- alpha_loc[i + ct]
        psi_loc1[i] <- psi_loc[i + ct]
        ct <- ct + 1
    }
    
    ct <- 0
    for (i in 2:(classes + 1)) {
        varorder <- c(varorder, alpha_loc[i + ct])
        kappa_loc[i - 1] <- alpha_loc[i + ct]
        phi_loc[i - 1] <- psi_loc[i + ct]
        ct <- ct + 1
    }
    
    varorder <- c(varorder, beta_loc)
    
    # ct <- 0 equal <- NULL for (i in 3:classes*2) { if (!identical(psi_loc[1],psi_loc[i+ct]) &
    # !identical(psi_loc[2],psi_loc[i+ct+1])) equal<-TRUE else equal<- FALSE ct <- ct + 1 }
    
    # equal <- NULL if (identical(means[psi_loc[1]],means[psi_loc[3]]) & identical(means[psi_loc[2]],means[psi_loc[4]]))
    # equal<-TRUE else equal<- FALSE
    
    equal <- NULL
    if (identical(means[psi_loc[1]], means[psi_loc[3]])) 
        equal <- TRUE else equal <- FALSE
    
    # if (!equal) { for(i in 1:(classes-1)){ for(i in 1:classes) { varorder <-c(varorder,psi_loc[i+ct]) alpha_loc1[i] <-
    # alpha_loc[i+ct] psi_loc1[i] <- psi_loc[i+ct] ct <- ct+1} } }
    
    ct <- 0
    if (!equal) {
        for (i in 1:(classes)) {
            varorder <- c(varorder, psi_loc[i + ct])
            ct <- ct + 1
        }
    }
    
    if (equal) {
        varorder <- c(varorder, psi_loc[1])
    }
    
    varorder <- c(varorder, c_loc)
    
    # selecting submatrices of acov vector of variable positions
    
    acov0 <- acov[varorder, varorder]
    means0 = means[varorder]
    
    # generating the data
    draws = 1000
    L = chol(acov0)  #chol decomposition of acov
    Z = rnorm(matrix(0, draws, length(means0)))  #random normal variates
    Z = matrix(Z, draws, length(means0))  #placing in matrix form
    Sim_est = Z %*% L + matrix(1, draws, 1) %*% means0  #getting simulated values
    
    # not seeing the point of this code.  the Missing variable isn't used elsewhere missing <- 0 for (i in 1:nrow(Sim_est)){ if
    # (Sim_est[i,7]<0) missing = missing+1 }
    
    yall <- NULL
    
    if (!equal) {
        for (i in seq(LB, UB, length = 47)) {
            yy <- 0
            placeholder <- 0
            for (j in 1:classes) {
                if (j < classes) {
                  placeholder = placeholder + exp(Sim_est[, classes * 4 + j]) * dnorm(i, mean = Sim_est[, j], sd = sqrt(Sim_est[, 
                    classes * 3 + j]))
                } else {
                  placeholder = placeholder + dnorm(i, mean = Sim_est[, classes], sd = sqrt(Sim_est[, classes * 4]))
                }
            }
            
            for (k in 1:classes) {
                if (k < classes) {
                  yy = yy + (exp(Sim_est[, classes * 4 + k]) * dnorm(i, mean = Sim_est[, k], sd = sqrt(Sim_est[, classes * 3 + 
                    k])) * (Sim_est[, classes + k] + Sim_est[, classes * 2 + k] * i))/placeholder
                } else {
                  yy = yy + (dnorm(i, mean = Sim_est[, classes], sd = sqrt(Sim_est[, classes * 4])) * (Sim_est[, classes * 2] + 
                    Sim_est[, classes * 3] * i))/placeholder
                }
            }
            
            yall = rbind(yall, yy)
        }
    }
    
    if (equal) {
        for (i in seq(LB, UB, length = 47)) {
            
            yy <- 0
            placeholder <- 0
            for (j in 1:classes) {
                if (j < classes) {
                  placeholder = placeholder + exp(Sim_est[, classes * 3 + 1 + j]) * dnorm(i, mean = Sim_est[, j], sd = sqrt(Sim_est[, 
                    classes * 3 + 1]))
                } else {
                  placeholder = placeholder + dnorm(i, mean = Sim_est[, classes], sd = sqrt(Sim_est[, classes * 3 + 1]))
                }
            }
            
            for (k in 1:classes) {
                if (k < classes) {
                  yy = yy + (exp(Sim_est[, classes * 3 + 1 + k]) * dnorm(i, mean = Sim_est[, k], sd = sqrt(Sim_est[, classes * 
                    3 + 1])) * (Sim_est[, classes + k] + Sim_est[, classes * 2 + k] * i))/placeholder
                } else {
                  yy = yy + (dnorm(i, mean = Sim_est[, classes], sd = sqrt(Sim_est[, classes * 3 + 1])) * (Sim_est[, classes * 
                    2] + Sim_est[, classes * 3] * i))/placeholder
                }
            }
            
            
            
            yall = rbind(yall, yy)
        }
    }
    
    
    
    # sorting before getting upper and lower CI values
    for (i in 1:nrow(yall)) {
        yall[i, ] = sort(yall[i, ])
    }
    
    LCL = (alpha/2) * draws
    UCL = (1 - (alpha/2)) * draws
    
    LCLall = yall[, LCL]
    UCLall = yall[, UCL]
    
    Ksi <- Eta1
    Eta <- Eta2
    denKsi <- sumpi
    denEta <- sumpi2
    post <- pi
    pKsi <- a_pi
    pEta <- a_pi2
    
    etahmat <- matrix(data = 0, nrow = length(Ksi), ncol = classes)
    for (i in 1:classes) {
        etahmat[, i] <- alphaarray[2, i] + gamma[i] * Ksi
    }
    
    etah_ <- y
    lo_ <- lo
    hi_ <- hi
    slo_ <- slo
    shi_ <- shi
    LCLall_ <- LCLall
    UCLall_ <- UCLall   
    
    bs_lo <- bs_high <- 0
    if(!is.null(boot)){
        bs <- bs.CI(boot, x=x)
        bs_lo <- bs$lb
        bs_high <- bs$ub
    }
    
    SEMLIdatapks <- data.frame(Ksi, Eta, denKsi, denEta, etah_, I(etahmat), 
                               I(z), classes, I(post), I(pKsi), I(pEta), LCLall_,
                               UCLall_, lo_, hi_, slo_, shi_, x, alpha=alpha, setup2=TRUE,
                               boot=!is.null(boot), bs_lo, bs_high)
    SEMLIdatapks
}
