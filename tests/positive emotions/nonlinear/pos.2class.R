#Figure 3 & 4 of manuscript - nonlinear effect of PE on HP

# clear workspace
rm(list=ls()) 

#setwd("C:/Documents and Settings/jolynn/Desktop/Fall 2010/CI auto/examples/positive emotions/nonlinear")
setwd("C:/Documents and Settings/pek/Desktop/CEauto/example/positive emotions/nonlinear")

nclass <- 2 #user input
nparm <-23 #user input - can be used to check whether acov has correct dimensions
alpha<- 0.10 #user defined


acov_f <- "acov.dat"
means_f <- "result.dat"

#location of parameters from TECH1 is input by user
c_loc <- 23
alpha_loc <- c(15,16,20,21) #alpha1, alpha2 by class
beta_loc <- c(17,22) 
psi_loc <- c(18,19,18,19)  #psi1, psi2 by class

	
	classes <- nclass   #number of classes
locations <- c(c_loc,alpha_loc,beta_loc,psi_loc[1],psi_loc[3])
	p <- nclass*(4)+(nclass-1)  #number of variables to be used later in generating curves
p1<- length(unique(locations))

	acov_ <- matrix(scan(file=acov_f,what=numeric(0))) #inputting acov matrix
	acov <- matrix(0,nparm,nparm)
	acov[upper.tri(acov,diag=TRUE)] <- acov_  #full acov matrix
	acov<-acov+t(acov)-diag(diag(acov)) #making acov square

	means_<- matrix(scan(file=means_f,what=double(0))) #inputting mean vector
	means <-matrix(means_[1:nparm,1]) #mean vector


	alphaarray <- array(data = 0, c(2,classes))
		j <- 0 #modified to make sure it's correct
	for (i in 1:classes) {
		alphaarray[1,i] <- means[alpha_loc[i+j],1]
		alphaarray[2,i] <- means[alpha_loc[i+j+1],1] 
		j <- j+1	
	 	}

	psiarray <- array(data = 0, c(2,classes))
		j <- 0
	for (i in 1:classes) {
		psiarray[1,i] <- means[psi_loc[i+j],1]
		psiarray[2,i] <- means[psi_loc[i+j+1],1] 
		j <- j+1
		}

	betavec <- vector(length=classes)
	ci_v <- vector(length=classes)
	pi_v<-vector(length=classes)
	for (i in 1:classes) {
		betavec[i]<-means[beta_loc[i],1]
		if (nclass==1) ci_v[i] <- 1 else
		ci_v[i]<-means[c_loc[i],1] }

	ci_v[classes] <- 0

	sum_expi <- 0

	for(i in 1:classes) {
		sum_expi <- sum_expi + exp(ci_v[i]) }

	for(i in 1:classes) {
		pi_v[i]<-exp(ci_v[i])/sum_expi }

	gamma <- betavec


#----------------------------------------------------------------------------------------------------------------------
# code for computing I(z)
# taken from older plotting code
#----------------------------------------------------------------------------------------------------------------------

	# these need to be in 2x2 matrices/ 2x1 vectors for computations

	alphaarray2 <- array(data = NA ,c(2,1,classes))
	betaarray <- array(data = NA ,c(2,2,classes))
	psiarray2 <- array(data = NA ,c(2,2,classes))

	for(i in 1:classes) { 
		alphaarray2[,,i] <-matrix(c(alphaarray[1,i], alphaarray[2,i]), 2, 1, byrow=TRUE)
		betaarray[,,i] <- matrix(c(0,0,betavec[i],0), 2, 2, byrow=TRUE)
		psiarray2[,,i] <- matrix(c(psiarray[1,i],0,0,psiarray[2,i]), 
						2, 2, byrow=TRUE)
	  }

	IMPCOV <- array(data = NA, c(2, 2, classes))
	IMPMEAN <- array(data = NA, c(2, 2, classes))

	for(i in 1:classes) { 
		IMPCOV[,,i] <- solve(diag(x = 1, nrow = 2, ncol = 2)  - betaarray[,,i]) %*% (psiarray2[,,i]) %*% t(solve(diag(x = 1, nrow = 2, ncol = 2) - betaarray[,,i]))
		IMPMEAN[,,i] <- solve(diag(x = 1, nrow = 2, ncol = 2) - betaarray[,,i]) %*% (alphaarray2[,,i])
        } 


	MuEta_1 <- vector(mode = "numeric", length = classes)
	MuEta_2 <- vector(mode = "numeric", length = classes)
	VEta_1 <- vector(mode = "numeric", length = classes)
	VEta_2 <- vector(mode = "numeric", length = classes)
	COVKSIETA <- vector(mode = "numeric", length = classes)

	for(i in 1:classes) { 
		MuEta_1[i] = IMPMEAN[1,1,i] 
		MuEta_2[i] = IMPMEAN[2,2,i]
		VEta_1[i] = IMPCOV[1,1,i]
		VEta_2[i] = IMPCOV[2,2,i]
		COVKSIETA[i] = IMPCOV[1, 2,i]
					}
#upper and lower bounds for plots

muEta1 <- 0
muEta2 <-0

	for(i in 1:classes) {

		muEta1 <- muEta1 + pi_v[i]*MuEta_1[i] 
		muEta2 <- muEta2 + pi_v[i]*MuEta_2[i] }

vEta1 <- 0
vEta2 <- 0
	for(i in 1:classes) {
		for(j in 1:classes) {
			if (i < j) {
				vEta1 <- vEta1 + pi_v[i]*pi_v[j]*(MuEta_1[i]-MuEta_1[j])^2 
				vEta2 <- vEta2 + pi_v[i]*pi_v[j]*(MuEta_2[i]-MuEta_2[j])^2
				}
			}
}
	for(i in 1:classes) {

				vEta1 <- vEta1  + pi_v[i]*VEta_1[i]
				vEta2 <- vEta2  + pi_v[i]*VEta_2[i]
}

 LEta1 = muEta1 - 3*sqrt(vEta1)
 UEta1 = muEta1 + 3*sqrt(vEta1)
 LEta2 = muEta2 - 3*sqrt(vEta2)
 UEta2 = muEta2 + 3*sqrt(vEta2)

 LB = min(LEta1,LEta2)
 UB = max(UEta1, UEta2)


#computations for contour plot

	Eta1 <- seq(LEta1, UEta1, length=47) 
	Eta2 <- seq(LEta2, UEta2, length=47)


	r <- vector(mode = "numeric", length = classes)

	for(i in 1: classes) {
	r[i] <- COVKSIETA[i]/sqrt(VEta_1[i]*VEta_2[i])
	}

denKE <- function(Eta1, Eta2) {

	placeholder <- 0
	denKE_ <- matrix(data = 0, nrow = length(Eta1), ncol = classes)

	for(i in 1:classes) { 
		z <-  ((Eta1 - MuEta_1[i])^2)/VEta_1[i] + ((Eta2 - MuEta_2[i])^2)/VEta_2[i] - 2*r[i]*(Eta1 - MuEta_1[i])*(Eta2 - MuEta_2[i])/sqrt(VEta_1[i]*VEta_2[i])
		denKE_[,i] <- (1/(2*22/7*sqrt(VEta_1[i])*sqrt(VEta_2[i])*sqrt(1-r[i]^2))) * exp(-z/(2*(1-r[i]^2)))  }

	for (i in 1:classes) {
		placeholder <- placeholder + pi_v[i]*denKE_[,i] }

	denKE <- placeholder
}

z <-outer (Eta1, Eta2, denKE)

#----------------------------------------------------------------------------------------------------------------------
# End of code for computing I(z)
#----------------------------------------------------------------------------------------------------------------------

x <- seq(LEta1,UEta1,length=47)
x2 <- seq(LEta2,UEta2,length=47)

phi <-array(data = 0, c(47,classes))
	for(i in 1:classes) {
		phi[,i]<-dnorm(x,mean=alphaarray[1,i],sd=sqrt(psiarray[1,i]))
}


a_pi <-array(data=0,c(47,classes))
a_pi2 <-array(data=0,c(47,classes))
    for(i in 1:classes) {
        a_pi[,i]<- pi_v[i]*dnorm(x,mean=MuEta_1[i],sd=sqrt(VEta_1[i]))	
	  a_pi2[,i]<- pi_v[i]*dnorm(x2,mean=MuEta_2[i],sd=sqrt(VEta_2[i]))
}

sumpi <- array(data=0,c(47,1))
sumpi2 <- array(data=0,c(47,1))
    for(i in 1:classes) {
        sumpi[,1] <- sumpi[,1]+a_pi[,i]
	  sumpi2[,1] <- sumpi2[,1]+a_pi2[,i]
}

pi <- array(data=0,c(47,classes))
    for(i in 1:classes) {
        pi[,i] <- a_pi[,i]/sumpi[,1] } 
y <- 0
for (i in 1:classes) {
	y<-y+ pi[,i]*(alphaarray[2,i]+gamma[i]*x) }

#Derivatives for delta method CIs
D<-0
for(i in 1:classes) {
	D = D+ exp(ci_v[i])*phi[,i]}


dalpha <-array(data = 0, c(47,classes))
dphi <-array(data = 0, c(47,classes))
dc <-array(data = 0, c(47,classes-1))

	for(i in 1:classes) {
		for (j in 1:classes) {
			if(i != j) {
				dalpha[,i]<-dalpha[,i]+((exp(ci_v[j])*phi[,j]*((alphaarray[2,i]-alphaarray[2,j])+(gamma[i]-gamma[j])*x))) 
				dphi[,i]<- dphi[,i]+((exp(ci_v[j])*phi[,j]*((alphaarray[2,i]-alphaarray[2,j])+(gamma[i]-gamma[j])*x)))
			
			} 
		}
		dalpha[,i]<-(dalpha[,i]*exp(ci_v[i])*phi[,i]*((x-alphaarray[1,i])/psiarray[1,i]))*(1/D^2) 
		dphi[,i]<-dphi[,i]*exp(ci_v[i])*phi[,i]*(((x-alphaarray[1,i])^2-1)/psiarray[1,i])*(1/(2*psiarray[1,i]))*(1/D^2)
	}
if(nclass > 1)
	for(i in 1:(classes-1)) {
		for (j in 1:(classes)) {
			if(i != j) {
			dc[,i]<-dc[,i]+(exp(ci_v[j])*phi[,j]*((alphaarray[2,i]-alphaarray[2,j])+(gamma[i]-gamma[j])*x))


} }
			dc[,i]<-dc[,i]*exp(ci_v[i])*phi[,i]*(1/D^2)
}

dkappa <-array(data = 0, c(47,classes))
dgamma <-array(data = 0, c(47,classes))
	for(i in 1:classes) {
	dkappa[,i]<-exp(ci_v[i])*phi[,i]/D 
	dgamma[,i]<-exp(ci_v[i])*phi[,i]*x/D }


ct <-0
varordered <-c()
	for(i in 1:classes) {
		varordered <-c(varordered,alpha_loc[i+ct],alpha_loc[i+1+ct],beta_loc[i]) 
		ct <- ct+1}

varordered <-c(varordered,c_loc)

ct <-0
	for(i in 1:classes) {
		varordered <-c(varordered,psi_loc[i+ct]) 
		ct <- ct+1}

acovd <- acov[varordered,varordered]


deriv <-c()
	for(i in 1:classes) {
		deriv <-cbind(deriv,dalpha[,i],dkappa[,i],dgamma[,i]) }

deriv <-c(deriv,dc,dphi)

deriv <-matrix(deriv,nrow=47,ncol=p)

se<-sqrt(diag(deriv%*%acovd%*%t(deriv)))
q <- abs(qnorm(alpha/2,mean=0,sd=1))
sq <- sqrt(qchisq(1-alpha,p1))

# delta method nonsimultaneous confidence intervals
lo <- y - q*se
hi <- y + q*se

# delta method simultaneous confidence intervals
slo <- y - sq*se
shi <- y + sq*se

# Bs confidence bands
ct <-0
varorder <-c()
alpha_loc1 <-vector(mode="numeric",length=classes)
kappa_loc <- vector(mode="numeric",length=classes)
psi_loc1 <- vector(mode="numeric",length=classes)
phi_loc <-vector(mode="numeric",length=classes)
	for(i in 1:classes) {
		varorder <-c(varorder,alpha_loc[i+ct]) 
		alpha_loc1[i] <- alpha_loc[i+ct]
		psi_loc1[i] <- psi_loc[i+ct] 
		ct <- ct+1}

ct <-0
	for(i in 2:(classes+1)) {
		varorder <-c(varorder,alpha_loc[i+ct]) 
		kappa_loc[i-1] <- alpha_loc[i+ct]
		phi_loc[i-1] <- psi_loc[i+ct]
		ct <- ct+1}

varorder <-c(varorder,beta_loc)

equal <- NULL
if (identical(means[psi_loc[1]],means[psi_loc[3]])) equal<-TRUE else equal<- FALSE

ct <- 0
if (!equal) {
	for(i in 1:(classes)){
		varorder <-c(varorder,psi_loc[i+ct])
		ct <- ct+1
		}
	}

if (equal) {varorder <-c(varorder,psi_loc[1])}
	 
varorder <- c(varorder,c_loc)

#selecting submatrices of acov
#vector of variable positions

acov0 <- acov[varorder,varorder]
means0 = means[varorder]

#nonsimultaneous bs CIs
#generating the data
draws = 1000
L = chol(acov0)						#chol decomposition of acov
Z = rnorm(matrix(0,draws,length(means0)))		#random normal variates
Z = matrix(Z,draws,length(means0))			#placing in matrix form
Sim_est = Z%*%L + matrix(1,draws,1)%*%means0	#getting simulated values

################################################################################
#                              need to make general
################################################################################

#check to see how many negative variances there are
#missing = 0
#for (i in 1:nrow(Sim_est)){
#if (Sim_est[i,psi1_1_]<0) missing = missing+1
#}

#################################################################################


#obtaining the eta2 values for all simulated data
yall <-NULL

if (!equal) {
for (i in seq(LEta1,UEta1,length=47)){
yy <-0
placeholder <-0
	for (j in 1:classes) {
		if (j < classes) {
			placeholder = placeholder+exp(Sim_est[,classes*4+j])*dnorm(i,mean=Sim_est[,j],sd=sqrt(Sim_est[,classes*3+j])) }
		else { placeholder = placeholder + dnorm(i,mean=Sim_est[,classes],sd=sqrt(Sim_est[,classes*4])) } }

	for (k in 1:classes) {
		if (k < classes) {
			yy = yy + (exp(Sim_est[,classes*4+k])*dnorm(i,mean=Sim_est[,k],sd=sqrt(Sim_est[,classes*3+k]))*(Sim_est[,classes+k]+Sim_est[,classes*2+k]*i))/placeholder }
		else { yy = yy+(dnorm(i,mean=Sim_est[,classes],sd=sqrt(Sim_est[,classes*4]))*(Sim_est[,classes*2]+Sim_est[,classes*3]*i))/placeholder }
	}
	
	yall = rbind(yall,yy)
} }

if(equal) { for (i in seq(LEta1,UEta1,length=47)){
	
yy <-0
placeholder <-0
	for (j in 1:classes) {
		if (j < classes) {
			placeholder = placeholder+exp(Sim_est[,classes*3+1+j])*dnorm(i,mean=Sim_est[,j],sd=sqrt(Sim_est[,classes*3+1])) }
		else { placeholder = placeholder + dnorm(i,mean=Sim_est[,classes],sd=sqrt(Sim_est[,classes*3+1])) } }

	for (k in 1:classes) {
		if (k < classes) {
			yy = yy + (exp(Sim_est[,classes*3+1+k])*dnorm(i,mean=Sim_est[,k],sd=sqrt(Sim_est[,classes*3+1]))*(Sim_est[,classes+k]+Sim_est[,classes*2+k]*i))/placeholder }
		else { yy = yy+ (dnorm(i,mean=Sim_est[,classes],sd=sqrt(Sim_est[,classes*3+1]))*(Sim_est[,classes*2]+Sim_est[,classes*3]*i))/placeholder } }
	
	
	
	yall = rbind(yall,yy)
} }



#sorting before getting upper and lower CI values
for (i in 1:nrow(yall)){
	yall[i,] = sort(yall[i,])
	}
#estimate of bias to obtain bias-corrected confidence bands
median <- 0

for (i in 1:nrow(yall)) {
	median[i]<- median(yall[i,])
	}

bcyall <- yall - median
for (i in 1:nrow(bcyall)){ #sorting
	bcyall[i,] = sort(bcyall[i,])
	}

LCL = (alpha/2)*draws
UCL = (1-(alpha/2))*draws
	
LCLall = yall[,LCL]
UCLall = yall[,UCL]

bcMin <- bcyall[,LCL]+y
bcMax <- bcyall[,UCL]+y

#for bootstrapped simultaneous bands
if(alpha == 0.05) a<-1
if(alpha == 0.1)  a<-2
 
nall = matrix(c(557,228,
		    14257,4586,
	          29943,9234,
	          1018340,261260,
		    28666000,6296000),5,2,byrow=T)
namesN = list(c(4,8,9,14,19),c(.05,.10))
dimnames(nall) = namesN
n = nall[2,a]



L = chol(acov0)						#chol decomposition of acov
sZ = rnorm(matrix(0,n,length(means0)))		#random normal variates
sZ = matrix(sZ,n,length(means0))			#placing in matrix form
sSim_est = sZ%*%L + matrix(1,n,1)%*%means0	#getting simulated values

syall<-NULL
sMin<-NULL
sMax<-NULL

if (!equal) {
for (i in seq(LEta1,UEta1,length=47)){
yy <-0
placeholder <-0
	for (j in 1:classes) {
		if (j < classes) {
			placeholder = placeholder+exp(sSim_est[,classes*4+j])*dnorm(i,mean=sSim_est[,j],sd=sqrt(sSim_est[,classes*3+j])) }
		else { placeholder = placeholder + dnorm(i,mean=sSim_est[,classes],sd=sqrt(sSim_est[,classes*4])) } }

	for (k in 1:classes) {
		if (k < classes) {
			yy = yy + (exp(sSim_est[,classes*4+k])*dnorm(i,mean=sSim_est[,k],sd=sqrt(sSim_est[,classes*3+k]))*(sSim_est[,classes+k]+sSim_est[,classes*2+k]*i))/placeholder }
		else { yy = yy+(dnorm(i,mean=sSim_est[,classes],sd=sqrt(sSim_est[,classes*4]))*(sSim_est[,classes*2]+sSim_est[,classes*3]*i))/placeholder }
	}
	
		sMin <- c(sMin,min(yy))
		sMax <- c(sMax,max(yy))
		syall <- rbind(syall,yy)
} }


if(equal) { for (i in seq(LEta1,UEta1,length=47)){
	
yy <-0
placeholder <-0
	for (j in 1:classes) {
		if (j < classes) {
			placeholder = placeholder+exp(sSim_est[,classes*3+1+j])*dnorm(i,mean=sSim_est[,j],sd=sqrt(sSim_est[,classes*3+1])) }
		else { placeholder = placeholder + dnorm(i,mean=sSim_est[,classes],sd=sqrt(sSim_est[,classes*3+1])) } }

	for (k in 1:classes) {
		if (k < classes) {
			yy = yy + (exp(sSim_est[,classes*3+1+k])*dnorm(i,mean=sSim_est[,k],sd=sqrt(sSim_est[,classes*3+1]))*(sSim_est[,classes+k]+sSim_est[,classes*2+k]*i))/placeholder }
		else { yy = yy+ (dnorm(i,mean=sSim_est[,classes],sd=sqrt(sSim_est[,classes*3+1]))*(sSim_est[,classes*2]+sSim_est[,classes*3]*i))/placeholder } }

		sMin <- c(sMin,min(yy))
		sMax <- c(sMax,max(yy))
		syall <- rbind(syall,yy)

} }

bcsMin <- sMin - median + y
bcsMax <- sMax - median + y

#----------------------------------------------------------------------------------------------------------------------
#		Plots
#----------------------------------------------------------------------------------------------------------------------

Ksi <- Eta1
Eta <- Eta2
denKsi <- sumpi
denEta <- sumpi2
post <- pi
pKsi <- a_pi
pEta <- a_pi2

etahmat <- matrix(data = 0, nrow = length(Ksi), ncol = classes) 
  	for(i in 1:classes) { 
  		etahmat[,i] <- alphaarray[2,i]+gamma[i]*Ksi } 

etah_ <- y

#supress lines/points to plot when data is sparse

etah_[denKsi<=.02] <- NA
etah_[denEta<=.02] <- NA 

lo_ <- lo
hi_ <- hi
slo_ <- slo
shi_ <- shi

lo_[denKsi<=.02] <- NA
lo_[denEta<=.02] <- NA
hi_[denKsi<=.02] <- NA
hi_[denEta<=.02] <- NA
 
slo_[denKsi<=.02] <- NA
slo_[denEta<=.02] <- NA
shi_[denKsi<=.02] <- NA
shi_[denEta<=.02] <- NA


LCLall_ <- LCLall
UCLall_ <- UCLall

LCLall_[denKsi<=.02] <- NA
LCLall_[denEta<=.02] <- NA
UCLall_[denKsi<=.02] <- NA
UCLall_[denEta<=.02] <- NA

sMin_ <- sMin
sMax_ <- sMax
sMin_[denKsi<=.02] <- NA
sMin_[denEta<=.02] <- NA
sMax_[denKsi<=.02] <- NA
sMax_[denEta<=.02] <- NA


bcMin_ <- bcMin
bcMax_ <- bcMax
bcMin_[denKsi<=.02] <- NA
bcMin_[denEta<=.02] <- NA
bcMax_[denKsi<=.02] <- NA
bcMax_[denEta<=.02] <- NA

bcsMin_ <- bcsMin
bcsMax_ <- bcsMax
bcsMin_[denKsi<=.02] <- NA
bcsMin_[denEta<=.02] <- NA
bcsMax_[denKsi<=.02] <- NA
bcsMax_[denEta<=.02] <- NA

Ksi1 <- seq(3.95,-0.12, length=47)
Eta1 <- seq(1.03,0.00706, length=47)

plot(Ksi1,Eta1,type='n',xlab="Positive Emotions", ylab="Heuristic Processing", 
	cex.lab=1, cex.axis=1)
points(x,etah_,col=1,lwd=2, pch=16, cex=.8)
points(x,lo_,col=1,lwd=1.5,lty=2,cex=.8)
points(x,hi_,col=1,lwd=1.5,lty=2,cex=.8)
points(x,LCLall_,col=1,lwd=1.5,lty=3,pch=4,cex=.8)
points(x,UCLall_,col=1,lwd=1.5,lty=3,pch=4,,cex=.8)
legend("topleft", legend=c('Predicted Outcome', 'Wald-type 95% Confidence Interval',
	'Bootstrap 95% Confidence Interval'), lwd=c(2,1,1), lty=c(0,0,0),
	pch=c(16,1,4),col=c(1,1,1),bty="n", cex=1
	)


plot(Ksi1,Eta1,type='n',xlab="Positive Emotions", ylab="Heuristic Processing", 
	cex.lab=1, cex.axis=1)
lines(x,etah_,col=1,lwd=2, lty=1)
lines(x,etah_,col=1,lwd=2, pch=16, cex=.8)
lines(x,slo_,col=1,lwd=1,lty=2)
lines(x,shi_,col=1,lwd=1,lty=2)
lines(x,sMin_,col=1,lwd=1,lty=3)
lines(x,sMax_,col=1,lwd=1,lty=3)
legend("topleft", legend=c('Predicted Outcome', 'Wald-type 95% Confidence Envelope',
	'Bootstrap 95% Confidence Envelope','Null Hypothesis'), lwd=c(2,1,1,2), lty=c(1,2,3,1),
	pch=c(26,26,26,26),col=c(1,1,1,"grey"),bty="n", cex=1
	)
y2 <- 0.2+0.1*x
y3 <- -0.9+exp(0.14*x)
lines(x,y3,col='grey', lty=1, lwd=2)


#lines(SEMLIdatapks$x,SEMLIdatapks$slo_,col=2,lwd=1.5,lty=2)
#lines(SEMLIdatapks$x,SEMLIdatapks$shi_,col=2,lwd=1.5,lty=2)
lines(SEMLIdatapks$x,SEMLIdatapks$sMin_,col=4,lwd=1.5,lty=3)
lines(SEMLIdatapks$x,SEMLIdatapks$sMax_,col=4,lwd=1.5,lty=3)
#}

#----------------------------------------------------------------------------------------------------------------------
#		Output points to plot
#----------------------------------------------------------------------------------------------------------------------

out <- data.frame(x, etah_, lo_, hi_, LCLall_, UCLall_, slo_, shi_, sMin_, sMax_)
write.table(out, file='pos.2class.dat', append=FALSE, quote=FALSE,
		col.names=TRUE, row.names=FALSE)

