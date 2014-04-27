#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#:::::::::::::::: HLM longitudinal data analysis using nlme ::::::::::::::::
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

library(nlme)  # instead of lme4 to use correlation= & weights= in lme() and to fit "unstructured" using gls()

# :::::::::::::::::::::: PROXIMITY DATA :::::::::::::::::::::::::::

prox <- read.csv("proximity.csv")
head(prox, 15)
tail(prox)                           # teacher ID: 1 to 51

# idfy(prox$teacherid, prox$female)  #"OK": gender is a level-2 variable

dim(prox)
length(table(prox$teacherID))        # of level-2 units
(freq.n.times <- table(prox$time))   # n.times frequencies
(n.per.teacher <- table(prox$teacherID))        # n.obs per level-2 units 
(freq.n.per.teacher <- table(n.per.teacher))    # n.per.teacher frequencies 
sum(freq.n.times)                    # level-1 sample size 
sum(freq.n.per.teacher)              # level-2 sample size



# :::::::::::::  TABLES (gender*time) :::::::::::::::

with(prox, tapply(proximity, list(time), mean))  

round(prox.m <- with(prox, tapply(proximity, list(female, time), mean)),3)    # mean 
table(prox$female, prox$time)                                                 # n


# proximity means by gender
round(tapply(prox$proximity, list(prox$female), mean),3)  # Is this correct? Why or why not?   
table(prox$female)


# create level-2 data 
prox.l2 <- aggregate(prox[, c('proximity', 'female')], list(prox$teacherID), mean)
head(prox.l2)
dim(prox.l2)
names(prox.l2) <- c('teacherID','prox.mean','female')
head(prox.l2)

(gender.n <- table(prox.l2$female))
round(gender.n/length(prox.l2$female),3)
round(gender.m <- tapply(prox.l2$prox.mean, list(prox.l2$female), mean),3)  # mean.m > mean.f

# compare to the means based on level-1 data
round(tapply(prox$proximity, list(prox$female), mean),3)                    # mean.m < mean.f  

# why are the values so different? 
round(prox.m <- with(prox, tapply(proximity, list(female, time), mean)),3)  # mean (gender x time)
table(prox$female, prox$time)                                               # n (gender x time)





#:::::::::::::::::: FIGURES :::::::::::::::::::::

(prox.m <- with(prox, tapply(proximity, list(female, time), mean)))

# Mean plot  --- curvilinear over time, 'seemingly' large gender effects  
range(prox.m)
plot(0:3, 0:3, type = 'n', xlab = '', ylab = '', xaxt = 'n', ylim = c(0.4,0.9))
axis(1, at = 0:3, labels = 1:4)
lines(0:3, prox.m[1, ], type = 'o', col = 'red', lwd = 2, pch = 16)
lines(0:3, prox.m[2, ], type = 'o', col = 'blue', lwd = 2, pch = 16)
legend('bottomright', c('male','female'), bty = 'n', col=c(2,4), pch=16)
title("Mean Promixity Scores Over Time by Gender")



# Plot all observations -- a wide range of Y, large individual differences, small gender effects  
# points for single observations
range(prox$proximity)
plot(0:3, 0:3, type = 'n', xlab = '', ylab = '', xaxt = 'n', ylim = c(-1,1.7))
axis(1, at = 0:3, labels = 1:4)
for (i in unique(prox$teacherID)){
with(prox[prox$teacherID == i, ], lines(time, proximity, col = c('yellow', 'gray')[female + 1]))
with(prox[prox$teacherID == i, ], if(length(teacherID) == 1) 
points(time, proximity, col = c('yellow', 'gray')[female + 1], pch = 4))
}
lines(0:3, prox.m[1, ], type = 'o', col = 'red2', lwd = 2, pch = 16)
lines(0:3, prox.m[2, ], type = 'o', col = 'blue2', lwd = 2, pch = 16)
legend('bottomright', c('male','female','m.mean','f.mean'), bty = 'n', col=c('yellow','gray','red','blue'), pch=16)
title("Promixity Scores Over Time by Gender")





# ::::::::::::::::: MODLES ::::::::::::::::::::::::

# Fit multilevel models for longitudinal data 
# a.k.a. "latent growth curve models" with linear and quadratic slopes

(nT <- length(unique(prox$time)))  # the number of time points

empty <- lme(proximity ~ 1, random =  ~ 1 | teacherID, data = prox, method="ML")
VarCorr(empty)
(VC.empty <- as.numeric(VarCorr(empty)))
(ICC <- VC.empty[1]/(VC.empty[1]+VC.empty[2]))   # tau_00 / (tau_00 + sigma^2)


# Random Intercept Models 

RIM.linear  <- lme(proximity ~ time, random =  ~ 1 | teacherID, data = prox, method="ML")
RIM.quad    <- lme(proximity ~ time + I(time^2), random =  ~ 1 | teacherID, data = prox, method="ML")
RIM.disc.time    <- lme(proximity ~ as.factor(time), random =  ~ 1 | teacherID, data = prox, method="ML")

summary(RIM.linear)      # close to zero slope
summary(RIM.quad)        # significant linear effect 
summary(RIM.disc.time)
anova(RIM.linear, RIM.quad, RIM.disc.time)   # compare three bad models, ignoring heterogeneity of slopes 

VarCorr(RIM.quad)  # tau00 & sigma^2


# Random Slope Models

RSM.linear <- lme(proximity ~ time, random  =  ~  time| teacherID, data = prox, method="ML")
RSM.quad <- lme(proximity ~ time + I(time^2), random =  ~  time| teacherID, data = prox, method="ML")
# RSM2.lin.sq      <- lme(proximity ~ time + I(time^2), random =  ~  time + I(time^2)| teacherID, data = prox, method="ML")  # fail to converge 

anova(RIM.linear, RIM.quad, RIM.disc.time, RSM.linear, RSM.quad)  # RSM.quad

summary(RSM.quad)
VarCorr(RSM.quad)

# other predictors 
RSM.quad.f <- lme(proximity ~ time + female + I(time^2), random =  ~  time| teacherID, data = prox, method="ML")
summary(RSM.quad.f)
anova(RSM.quad, RSM.quad.f)  # not significant


# ::::::: COVARIANCE STRUCTURES :::::::::::::

# ::::: gls() :::::::::::::::
# Unstructured error covariance matrix
# gls(): Fit Linear Model Using Generalized Least Squares
# fit gls() for "unstructured" --- "random =  ~ 1 | id" and "correlation = corSymm(form = ~ 1 | ID)" are incompatible formulas for groups 

(nT <- length(unique(prox$time)))  # the number of time points

head(prox, 30)
# caution: ids 1-4 has less than nT = 4 observatons

unstruct <- gls(proximity  ~ time + I(time^2), data = prox, correlation = corSymm(form = ~ 1 | teacherID),  weights = varIdent(form = ~ 1|time), method="ML")
anova(RSM.quad, unstruct)
summary(unstruct)

# id = 1 has observations at 0 1 3 (no t = 2)

unstruct.varests <- c(1.0000000, 0.8266989, 0.6341750, 0.6923248) # length(unique(prox$time))
(unstruct.cor <- corMatrix(unstruct$modelStruct$corStruct)[[5]])  # 4 * 4  
#          [,1]      [,2]      [,3]      [,4]
# [1,] 1.0000000 0.7053791 0.5561097 0.6082109
# [2,] 0.7053791 1.0000000 0.6532491 0.8122030
# [3,] 0.5561097 0.6532491 1.0000000 0.8309205
# [4,] 0.6082109 0.8122030 0.8309205 1.0000000

unstruct$sigma^2
#  0.2905667

unstruct$sigma^2*unstruct.varests
# [1] 0.2905667 0.2402112 0.1842701 0.2011665

(unstruct.cov <- unstruct.cor*unstruct$sigma^2*unstruct.varests%*%t(unstruct.varests))

#           [,1]      [,2]      [,3]      [,4]
# [1,] 0.2905667 0.1694399 0.1024744 0.1223517
# [2,] 0.1694399 0.1985823 0.0995133 0.1350727
# [3,] 0.1024744 0.0995133 0.1168595 0.1060045
# [4,] 0.1223517 0.1350727 0.1060045 0.1392726

logLik(unstruct)

# independent or uncorrelated error covariance matrix 
indep <- gls(proximity  ~ time + I(time^2), data = prox, method="ML")  
summary(indep)       # single level model with only sigma (no tau) 
summary(indep)$sigma^2   # .18468

logLik(indep)

summary(indep)$sigma^2*diag(nT)

#           [,1]      [,2]      [,3]      [,4]
# [1,] 0.1846831 0.0000000 0.0000000 0.0000000
# [2,] 0.0000000 0.1846831 0.0000000 0.0000000
# [3,] 0.0000000 0.0000000 0.1846831 0.0000000
# [4,] 0.0000000 0.0000000 0.0000000 0.1846831


# Use update() to modify model specification
# Compound Symmetry: 'homogeneous' in HLM7 
comp.symm <- update(indep, correlation = corCompSymm(form = ~ 1 | teacherID))  
summary(comp.symm) # Rho = .6284
(cs.cor <- corMatrix(comp.symm$modelStruct$corStruct)[[5]])  # residual ICC in RIM

#           [,1]      [,2]      [,3]      [,4]
# [1,] 1.0000000 0.6283655 0.6283655 0.6283655
# [2,] 0.6283655 1.0000000 0.6283655 0.6283655
# [3,] 0.6283655 0.6283655 1.0000000 0.6283655
# [4,] 0.6283655 0.6283655 0.6283655 1.0000000

comp.symm$sigma^2  # 0.190305

(cs.cov <- cs.cor * comp.symm$sigma^2)  # .11958 = .190305*.62836
# diagonals: sigma^2; off-diagonals: sigma^2 * rho (= tau_00 in RIM)

#          [,1]      [,2]      [,3]      [,4]
# [1,] 0.1903050 0.1195811 0.1195811 0.1195811
# [2,] 0.1195811 0.1903050 0.1195811 0.1195811
# [3,] 0.1195811 0.1195811 0.1903050 0.1195811
# [4,] 0.1195811 0.1195811 0.1195811 0.1903050


anova(indep, comp.symm)

# ::::: lme() :::::::::::::::

# Random intercept model 
RIM.quad <- lme(proximity ~ time + I(time^2), random =  ~ 1 | teacherID, data = prox, method="ML")
summary(RIM.quad)
VarCorr(RIM.quad)  

(tau00 <- as.numeric(VarCorr(RIM.quad)[1]))     
(TAU.nT <- tau00*matrix(1,nT,nT)) 
#           [,1]      [,2]      [,3]      [,4]
# [1,] 0.1195809 0.1195809 0.1195809 0.1195809
# [2,] 0.1195809 0.1195809 0.1195809 0.1195809
# [3,] 0.1195809 0.1195809 0.1195809 0.1195809
# [4,] 0.1195809 0.1195809 0.1195809 0.1195809


(SIGMA.nT <- summary(RIM.quad)$sigma^2*diag(nT))
#           [,1]       [,2]       [,3]       [,4]
# [1,] 0.07072394 0.00000000 0.00000000 0.00000000
# [2,] 0.00000000 0.07072394 0.00000000 0.00000000
# [3,] 0.00000000 0.00000000 0.07072394 0.00000000
# [4,] 0.00000000 0.00000000 0.00000000 0.07072394



(RIM.quad.cov <- TAU.nT + SIGMA.nT) 
#           [,1]      [,2]      [,3]      [,4]
# [1,] 0.1903049 0.1195809 0.1195809 0.1195809
# [2,] 0.1195809 0.1903049 0.1195809 0.1195809
# [3,] 0.1195809 0.1195809 0.1903049 0.1195809
# [4,] 0.1195809 0.1195809 0.1195809 0.1903049


# diagonals: tau_00 + sigma^2; off-diagonals: tau_00
# 0.11958094/(0.11958094+0.07072394) = 0.11958094/0.1903049 = 0.6283651 = rho in comp.symm


anova(comp.symm, RIM.quad)        # comp.symm in gls() = random intercept model in lme()



# :::: Frequently used covariance structures :::::
# use "update" for model specification

# Heterogeneous variances 
hetero.RIM <- update(RIM.quad, weights=varIdent(form = ~ 1|time))
summary(hetero.RIM)    # 3 additional variance parameters 

hetero.varests <- c(1.0000000, 0.7127709, 0.4168145, 0.3947354)    # SDs compared to sigma
summary(hetero.RIM)$sigma^2
(hetero.error <- hetero.RIM$sigma^2*diag(nT)*(hetero.varests)^2) 


# Heterogeneous correlations 
off.diag.RIM <- update(RIM.quad, correlation = corSymm(form = ~ 1 | teacherID))
summary(off.diag.RIM)  # 6 additional correlation parameters


# First-order auto regressive AR(1)
AR1.RIM <- update(RIM.quad, correlation=corAR1(,form = ~ 1 |teacherID))
summary(AR1.RIM)
AR1.RIM$modelStruct$corStruct
corMatrix(AR1.RIM$modelStruct$corStruct)[[5]]
# phi = .3659; .3659^2 = .1338; .3659^3 = .0489


# Toeplitz (diagonal-constant) as ARMA(3,0)
toep.RIM <- update(RIM.quad, correlation=corARMA(,form = ~ 1 |teacherID, p=3,q=0))
summary(toep.RIM)
# phi1, phi2, phi3
corMatrix(toep.RIM$modelStruct$corStruct)[[5]]


ARMA10.RIM <- update(RIM.quad, correlation=corARMA(,form = ~ 1 |teacherID, p=1,q=0))
anova(AR1.RIM, ARMA10.RIM)   # AR(1) = ARMA(1,0)


# Heterogeneous + AR1 
hetero.AR1.RIM <- update(RIM.quad, correlation=corAR1(,form = ~ 1 |teacherID), weights=varIdent(form = ~ 1|time))
summary(hetero.AR1.RIM)
corMatrix(hetero.AR1.RIM$modelStruct$corStruct)[[5]]
# phi = .3762699;  0.3762699^2 = .141579; 0.3762699^3 = .05327

anova(RIM.quad, AR1.RIM, toep.RIM, hetero.RIM, hetero.AR1.RIM, off.diag.RIM, unstruct, RSM.quad)



#               Model df      AIC      BIC    logLik   Test   L.Ratio p-value
# RIM.quad           1  5 129.2002 144.3524 -59.60009                         
# AR1.RIM            2  6 128.1308 146.3134 -58.06540 1 vs 2  3.069382  0.0798
# toep.RIM           3  8 131.9712 156.2147 -57.98560 2 vs 3  0.159582  0.9233
# hetero.RIM         4  8 117.8825 142.1260 -50.94126                               # 1 tau, 4 sigmas, 3 gammas
# hetero.AR1.RIM     5  9 116.9965 144.2705 -49.49828 4 vs 5  2.885977  0.0894      # 1 tau, 4 sigmas, 1 phi, 3 gammas
# off.diag.RIM       6 11 127.4600 160.7949 -52.73003 5 vs 6  6.463499  0.0395
# unstruct           6 13 117.9001 157.2958 -45.95007 5 vs 6  7.096404  0.1309      # 4*5/2 = 10 varcov (little Gauss!), 3 gammas 
# RSM.quad           8  7 118.4022 139.6153 -52.20110 7 vs 8 12.502049  0.0517      # 3 tau's, 1 sigma, 3 gammas  

# hetero.AR1.RIM has the smallest AIC
# RSM.quad has the smallest BIC 

###########################################################################################


anova(hetero.RIM, hetero.AR1.RIM)  # nested
anova(hetero.RIM, RSM.quad)

 
summary(hetero.RIM) # linear and quadratic terms are not significant, heterogeneous variances at time 0, 1, 2, 3
summary(RSM.quad)   # linear and quadratic terms, variability in the slope of time 



# :::::::::::::::::::::::::::::::::::::::::
# ::::  var-cov matrix of hetero.RIM ::::
# :::::::::::::::::::::::::::::::::::::::::

hetero.varests <- c(1.0000000, 0.7127709, 0.4168145, 0.3947354)   # SDs compared to sigma
(hetero.error <- hetero.RIM$sigma^2*diag(4)*(hetero.varests)^2) 
#           [,1]       [,2]       [,3]       [,4]
# [1,] 0.1558199 0.00000000 0.00000000 0.00000000
# [2,] 0.0000000 0.07916311 0.00000000 0.00000000
# [3,] 0.0000000 0.00000000 0.02707127 0.00000000
# [4,] 0.0000000 0.00000000 0.00000000 0.02427924


VarCorr(hetero.RIM)
(tau00 <- as.numeric(VarCorr(hetero.RIM)[1]))

# Var(εi) = Var(Ari + ei) = Δ = A*τ*A' + diag(σ21,...,σ24)

tau00*matrix(1,nT,nT) 
#           [,1]      [,2]      [,3]      [,4]
# [1,] 0.1038702 0.1038702 0.1038702 0.1038702
# [2,] 0.1038702 0.1038702 0.1038702 0.1038702
# [3,] 0.1038702 0.1038702 0.1038702 0.1038702
# [4,] 0.1038702 0.1038702 0.1038702 0.1038702


(hetero.RIM.cov <- tau00*matrix(1,4,4) + hetero.error)

# In short,
tau00 + hetero.error  # .1038 + 4x4 matrix

#          [,1]      [,2]      [,3]      [,4]
# [1,] 0.2596901 0.1038702 0.1038702 0.1038702
# [2,] 0.1038702 0.1830333 0.1038702 0.1038702
# [3,] 0.1038702 0.1038702 0.1309415 0.1038702
# [4,] 0.1038702 0.1038702 0.1038702 0.1281494


# :::::::::::::::::::::::::::::::::::::::::
# ::::: var-cov matrix of RSM.quad ::::::::
# :::::::::::::::::::::::::::::::::::::::::


VarCorr(RSM.quad)
tau00 <- as.numeric(VarCorr(RSM.quad)[1,1])
tau11 <- as.numeric(VarCorr(RSM.quad)[2,1])
tau10 <- as.numeric(VarCorr(RSM.quad)[2,3])*sqrt(tau00*tau11)
(TAU <- matrix(c(tau00,tau10,tau10,tau11),ncol=2))

#            [,1]        [,2]
# [1,]  0.22601635 -0.04931547
# [2,] -0.04931547  0.01517757


(sigma2 <- summary(RSM.quad)$sigma^2)
# [1] 0.04940081

(design <- cbind(rep(1,4),seq(0,3)))
#      [,1] [,2]
# [1,]    1    0
# [2,]    1    1
# [3,]    1    2
# [4,]    1    3

# Var(εi) = Var(Ari + ei) = Δ = AτA' + σ2I

design%*%TAU%*%t(design) 

#           [,1]      [,2]       [,3]       [,4]
# [1,] 0.22601635 0.1767009 0.12738542 0.07806995
# [2,] 0.17670088 0.1425630 0.10842509 0.07428720
# [3,] 0.12738542 0.1084251 0.08946477 0.07050444
# [4,] 0.07806995 0.0742872 0.07050444 0.06672168

sigma2*diag(nT)
#            [,1]       [,2]       [,3]       [,4]
# [1,] 0.04940081 0.00000000 0.00000000 0.00000000
# [2,] 0.00000000 0.04940081 0.00000000 0.00000000
# [3,] 0.00000000 0.00000000 0.04940081 0.00000000
# [4,] 0.00000000 0.00000000 0.00000000 0.04940081

(homog.RSM.cov <- design%*%TAU%*%t(design) + sigma2*diag(nT))

#            [,1]      [,2]       [,3]       [,4]
# [1,] 0.27541716 0.1767009 0.12738542 0.07806995
# [2,] 0.17670088 0.1919638 0.10842509 0.07428720
# [3,] 0.12738542 0.1084251 0.13886557 0.07050444
# [4,] 0.07806995 0.0742872 0.07050444 0.11612249

# CAUTION: NOT design%*%TAU%*%t(design) + sigma2.

homog.RSM.cov
hetero.RIM.cov
unstruct.cov

anova(RSM.quad, hetero.RIM, unstruct)

# Convert a Covariance Matrix to a Correlation Matrix 

d <- sqrt(diag(hetero.RIM.cov))
hetero.RIM.cov/outer(d,d)

d <- sqrt(diag(homog.RSM.cov))
homog.RSM.cov/outer(d,d)

d <- sqrt(diag(unstruct.cov))
unstruct.cov/outer(d,d)

unstruct.cor  