#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#:::::::::::::::: HLM longitudinal data analysis using nlme ::::::::::::::::
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

library(nlme)  # instead of lme4 to use correlation= & weights= in lme() and to fit "unstructured" using gls()



# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# :::::::::::::::::::::::::::: OPPOSITES DATA :::::::::::::::::::::::::::::::
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

library(dplyr)
library(ggplot2)
opposites <- read.csv('opposites.csv')

# Scores on an opposite's naming task across four occasions of measurement, obtained weekly, and a baseline 
# measure of cognitive skill (COG), obtained in the first week.  

# RQ: Is the interaction between time and cognitive skill significant? 
 
head(opposites, 20)
dim(opposites)
table(opposites$id)
(nT <- max(table(opposites$id)))  # nT x nT variance-covariance matrix 
range(opposites$opp)

# CCOG = COG - mean(COG)

# :::::::::::::::::: FIGURE :::::::::::::::::::

(opp.m <- with(opposites, tapply(opp, list(time), mean)))
plot(0:3, 0:3, type = 'n', xlab = '', ylab = '', xaxt = 'n', ylim = c(70,320))
axis(1, at = 0:3, labels = 1:4)
for (i in unique(opposites$id)){
with(opposites[opposites$id == i, ], 
     lines(time, opp, col = c('gray')))
}
lines(0:3, opp.m, type = 'o', col = 'red', lwd = 2, pch = 16)

ggplot(opposites, aes(x = time, y = opp, group = id)) + geom_line(alpha = .25) + 
  stat_smooth(aes(group = NULL), method = "lm", size = 1, se = FALSE)


# :::::::::::::::::: TABLE :::::::::::::::::::

with(opposites, tapply(opp, list(time), mean))  
with(opposites, tapply(opp, list(time), sd))  
with(opposites, tapply(opp, list(time), length))  


# create level-2 data 

opp.l2 <- aggregate(opposites[, c('opp', 'ccog')], list(opposites$id), mean)
names(opp.l2) <- c('id','opp.mean','ccog')

opp.12 <- opposites %.% group_by(id) %.% 
  summarize(opp.mean = mean(opp),
            ccog = unique(ccog))

head(opp.l2)
dim(opp.l2)
mean(opp.l2$opp.mean)  
# == mean(opposites$opp) only with balanced data



#::::::::::::::::: MODELS ::::::::::::::::::::::::



# gls(): Fit Linear Model Using Generalized Least Squares
# fit gls() for "unstructured" --- "random =  ~ 1 | id" and "correlation = corSymm(form = ~ 1 | id)" are incompatible formulas for groups 


# Unstructured Covariance
unstruct <- gls(opp ~ time*ccog, data = opposites, correlation = corSymm(form = ~ 1 | id),  weights = varIdent(form = ~ 1|wave), method="REML")
summary(unstruct)
unstruct.varests <- c(1.0000000, 0.9248170, 0.9584917, 0.9468611) 
(unstruct.cor <- corMatrix(unstruct$modelStruct$corStruct)[[1]])
(unstruct.cov <- unstruct.cor*unstruct$sigma^2*unstruct.varests%*%t(unstruct.varests))

# Table 7.3 in Singer & Willett (pp. 258-259)
#          [,1]      [,2]      [,3]      [,4]
#[1,] 1345.1222 1005.7729  946.1943  583.1995
#[2,] 1005.7729 1150.4648 1028.5349  846.5659
#[3,]  946.1943 1028.5349 1235.7723  969.2919
#[4,]  583.1995  846.5659  969.2919 1205.9638

summary(unstruct)$tTable
summary(unstruct)

logLik(unstruct)
# 'log Lik.' -627.8944 (df=14)
loglik <- as.numeric(logLik(unstruct))
(Deviance <- -2*loglik)
(AIC <- -2*loglik + 2*10)  # REML is used.  Only counting the random effect parameters (10) 
# number of parameters in SAS with REML: random only = 10
# number of parameters in R with REML: fixed + random = 14 

(BIC <- -2*loglik + log(35)*10)  
# SAS PROC MIXED uses the number of level-2 units as the sample size in BIC = 35
# R uses the number of level-1 units as the sample size in BIC = 140 

# Independent error 
indep <- gls(opp ~ time*ccog, opposites, method="REML")  
summary(indep)$sigma^2  # 1204.883

summary(indep)$sigma^2*diag(nT)

#         [,1]     [,2]     [,3]     [,4]
#[1,] 1204.883    0.000    0.000    0.000
#[2,]    0.000 1204.883    0.000    0.000
#[3,]    0.000    0.000 1204.883    0.000
#[4,]    0.000    0.000    0.000 1204.883

# Use update() to modify model specification
# Compound Symmetry: 'homogeneous' in HLM7 
comp.symm <- update(indep, correlation = corCompSymm(, form = ~ 1 | id))  
summary(comp.symm)     # Rho = .731
(cs.cor <- corMatrix(comp.symm$modelStruct$corStruct)[[1]])
(cs.cov <- cs.cor * comp.symm$sigma^2)  

#          [,1]      [,2]      [,3]      [,4]
#[1,] 1231.3559  900.0718  900.0718  900.0718
#[2,]  900.0718 1231.3559  900.0718  900.0718
#[3,]  900.0718  900.0718 1231.3559  900.0718
#[4,]  900.0718  900.0718  900.0718 1231.3559


# Multilevel Models
# RIM using lme() = compound symmetry using gls()
RIM <- lme(opp ~ time * ccog, opposites, random =  ~ 1 | id, method = "REML")
summary(RIM)
VarCorr(RIM)  # 1231.4 = 900.1 (tau) + 331.3 (sigma)
(tau00 <- as.numeric(VarCorr(RIM)[1]))

(tau.nT <- tau00 * rep(1, nT) %*% t(rep(1, nT))) 

#         [,1]     [,2]     [,3]     [,4]
#[1,] 900.0719 900.0719 900.0719 900.0719
#[2,] 900.0719 900.0719 900.0719 900.0719
#[3,] 900.0719 900.0719 900.0719 900.0719
#[4,] 900.0719 900.0719 900.0719 900.0719
>
(SIGMA.nT <- summary(RIM)$sigma ^ 2 * diag(nT))
#         [,1]     [,2]     [,3]     [,4]
#[1,] 331.2841   0.0000   0.0000   0.0000
#[2,]   0.0000 331.2841   0.0000   0.0000
#[3,]   0.0000   0.0000 331.2841   0.0000
#[4,]   0.0000   0.0000   0.0000 331.2841

(RIM.cov <- tau.nT + SIGMA.nT) 
#          [,1]      [,2]      [,3]      [,4]
#[1,] 1231.3560  900.0719  900.0719  900.0719
#[2,]  900.0719 1231.3560  900.0719  900.0719
#[3,]  900.0719  900.0719 1231.3560  900.0719
#[4,]  900.0719  900.0719  900.0719 1231.3560


anova(indep, comp.symm, RIM, unstruct)


# Random Slope Model
RSM <- update(RIM, random =  ~ time | id)   
summary(RSM)


anova(RIM, RSM)

# Frequently used correlation/covariance structures
# First-order auto regressive AR(1)
RIM.AR1 <- update(RIM, correlation = corAR1(, form = ~ 1 | id))
summary(RIM.AR1)
# Phi = 0.8253 


# Heterogeneous variances 
RIM.hetero <- update(RIM, weights = varIdent(form = ~ 1 | wave))
summary(RIM.hetero)
#        1         2         3         4 
# 1.0000000 0.5237753 0.6094989 1.0146765 


# Heterogeneous correlations 
RIM.off.diag <- update(RIM, correlation = corSymm(form = ~ 1 | id))
summary(RIM.off.diag)


anova(RIM, RIM.AR1, RIM.hetero, RIM.off.diag, RSM, unstruct)


# TOEP
RIM.toep <- update(RIM, correlation = corARMA(, form = ~ 1 | id, p = 3, q = 0))
summary(RIM.toep)
# Phi1, Phi2, Phi3
corMatrix(RIM.toep$modelStruct$corStruct)[[1]]
logLik(RIM.toep)

# Heterogeneous + AR1 
RIM.hetero.AR1 <- update(RIM, correlation = corAR1(, form = ~ 1 | id), 
                         weights = varIdent(form = ~ 1 | wave))
summary(RIM.hetero.AR1)


anova(indep, RIM, RIM.AR1, RSM, RIM.toep, RIM.hetero, RIM.hetero.AR1, unstruct)

#               Model df      AIC      BIC     logLik   Test  L.Ratio p-value
# indep              1  5 1391.010  1405.573  -690.5048                        
# RIM/homogeneous    2  6 1299.048  1316.524  -643.5238 1 vs 2 93.96199  <.0001
# RIM.AR1            3  7 1279.876* 1300.265* -632.9382 2 vs 3 21.17123  <.0001
# RSM                4  8 1276.285* 1299.586* -630.1424 3 vs 4  5.59168  0.0180
# RIM.toep           5  9 1276.081* 1302.295* -629.0404 4 vs 5  2.20405  0.1376
# RIM.hetero         6  9 1289.917  1316.131  -635.9586                        
# RIM.hetero.AR1     7 10 1284.778  1313.904  -632.3889 6 vs 7  7.13942  0.0075
# unstruct           8 14 1283.789  1324.566  -627.8944 7 vs 8  8.98901  0.0614


anova(RIM.AR1, RIM.toep)  # nested
anova(RIM.toep, RSM)      # not nested

#         Model df      AIC      BIC    logLik   Test  L.Ratio p-value
#RIM.toep     1  9 1276.081 1302.295 -629.0404                        
#RSM          2  8 1276.285 1299.586 -630.1424 1 vs 2 2.204054  0.1376


 
summary(RIM.toep)
summary(RIM.toep)$tTable


(toep <- corMatrix(RIM.toep$modelStruct$corStruct)[[1]])

(tau00 <- as.numeric(VarCorr(RIM.toep)[1]))     
(tau.nT <- tau00 * matrix(1, nT, nT)) 

(toep.cov <- corMatrix(RIM.toep$modelStruct$corStruct)[[1]] * summary(RIM.toep)$sigma ^ 2)

(RIM.toep.cov <- tau.nT + toep.cov)
#          [,1]     [,2]     [,3]      [,4]
#[1,] 1246.8843 1029.312  896.579  624.0484
#[2,] 1029.3124 1246.884 1029.312  896.5790
#[3,]  896.5790 1029.312 1246.884 1029.3124
#[4,]  624.0484  896.579 1029.312 1246.8843


summary(RSM)
summary(RSM)$tTable


tau00 <- as.numeric(VarCorr(RSM)[1, 1])
tau11 <- as.numeric(VarCorr(RSM)[2, 1])
tau10 <- as.numeric(VarCorr(RSM)[2, 3]) * sqrt(tau00 * tau11)
(TAU <- matrix(c(tau00, tau10, tau10, tau11), ncol = 2))

#         [,1]      [,2]
#[1,] 1236.425 -178.0690
#[2,] -178.069  107.2485


(sigma <- summary(RSM)$sigma ^ 2)
(design <- cbind(rep(1, nT), seq(0, nT - 1)))

# design matrix for "random" effects: note that the design matrix is a vector of ones in RIM

#       [,1] [,2]
# [1,]    1    0
# [2,]    1    1
# [3,]    1    2
# [4,]    1    3
 
(TAU.nT <- design %*% TAU %*% t(design))
#          [,1]      [,2]     [,3]      [,4]
#[1,] 1236.4253 1058.3563 880.2872  702.2182
#[2,] 1058.3563  987.5357 916.7152  845.8946
#[3,]  880.2872  916.7152 953.1431  989.5711
#[4,]  702.2182  845.8946 989.5711 1133.2476

(SIGMA.nT <- summary(RSM)$sigma ^ 2 * diag(nT))

#          [,1]     [,2]     [,3]     [,4]
# [1,] 159.4775   0.0000   0.0000   0.0000
# [2,]   0.0000 159.4775   0.0000   0.0000
# [3,]   0.0000   0.0000 159.4775   0.0000
# [4,]   0.0000   0.0000   0.0000 159.4775


(RSM.cov <- TAU.nT + SIGMA.nT)
#          [,1]      [,2]      [,3]      [,4]
#[1,] 1395.9028 1058.3563  880.2872  702.2182
#[2,] 1058.3563 1147.0132  916.7152  845.8946
#[3,]  880.2872  916.7152 1112.6206  989.5711
#[4,]  702.2182  845.8946  989.5711 1292.7250

summary(RIM.toep)
#      Phi1       Phi2       Phi3 
# 0.5616707  0.3147264 -0.5952173 
 
(toep.cov <- corMatrix(RIM.toep$modelStruct$corStruct)[[1]] * summary(RIM.toep)$sigma ^ 2)
# negative Phi3 => negative cov (sigma_(1,3) = -125)

#          [,1]     [,2]     [,3]      [,4]
#[1,]  497.4875 279.9156 147.1822 -125.3484
#[2,]  279.9156 497.4875 279.9156  147.1822
#[3,]  147.1822 279.9156 497.4875  279.9156
#[4,] -125.3484 147.1822 279.9156  497.4875

tau.nT
#         [,1]     [,2]     [,3]     [,4]
#[1,] 749.3968 749.3968 749.3968 749.3968
#[2,] 749.3968 749.3968 749.3968 749.3968
#[3,] 749.3968 749.3968 749.3968 749.3968
#[4,] 749.3968 749.3968 749.3968 749.3968

(RIM.toep.cov <- tau.nT + toep.cov)  # => positive but smaller cov sigma_(1,3) = 624 
#           [,1]     [,2]     [,3]      [,4]
# [1,] 1246.8843 1029.312  896.579  624.0484
# [2,] 1029.3124 1246.884 1029.312  896.5790
# [3,]  896.5790 1029.312 1246.884 1029.3124
# [4,]  624.0484  896.579 1029.312 1246.8843


d <- sqrt(diag(RIM.toep.cov))
RIM.toep.cov / outer(d, d)   # all positive correlations, banded corr

RSM.cov                   # different vars & covariances
d <- sqrt(diag(RSM.cov))
RSM.cov / outer(d, d)

unstruct.cor
