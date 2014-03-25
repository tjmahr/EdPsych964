Chapter 4: The Random Intercept Model
===============================================================================


```r
library(lme4)
library(dplyr)

varcomp <- function(obj) {
  components <- unlist(lapply(VarCorr(obj), diag))
  residual <- attr(VarCorr(obj), "sc")
  variance_components <- c(components, Residual = residual ^ 2)
  ICC <- components / sum(variance_components)
  list(var.components = variance_components, ICC = ICC)
}

# Access and pretty print a fixed effect coefficient
fixef2 <- function(model, variable) round(fixef(model)[variable], 2)

# Downloaded from http://www.stats.ox.ac.uk/~snijders/mlbook.htm
d <- read.table("mlbook2_r.dat", header = TRUE)
str(d)
```

```
## 'data.frame':	3758 obs. of  11 variables:
##  $ schoolnr   : num  1 1 1 1 1 1 1 1 1 1 ...
##  $ pupilNR_new: num  3 4 5 6 7 8 9 10 11 12 ...
##  $ langPOST   : num  46 45 33 46 20 30 30 57 36 36 ...
##  $ ses        : num  -4.73 -17.73 -12.73 -4.73 -17.73 ...
##  $ IQ_verb    : num  3.13 2.63 -2.37 -0.87 -3.87 -2.37 -2.37 1.13 -2.37 -0.87 ...
##  $ sex        : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ Minority   : num  0 1 0 0 0 1 1 0 1 1 ...
##  $ denomina   : num  1 1 1 1 1 1 1 1 1 1 ...
##  $ sch_ses    : num  -14 -14 -14 -14 -14 ...
##  $ sch_iqv    : num  -1.4 -1.4 -1.4 -1.4 -1.4 ...
##  $ sch_min    : num  0.63 0.63 0.63 0.63 0.63 0.63 0.63 0.63 0.63 0.63 ...
```



Ex. 4.1: Empty model for language scores in elementary schools
-------------------------------------------------------------------------------


Ex. 4.2: Random intercept and one explanatory variable (IQ)
-------------------------------------------------------------------------------


Ex. 4.3: Within- and between-group regressions for IQ
-------------------------------------------------------------------------------


```r
m_3a <- lmer(langPOST ~ IQ_verb + sch_iqv + (1 | schoolnr), d, REML = FALSE)
summary(m_3a)
```

```
## Linear mixed model fit by maximum likelihood  ['lmerMod']
## Formula: langPOST ~ IQ_verb + sch_iqv + (1 | schoolnr)
##    Data: d
## 
##      AIC      BIC   logLik deviance df.resid 
##    24898    24929   -12444    24888     3753 
## 
## Scaled residuals: 
##    Min     1Q Median     3Q    Max 
## -4.222 -0.641  0.063  0.706  3.219 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  schoolnr (Intercept)  8.68    2.95    
##  Residual             40.43    6.36    
## Number of obs: 3758, groups: schoolnr, 211
## 
## Fixed effects:
##             Estimate Std. Error t value
## (Intercept)  41.1138     0.2318   177.4
## IQ_verb       2.4536     0.0555    44.2
## sch_iqv       1.3124     0.2616     5.0
## 
## Correlation of Fixed Effects:
##         (Intr) IQ_vrb
## IQ_verb -0.007       
## sch_iqv  0.043 -0.210
```


> Classes differ in two ways: they may have different mean IQ values, whichs affects the expected results _Y_ through the term 1.312 * `sch_iqv` [i.e., mean IQ in class _j_]; this is an explained difference between the classes; and they have randomly differing values for _U_<sub>0<em>j</em></sub>, which is an unexplained difference. These two ingredients contribute to the class-dependent intercept, given by 41.11 + _U_<sub>0<em>j</em></sub> + 1.312 * `sch_iqv`. (p. 59)

### Within-group centering


```r
d <- mutate(d, dev_iqv = IQ_verb - sch_iqv)
m_3b <- lmer(langPOST ~ dev_iqv + sch_iqv + (1 | schoolnr), d, REML = FALSE)
summary(m_3b)
```

```
## Linear mixed model fit by maximum likelihood  ['lmerMod']
## Formula: langPOST ~ dev_iqv + sch_iqv + (1 | schoolnr)
##    Data: d
## 
##      AIC      BIC   logLik deviance df.resid 
##    24898    24929   -12444    24888     3753 
## 
## Scaled residuals: 
##    Min     1Q Median     3Q    Max 
## -4.222 -0.641  0.063  0.706  3.219 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  schoolnr (Intercept)  8.68    2.95    
##  Residual             40.43    6.36    
## Number of obs: 3758, groups: schoolnr, 211
## 
## Fixed effects:
##             Estimate Std. Error t value
## (Intercept)  41.1138     0.2318   177.4
## dev_iqv       2.4536     0.0555    44.2
## sch_iqv       3.7660     0.2558    14.7
## 
## Correlation of Fixed Effects:
##         (Intr) dev_qv
## dev_iqv -0.007       
## sch_iqv  0.042  0.002
```

```r
# Equivalently
m_3c <- lmer(langPOST ~ I(IQ_verb - sch_iqv) + sch_iqv + (1 | schoolnr), d, 
    REML = FALSE)
```


The within-group effect is 2.45. The between-group effect is 3.77. The advantage of within-group centering is that the within-group regression coefficient is now conveniently expressed in the model's fixed effects.







*** 


```r
sessionInfo()
```

```
## R version 3.0.2 (2013-09-25)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## 
## locale:
## [1] LC_COLLATE=English_United States.1252 
## [2] LC_CTYPE=English_United States.1252   
## [3] LC_MONETARY=English_United States.1252
## [4] LC_NUMERIC=C                          
## [5] LC_TIME=English_United States.1252    
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] dplyr_0.1.3    lme4_1.1-5     Rcpp_0.11.1    Matrix_1.1-2-2
## [5] knitr_1.5     
## 
## loaded via a namespace (and not attached):
##  [1] assertthat_0.1      evaluate_0.5.1      formatR_0.10       
##  [4] grid_3.0.2          lattice_0.20-27     MASS_7.3-29        
##  [7] minqa_1.2.3         nlme_3.1-111        RcppEigen_0.3.2.1.1
## [10] splines_3.0.2       stringr_0.6.2       tools_3.0.2
```

```r
date()
```

```
## [1] "Tue Mar 25 13:52:23 2014"
```


