Ed. Psych. 964: HLM Assignment 3
===============================================================================

_Tristan Mahr, April 2014_


Codebook
-------------------------------------------------------------------------------

* `id`: subject ID
* `attit`: attitudes toward deviance (composite score). Larger values imply more tolerance toward deviant behaviors by peers (such as cheating, stealing five dollars, etc.).
* `expo`: exposure to delinquent peers (composite score). Larger values imply more frequent contacts with delinquent peers.
* `age`: age of subject
* `age11`: `age` minus 11
* `age13`: `age` minus 13
* `age11s`: `age11` squared
* `age13s`: `age13` squared
* `ind1`--`ind5`: measurement indicator matrix for HLM7.  Same for `age`, `age11`, `age13`.
* `female`: 1 if female, otherwise 0
* `minority`: 1 if minority, otherwise 0
* `income`: family income categories. Larger values imply higher family income.


Exercises
-------------------------------------------------------------------------------

**1\. Fit a linear growth model to these data allowing for random intercepts and slopes using `age11` as the level-one predictor, and `attit` as the outcome. Of the various types of level-1 covariance structures that can be applied using the HLM software, which appears to be the best for this model? Justify your answer.**

**2\. Fit a quadratic growth model using `age11` as a predictor under the homogeneous covariance structure. Allow all of the growth parameters to be random. Report and interpret the parameter estimates. Does it appear necessary to add a quadratic term to accurately model change in attitudes toward deviant behavior?**

**3\. Are `gender`, `minority`, or `income` significant predictors of the random intercepts, slopes, or quadratic terms? Interpret any significant findings.**