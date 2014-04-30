---
title       : Ed. Psych. 964: HLM Assignment 3
author      : Tristan Mahr
---




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




First look at the data
-------------------------------------------------------------------------------


```r
head(d)
```

```
## Error: object 'd' not found
```

```r

alpha_blue <- adjustcolor("steelblue", alpha.f = 0.25)
ggplot(d, aes(x = age, y = attit, group = id)) + stat_smooth(method = "lm", 
    se = FALSE, color = alpha_blue) + stat_smooth(mapping = aes(group = NULL), 
    method = "lm", size = 1.5, se = FALSE, color = "black")
```

```
## Error: could not find function "ggplot"
```

```r

# Quadratic growth
ggplot(d, aes(x = age, y = attit, group = id)) + stat_smooth(method = "lm", 
    formula = y ~ x + I(x^2), se = FALSE, color = alpha_blue) + stat_smooth(mapping = aes(group = NULL), 
    method = "lm", formula = y ~ x + I(x^2), size = 1, se = FALSE, color = "black") + 
    facet_grid(female ~ minority)
```

```
## Error: could not find function "ggplot"
```

```r

ggplot(d, aes(x = age, y = attit, group = age)) + geom_boxplot()
```

```
## Error: could not find function "ggplot"
```

