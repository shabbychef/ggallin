

# ggallin

[![Build Status](https://travis-ci.org/shabbychef/ggallin.png)](https://travis-ci.org/shabbychef/ggallin)
[![codecov.io](http://codecov.io/github/shabbychef/ggallin/coverage.svg?branch=master)](http://codecov.io/github/shabbychef/ggallin?branch=master)
[![CRAN](http://www.r-pkg.org/badges/version/ggallin)](https://cran.r-project.org/package=ggallin)
[![Downloads](http://cranlogs.r-pkg.org/badges/ggallin?color=green)](http://www.r-pkg.org/pkg/ggallin)
[![Total](http://cranlogs.r-pkg.org/badges/grand-total/ggallin?color=green)](http://www.r-pkg.org/pkg/ggallin)

>	*If you think I'm into this for the money you're dead wrong because I'm not doing this for the money.  I'm doing it because it lives inside of me.* -- GG Allin

A grab bag of _ggplot2_ extensions and hacks.

-- Steven E. Pav, shabbychef@gmail.com

## Installation

This package can be installed 
from CRAN (not yet), 
via [drat](https://github.com/eddelbuettel/drat "drat"), or
from github:


```r
# via CRAN: (not yet) install.packages('ggallin')
# via drat:
if (require(drat)) {
    drat:::add("shabbychef")
    install.packages("ggallin")
}
# get snapshot from github (may be buggy)
if (require(devtools)) {
    install_github("shabbychef/ggallin")
}
```

## `geom_cloud`

This `geom` acts nearly as a drop-in replacement for `geom_errorbar`,
converting `ymin` and `ymax` into 'clouds' of uncertainty with alpha
proportional to normal density. 


```r
library(ggplot2)
library(ggallin)
library(dplyr)

nobs <- 1000

set.seed(2134)
mydat <- data.frame(grp = sample(c(0, 1), nobs, replace = TRUE), 
    colfac = sample(letters[1:2], nobs, replace = TRUE), 
    rowfac = sample(letters[10 + (1:3)], nobs, replace = TRUE)) %>% 
    mutate(x = seq(0, 1, length.out = nobs) + 0.33 * 
        grp) %>% mutate(y = 0.25 * rnorm(nobs) + 2 * 
    grp) %>% mutate(grp = factor(grp)) %>% mutate(se = sqrt(x)) %>% 
    mutate(ymin = y - se, ymax = y + se)

offs <- 2
ph <- mydat %>% mutate(y = y + offs, ymin = ymin + 
    offs, ymax = ymax + offs) %>% ggplot(aes(x = x, 
    y = y, ymin = ymin, ymax = ymax, color = grp, fill = grp)) + 
    facet_grid(rowfac ~ colfac) + scale_y_sqrt() + 
    geom_line() + geom_cloud(aes(fill = grp), steps = 15, 
    max_alpha = 0.85, color = NA) + labs(title = "geom cloud")
print(ph)
```

<img src="man/figures/geom_cloud-1.png" title="plot of chunk geom_cloud" alt="plot of chunk geom_cloud" width="600px" height="500px" />

