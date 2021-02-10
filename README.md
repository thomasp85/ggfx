
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggfx

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/thomasp85/ggfx/branch/master/graph/badge.svg)](https://codecov.io/gh/thomasp85/ggfx?branch=master)
[![R-CMD-check](https://github.com/thomasp85/ggfx/workflows/R-CMD-check/badge.svg)](https://github.com/thomasp85/ggfx/actions)
<!-- badges: end -->

ggfx is a (currently experimantal) package that allows the use of
various filters and shaders on ggplot2 layers. At the moment it
implements a blur filter but more will be added, along with the
possibility to use custom filters.

## Installation

ggfx is not on CRAN yet (but will probably at some point). In the mean
time, if you are craving for that sweet sweet blur, you can install it
from github:

``` r
# install.packages('devtools')
devtools::install_github('thomasp85/ggfx')
```

## Example

The basic API of ggfx is to provide a range of `with_*()` modifier
functions instead of special versions of common geoms. This means that
ggfx will work with any geom from ggplot2 and the extension packages (I
think…). And example of blurring a point geom can be seen below.

``` r
library(ggplot2)
library(ggfx)
ggplot(mtcars, aes(mpg, disp)) + 
  with_blur(geom_point(size = 3), sigma = 3) + 
  geom_point()
```

<img src="man/figures/README-example-1.png" width="100%" />

## Code of Conduct

Please note that the ggfx project is released with a [Contributor Code
of Conduct](https://contributor-covenant.org/version/1/0/0/). By
contributing to this project, you agree to abide by its terms.
