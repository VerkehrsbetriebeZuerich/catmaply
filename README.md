catmaply <img src="man/figures/logo.png" align="right" height="192 px"/>
======================

[![R build status](https://github.com/yvesmauron/catmaply/workflows/R-CMD-check/badge.svg)](https://github.com/yvesmauron/catmaply/actions) [![codecov](https://codecov.io/gh/yvesmauron/catmaply/branch/master/graph/badge.svg)](https://codecov.io/gh/yvesmauron/catmaply) [![](https://img.shields.io/badge/lifecycle-development-blue.svg)](https://www.tidyverse.org/lifecycle/#development)


## Introduction

A heatmap a graphical representation of data that uses a system of color-coding to represent different values. Heatmaps are used in various forms of analytics, however, this R package specifically focuses on providing an efficient way for creating interactive heatmaps for categorical data or continuous data that belongs to seperate groups/categories. 

This package is originally beeing developed for Zurich Public Transport (VBZ) to illustrate the utilization of the different routes and vehicles during different times of the day. It therefore groups utilization data (e.g. persons per m^2) into different categories (e.g. low, medium, high utilization) and plots it for certain stops over time in a heatmap.

This package is can easily be integrated into a shiny dashboard; which supports additional intactions of different kind of plots using plotly events. A mini-demo app is provided with [catmaply_shiny](https://github.com/yvesmauron/catmaply_shiny).

This work is based on the plotly.js engine. 

### Please submit feature requests

This package is still under active development. If you have features you would like to have added, please submit your suggestions (and bug-reports) at: <https://github.com/yvesmauron/catmaply/issues>

## News

You can see the most recent changes to the package in NEWS.md. 


### Code of conduct

Please note that this project is released with a [Contributor Code of Conduct](https://github.com/yvesmauron/catmaply/blob/master/CONDUCT.md). By participating in this project you agree to abide by its terms.



## Installation

To install the latest ("cutting-edge") GitHub version run:

```R
# make sure that you have the corrent R Tools installed.
# as you might need to build some packages from source

# if do not have RTools installed, you can install it with:
# install.packages('installr'); install.Rtools() # not tested on windows
# or download it from here:
# https://cran.r-project.org/bin/windows/Rtools/
# in any case, make sure that you select the correct version, 
# otherwise the installation will fail.

# Then you'll need devtools
if (!require('devtools'))
  install.packages('devtools')

# Finally install the package
devtools::install_github('yvesmauron/catmaply')
```

And then you may load the package using:

```R
library("catmaply")
```
