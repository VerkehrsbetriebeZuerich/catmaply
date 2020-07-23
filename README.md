catmaply  <img src="man/figures/logo.png" align="right" height="192 px"/>
======================

[![R build status](https://github.com/yvesmauron/catmaply/workflows/R-CMD-check/badge.svg)](https://github.com/yvesmauron/catmaply/actions) [![codecov](https://codecov.io/gh/yvesmauron/catmaply/branch/master/graph/badge.svg)](https://codecov.io/gh/yvesmauron/catmaply) [![](https://img.shields.io/badge/lifecycle-development-blue.svg)](https://www.tidyverse.org/lifecycle/#development)


## Introduction

A heatmap is a graphical representation of data that uses a system of color-coding to represent different values. Heatmaps are used in various forms of analytics, however, this R package specifically focuses on providing an efficient way for creating interactive heatmaps for categorical data or continuous data that can be grouped into categories. 

This package is originally beeing developed for Verkehrsbetriebe Zürich (VBZ), the public transport operator in the Swiss city of Zurich, to illustrate the utilization of different routes and vehicles during different times of the day. Therefore, it groups utilization data (e.g. persons per m^2) into different categories (e.g. low, medium, high utilization) and illustrates it for certain stops over time in a heatmap.

This package can easily be integrated into a shiny dashboard which supports additional interactions with other plots (e.g. boxplot, histogram, forecast) by using plotly events. A mini-demo app is provided in a seperate github repository named [catmaply_shiny](https://github.com/yvesmauron/catmaply_shiny).

This work is based on the plotly.js engine. 

### Please submit feature requests

This package is still under active development. If you have features you would like to have added, please submit your suggestions (and bug-reports) at: <https://github.com/yvesmauron/catmaply/issues>

### News

You can see the most recent changes of the package in [NEWS.md](https://github.com/yvesmauron/catmaply/blob/master/NEWS.md).

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
