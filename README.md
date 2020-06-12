catmaply <img src="man/figures/logo.png" align="right" height="192 px"/>
======================

[![R build status](https://github.com/yvesmauron/catmaply/workflows/R-CMD-check/badge.svg)](https://github.com/yvesmauron/catmaply/actions) [![codecov](https://codecov.io/gh/yvesmauron/catmaply/branch/master/graph/badge.svg)](https://codecov.io/gh/yvesmauron/catmaply) [![](https://img.shields.io/badge/lifecycle-development-blue.svg)](https://www.tidyverse.org/lifecycle/#development)


## Introduction

A heatmap is a popular graphical method for visualizing high-dimensional data, in which a table of numbers are encoded as a grid of colored cells. The rows and columns of the matrix are ordered to highlight patterns and are often accompanied by dendrograms. Heatmaps are used in many fields for visualizing observations, correlations, missing values patterns, and more.

Interactive heatmaps allow the inspection of specific value by hovering the mouse over a cell, as well as zooming into a region of the heatmap by dragging a rectangle around the relevant area.

This work is based on the ggplot2 and plotly.js engine. It produces similar heatmaps as d3heatmap (or the static heatmap.2 from gplots), with the advantage of more features such as speed (plotly.js is able to handle larger size matrix), sidebar annotation, and the ability to zoom from the dendrogram.


### Please submit features requests

This package is still under active development. If you have features you would like to have added, please submit your suggestions (and bug-reports) at: <https://github.com/yvesmauron/catmaply/issues>

## News

tbd


### Code of conduct

Please note that this project is released with a [Contributor Code of Conduct](https://github.com/talgalili/heatmaply/blob/master/CONDUCT.md). By participating in this project you agree to abide by its terms.



## Installation

To install the stable version on CRAN:

```r
install.packages('heatmaply')
```

To install the latest ("cutting-edge") GitHub version run:

```R

# good packages to install for this to work smoothly:

install.packages(c("Rcpp","ggplot2","munsell","htmltools","DBI","assertthat",
"gridExtra","digest","fpc","TSP","registry","gclus","gplots","RColorBrewer",
"stringr","labeling","yaml"))

# You'll need devtools
install.packages.2 <- function (pkg) if (!require(pkg)) install.packages(pkg);
install.packages.2('devtools')
# make sure you have Rtools installed first! if not, then run:
#install.packages('installr'); install.Rtools()

devtools::install_github("ropensci/plotly") # you will probably benefit from the latest version of plotly
devtools::install_github('talgalili/heatmaply')
```

And then you may load the package using:

```R
library("heatmaply")
```
