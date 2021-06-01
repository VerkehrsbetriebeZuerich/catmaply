## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
# make sure that you have the corrent RTools installed.
# as you might need to build some packages from source
# if you don't have RTools installed, you can install it with:
# install.packages('installr'); install.Rtools() # not tested on windows
# or download it from here:
# https://cran.r-project.org/bin/windows/Rtools/
# in any case, make sure that you select the correct version, 
# otherwise the installation will fail.
# then you'll need devtools
# if (!require('devtools'))
  # install.packages('devtools')
# finally install the package
# devtools::install_github('VerkehrsbetriebeZuerich/catmaply')

## -----------------------------------------------------------------------------
#install.packages("catmaply")

