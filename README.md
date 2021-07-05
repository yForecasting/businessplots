# businessplots
Plots in R for business use

## Installation

This package is available via github and can be installed using "devtools" in R. First make sure that you have devtools:
> if (!require("devtools")){install.packages("devtools")}

and after that run:
> devtools::install_github("yForecasting/businessplots")


## For the contributers, how to commit:

1) Pull git
2) Open Rproj
3) Add code ;-)
4) Validate package via:

> library(devtools)
> setwd("...") # to dir above "businessplots"
> document("businessplots")
> check("businessplots")

5) make a commit in git
6) Sync by Pull then Push

All done !

## For contributers, how to add a dependency package:
1) in console: use_package("...")
2) in fun.R file header: @import ...
