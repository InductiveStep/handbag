# This is some housekeeping code to get things going

# Using this very handy page:
# https://tinyheero.github.io/jekyll/update/2015/07/26/making-your-first-R-package.html


# Run this once at the beginning to make a package

library(devtools)
library(roxygen2)
library(here)

devtools::create("../handbag") # in retrospect, "." might have done
                               # as a path?



# Sets up the docs

devtools::document()
