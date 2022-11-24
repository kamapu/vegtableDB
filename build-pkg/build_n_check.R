# TODO:   Working script for testing the package 'taxlist'
# 
# Author: Miguel Alvarez
################################################################################

library(devtools)
library(styler)

# Clean session
rm(list = ls())

# Clean folder
unlink(file.path("build-pkg", list.files("build-pkg", ".tar.gz")))
unlink(file.path("build-pkg", list.files("build-pkg", ".pdf")))

# Write data
## source("data-raw/create-data.R")

# re-style scripts
style_pkg()

# Write internal data
## source("data-raw/write-data.R")

# write documentation
document()

# Build and check package
pkg_loc <- build(path = "build-pkg", args = "--resave-data")
check_built(path = pkg_loc)

# Post-Test --------------------------------------------------------------------

# write manual
build_manual(path = "build-pkg")

# install
install()
