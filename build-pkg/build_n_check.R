# TODO:   Working script for testing the package 'taxlist'
# 
# Author: Miguel Alvarez
################################################################################

# Required packages
library(devtools)
library(styler)

# Clean session
rm(list = ls())

# Clean folder
unlink(file.path("build-pkg", list.files("build-pkg", ".tar.gz")))
unlink(file.path("build-pkg", list.files("build-pkg", ".pdf")))

# Apply style and document
style_pkg()
document()

# Build and check
pkg_loc <- build(path = "build-pkg")
check_built(path = pkg_loc)

# Build manual and install
build_manual(path = "build-pkg")
install()
