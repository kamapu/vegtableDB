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

# Apply style to scripts
style_pkg()

# Document and build
document()
pkg_loc <- build(path = "build-pkg")
build_manual(path = "build-pkg")

# Check package
check_built(path = pkg_loc)
