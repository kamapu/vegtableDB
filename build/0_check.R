# TODO:   Working script for testing the package 'taxlist'
# 
# Author: Miguel Alvarez
################################################################################

# Required packages
library(devtools)

# Document and Build package
## file.remove("NAMESPACE")
document()
pkg_loc <- build(path = "build")

# Test the package
## Sys.setenv(LANG="en_US.iso88591")
## Sys.setenv('_R_CHECK_SYSTEM_CLOCK_' = 0)
check_built(path = pkg_loc)

## library(covr)
## library(goodpractice)
## library(rmarkdown)
## library(knitr)
## library(pkgdown)


# Report coverage
## report()

# Carry out the tests
## test()

# Write data set
## source("data-raw/Easplist/Easplist.R")

# Purl vignette R-code
## purl("vignettes/taxlist-intro.Rmd", "vignettes/taxlist-intro.R")

# Check application of good practices
## gp()


# After check ------------------------------------------------------------------

# Install the package
## install()

# Render readme-file.
## render("README.Rmd")

# Render package-site
## usethis::use_pkgdown()
## pkgdown::build_site(preview=FALSE)
## 
## # Copy site
## r_path <- gsub("/taxlist", "", getwd())
## pkg_path <- file.path(r_path, "kamapu.github.io", "rpkg")
## 
## file.copy("docs", pkg_path, recursive=TRUE)
## unlink("docs", recursive=TRUE)
## 
## unlink(file.path(pkg_path, "taxlist"), recursive=TRUE)
## file.rename(file.path(pkg_path, "docs"), file.path(pkg_path, "taxlist"))
## 
## file.copy("README-figures", file.path(pkg_path, "taxlist"), recursive=TRUE)
