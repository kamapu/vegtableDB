# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

library(vegtableDB)

source("R/lib_db-class.R")


x <- new("lib_db")

?rep

rep_len(c(4,5,6), 2)

file_x <- NULL
is.null(file_x)


# Test reading bibliography
library(RPostgreSQL)
source("R/db2lib_db.R")

Bib <- db2lib_db(connect_db("vegetation_v3", user = "miguel"),
    name = "bib_references",
    file_folder = "/media/miguel/miguel2022/soft-copies")

validObject(Bib)
