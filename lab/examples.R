# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

library(vegtableDB)
library(biblio)

source("R/lib_db-class.R")
source("R/db2lib_db.R")
source("R/file_list.R")

Bib <- db2lib_db(connect_db("vegetation_v3", user = "miguel"),
    name = "bib_references",
    file_folder = "/media/miguel/miguel2022/soft-copies")

Test <- file_list2string(Bib)


Test <- file_list2string(Bib@file_list)




Bib <- read_bib("../../db-dumps/literatur_db/bib/MiguelReferences.bib")

Bib <- db2lib_db()



Test <- file_list(Bib)

x <- Test


x <- Bib



x <- new("lib_db")

?rep

rep_len(c(4,5,6), 2)

file_x <- NULL
is.null(file_x)


# Test reading bibliography
library(RPostgreSQL)


Bib <- db2lib_db(connect_db("vegetation_v3", user = "miguel"),
    name = "bib_references",
    file_folder = "/media/miguel/miguel2022/soft-copies")

validObject(Bib)
