# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

library(vegtableDB)
library(biblio)

Bib <- db2lib_db(connect_db("vegetation_v3", user = "miguel"),
    name = "bib_references",
    file_folder = "/media/miguel/miguel2022/soft-copies")

Refs <- read_bib("../../db-dumps/literatur_db/bib/MiguelReferences.bib")
Refs <- as(Refs, "lib_db")

# Compare versions
Test <- compare_df(Bib, Refs)
Test

from <- Refs
x <- Refs












x <- iris
y <- iris[ , names(iris) != "Species"]
x$ord <- 1:nrow(x)

y$new_var <- rep_len(letters, nrow(y))
y <- do.call(rbind, list(y, y[1:3, ]))
y$ord <- 1:nrow(y)
y <- y[-c(5,7), ]
y[20, 1] <- 1000
y[95, 4] <- 3000



compare_df(x, y, "ord")

source("../biblio/R/print.R")
source("../biblio/R/update_data.R")

z1 <- update_data(x, y, key = "ord", delete = TRUE)
compare_df(x, z1, "ord")

z2 <- update_data(x, y, key = "ord", add = TRUE)
compare_df(x, z2, "ord")

z3 <- update_data(x, y, key = "ord", update = TRUE)
compare_df(x, z3, "ord")

z4 <- update_data(x, y, key = "ord", add = TRUE, delete = TRUE, update = TRUE)
compare_df(x, z4, "ord")

z5 <- x
update_data(z5, "ord", add = TRUE) <- y
compare_df(x, z5, "ord")




object = x
revision = y
key = "ord"






source("../biblio/R/update_data.R")

Test

compare_df(x, x, "ord")


key = "ord"




Bib2 <- as(Bib, "lib_df")

Bib3 <- as(Bib2, "lib_db")
validObject(Bib3)

Bib3 <- Bib
coerce(Bib3) <- "lib_df"




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

setAs(from = "table", to = "data.frame",
    def = function(from) {
      return(as.data.frame(from))
    },
    replace = function(from, value) {
      from <- as(from, value)
      return(from)
    })

setAs(from = "table", to = "data.frame",
    def = function(from) {
      return(as.data.frame(from))
    })

data(Titanic)
x <- Titanic

# two coerce alternatives
y <- as(x, "data.frame")
as(x) <- "data.frame"

as(x, "data.frame") <- x

setReplaceMethod("as", signature(object = "table", Class = "missing",
        value = "character"), 
    function(object, Class, value) {
      object <- as(object, value)
      return(object)
    })
