# TODO:   Test the use of catalog for import session
# 
# Author: Miguel Alvarez
################################################################################

library(devtools)

install_github("kamapu/vegtableDB", ref = "catalogue")

library(RPostgreSQL)
library(dbaccess)
library(vegtableDB)

library(sf)

DB <- "veg_databases"
conn <- connect_db2(DB, user = "miguel")

# Test db2vegtable
load("R/sysdata.rda")
database = "sudamerica"
geometry = "plot_centroid"
simplify_header = TRUE
subset_levels = TRUE
as_list = FALSE

(Test <- db2vegtable(conn, "sudamerica"))

Test <- db2vegtable(conn, "sudamerica", as_list = TRUE)


i <- "plot_centroid"
any(!Test$header[, i] %in% Test$relations[[i]][, i] &
        !is.na(Test$header[, i]))

Test2 <- Test$header[!Test$header[, i] %in% Test$relations[[i]][, i],]





T1 <- Test$head[,i]
T2 <- as.data.frame(Test$relations[[i]])[,i]

T3 <- as.data.frame(iris)

(Test <- db2taxlist(conn, "swea_dataveg"))
(Test <- db2taxlist(conn, "sam"))
(Test <- db2taxlist(conn, "ecoveg"))


veg_obj$species <- db2taxlist(conn, taxonomy)
