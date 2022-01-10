# TODO:   Test the use of catalog for import session
# 
# Author: Miguel Alvarez
################################################################################

library(devtools)

install_github("kamapu/vegtableDB", ref = "catalogue")

library(RPostgreSQL)
library(dbaccess)
library(vegtableDB)
library(biblioDB)

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
(Test <- db2vegtable(conn, "swea_dataveg"))
