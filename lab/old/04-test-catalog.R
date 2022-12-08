# TODO:   Test the use of catalog for import session
# 
# Author: Miguel Alvarez
################################################################################

library(devtools)

install_github("kamapu/vegtableDB", ref = "catalogue")

library(RPostgreSQL)
library(dbaccess)
library(vegtableDB)

DB <- "veg_databases"
conn <- connect_db2(DB, user = "miguel")

# Test db2taxlist
load("R/sysdata.rda")
taxonomy = "sudamerica"
subset_levels = TRUE
as_list = FALSE

(Test <- db2taxlist(conn, "sudamerica"))
(Test <- db2taxlist(conn, "swea_dataveg"))
(Test <- db2taxlist(conn, "sam"))
(Test <- db2taxlist(conn, "ecoveg"))
