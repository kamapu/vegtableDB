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
library(rpostgis)

DB <- "veg_databases"
conn <- connect_db2(DB, user = "miguel")

# Import EcoVeg
ecoveg <- db2taxlist(conn, "ecoveg")

summary(ecoveg, 74)

insert_concept(conn, "ecoveg", data.frame(
        usage_name = "blabla bla",
        author_name = "myself 2022"
))

ecoveg <- db2taxlist(conn, "ecoveg")
summary(ecoveg, 75)

insert_synonym(conn, "ecoveg", data.frame(
        usage_name = "blabla blu",
        author_name = "other one (2021)",
        taxon_concept_id = 75
    ))

ecoveg <- db2taxlist(conn, "ecoveg")
summary(ecoveg, 75)

# Test db2vegtable
load("R/sysdata.rda")
taxonomy = "ecoveg"
df = data.frame(usage_name = "blabla bla", author_name = "myself 2022")

# Restore database
DB <- "veg_databases"
do_restore(dbname = DB,
    user = "miguel",
    filepath = file.path("../../db-dumps/00_dumps", DB),
    path_psql = "/usr/bin")

DBI::dbDisconnect(conn)
