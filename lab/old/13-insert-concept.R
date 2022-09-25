# TODO:   Test for inserting a concept in database
# 
# Author: Miguel Alvarez
################################################################################

remotes::install_github("kamapu/vegtableDB")
remotes::install_github("kamapu/specimensDB", "devel")

library(vegtableDB)
library(specimensDB)
library(RPostgreSQL)

# Restore PostgreSQL database and connect
DB <- "vegetation_v3"

do_restore(
    dbname = DB,
    user = "miguel",
    filepath = file.path("../../db-dumps/00_dumps", DB)
)

conn <- connect_db(DB, user = "miguel")

# New concept 
df = data.frame(
    usage_name = "Isoëtaceae",
    author_name = "Dumort.",
    rank = "family",
    view_key = "Zuloaga2015"
)

insert_concept(conn, "sam_splist", df = df)

# Cross-check
Spp <- db2taxlist(conn, "sam_splist")
summary(Spp, "Isoëtaceae", secundum = "bibtexkey")
