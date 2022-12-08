# TODO:   Add comment
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

# Load taxonomic list
Spp <- db2taxlist(conn, taxonomy = "ea_splist")

## # Check for new taxa
## summary(Spp, "Ocimum grat")

# New concept
df = data.frame(
    usage_name = c(
        "Ocimum gratissimum var. gratissimum",
        "Ocimum gratissimum var. nova"
            ),
    author_name = c("L.", NA),
    rank = c("variety", "variety"),
    view_key = c("CJBGSANBI2012", NA),
    parent_id = c(400, NA))

library(rpostgis)
source("R/insert_concept.R")

# Error
insert_concept(conn, "ea_splist", df = df)

# Load taxonomic list
Spp <- db2taxlist(conn, taxonomy = "ea_splist")

# Check for new taxa
summary(Spp, "Ocimum grat")

taxonomy = "ea_splist"
schema = "plant_taxonomy"
clean = TRUE

query_names(conn, "Ocimum grat")


df = data.frame(usage_name = "Ocimum gratissimum var. nova",
    author_name = "L.", taxon_concept_id = 400)




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
