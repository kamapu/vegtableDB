# TODO:   Test function merging names
# 
# Author: Miguel Alvarez
################################################################################

library(devtools)
install_github("kamapu/vegtableDB", "devel")

# Load libraries
## library(dbaccess)
library(RPostgreSQL)
library(vegtableDB)

# Restore database
DB <- "vegetation_v3"

do_restore(
    dbname = DB,
    user = "miguel",
    filepath = file.path("../../db-dumps/00_dumps", DB)
)

# Connect database
conn <- connect_db(DB, user = "miguel")

# Query a name
(Names <- query_names(conn, "Cyperus el", concepts = TRUE, accepted = TRUE))

# Use Query in db2taxlist (multiple taxonomies => error)
TAX <- db2taxlist(conn, taxonomy = "ea_splist",
    concepts = Names$taxon_concept_id)

# Filter concepts
Concepts <- with(Names, taxon_concept_id[taxonomy == "ea_splist"])

# Only one concept
TAX <- db2taxlist(conn, taxonomy = "ea_splist", concepts = Concepts[1])
TAX

# Both concepts
TAX <- db2taxlist(conn, taxonomy = "ea_splist", concepts = Concepts)
TAX
