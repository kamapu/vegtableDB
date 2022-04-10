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
check_tax(conn)






(Names <- query_names(conn, "Cyperus el", concepts = TRUE, accepted = TRUE))


schema = "plant_taxonomy"


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

# incluging parents
TAX <- db2taxlist(conn, taxonomy = "ea_splist", concepts = Concepts,
    keep_parents = TRUE)
TAX
indented_list(TAX, synonyms = TRUE)

# The opposite direction
(Names <- query_names(conn, "Bidens", concepts = TRUE, accepted = TRUE))

TAX <- db2taxlist(conn, taxonomy = "ea_splist", concepts = 54793,
    keep_children = TRUE, keep_parents = TRUE)
TAX
indented_list(TAX)


TAX <- db2taxlist(conn, taxonomy = "ea_splist")
TAX <- db2taxlist(conn, taxonomy = "sam_splist")

# Including parents and children
species_obj <- list()
taxonomy = "ea_splist"
schema = "plant_taxonomy"
## concepts = Concepts
subset_levels = TRUE
keep_parents = FALSE
keep_children = FALSE
as_list = FALSE


concepts = 54793


(Names <- query_names(conn, "Bidens", concepts = TRUE, accepted = TRUE))
subset(Names, usage_name == "Bidens")

