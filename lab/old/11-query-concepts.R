# TODO:   Test function merging names
# 
# Author: Miguel Alvarez
################################################################################

library(devtools)
install_github("kamapu/vegtableDB", "devel")

# Load libraries
## library(dbaccess)
## library(RPostgreSQL)
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
(Names <- query_names(conn, "Cyperus", concepts = TRUE, exact = TRUE))

# Query concepts
TAX <- query_concepts(conn, "Cyperus el", taxonomy = "ea_splist")
TAX

TAX <- query_concepts(conn, "Cyperus el", taxonomy = "ea_splist",
    keep_parents = TRUE)
TAX
indented_list(TAX)

TAX <- query_concepts(conn, "Bidens", taxonomy = "ea_splist", exact = TRUE,
    keep_parents = TRUE, keep_children = TRUE)
TAX
indented_list(TAX)
