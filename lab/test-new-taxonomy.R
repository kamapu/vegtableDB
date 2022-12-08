# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

library(vegtableDB)
## library(taxlist)
library(biblio)

# Import species list
spp <- readRDS("../../super-miguel/wfo/data/wfo_v1.7.rds")

# Import Bib and set as view
bibl <- read_bib("../../db-dumps/literatur_db/bib/MiguelReferences.bib")
bibl2 <- subset(bibl, bibtexkey == "WFOTeam2021")

spp@taxonRelations$ViewID <- 1
spp@taxonViews <- data.frame(ViewID = 1, bibl2)
class(spp@taxonViews) <- c("lib_df", "data.frame")

# Select some species
list_spp <- with(spp@taxonRelations, sample(TaxonConceptID[Level == "species"],
        25, replace = FALSE))

spp2 <- subset(spp, TaxonConceptID %in% list_spp[1:15], slot = "taxonRelations",
    keep_children = TRUE, keep_parents = TRUE)

spp3 <- subset(spp, TaxonConceptID %in% list_spp,
    slot = "taxonRelations", keep_children = TRUE, keep_parents = TRUE)

# Connect to database
conn <- connect_db("example_db", user = "miguel")

# Tests ------------------------------------------------------------------------

dbSendQuery(conn, "drop schema if exists plant_taxonomy cascade")
dbSendQuery(conn, "drop schema if exists bib_references cascade")

# Empty database
new_taxonomy(conn, schema = "plant_taxonomy", schema_refs = "bib_references")

# Add references
update_data(conn, revision = bibl, schema = "bib_references", add = TRUE)

# Append taxonomy
append_taxonomy(conn, obj = spp2, taxonomy = "wfo", schema = "plant_taxonomy",
    schema_refs = "bib_references")

spp2_db <- db2taxlist(conn, taxonomy = "wfo")
spp2_db
spp2

# Append another taxonomy
append_taxonomy(conn, obj = spp3, taxonomy = "wfo2", schema = "plant_taxonomy",
    schema_refs = "bib_references")

spp3_db <- db2taxlist(conn, taxonomy = "wfo2")
spp3_db
spp3

# Once more
append_taxonomy(conn, obj = spp, taxonomy = "wfo3", schema = "plant_taxonomy",
    schema_refs = "bib_references")

spp4_db <- db2taxlist(conn, taxonomy = "wfo3")
spp4_db
spp

# Final test -------------------------------------------------------------------

dbSendQuery(conn, "drop schema if exists plant_taxonomy cascade")
dbSendQuery(conn, "drop schema if exists bib_references cascade")

# Empty database
add2names <- data.frame(name = c("wfo_id", "url"), type = c("numeric", "text"),
    comment = c("ID at the World Flora Online.",
        "Link to the name's information."))

new_taxonomy(conn, schema = "plant_taxonomy", schema_refs = "bib_references",
    add_to_names = add2names)

# Add references
update_data(conn, revision = bibl, schema = "bib_references", add = TRUE)

# Append taxonomy
append_taxonomy(conn, obj = spp, taxonomy = "wfo", schema = "plant_taxonomy",
    schema_refs = "bib_references")

spp_db <- db2taxlist(conn, taxonomy = "wfo")
spp_db
spp
