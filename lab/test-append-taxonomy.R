# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

library(vegtableDB)
## library(taxlist)
library(biblio)
library(RPostgreSQL)

# Import species list
spp <- readRDS("../../super-miguel/wfo/data/wfo_v1.7.rds")

# Import Bib and set as view
bibl <- read_bib("../../db-dumps/literatur_db/bib/MiguelReferences.bib")
bibl <- subset(bibl, bibtexkey == "WFOTeam2021")

spp@taxonRelations$ViewID <- 1
spp@taxonViews <- data.frame(ViewID = 1, bibl)
class(spp@taxonViews) <- c("lib_df", "data.frame")

# Select some species
list_spp <- with(spp@taxonRelations, sample(TaxonConceptID[Level == "species"],
        size = 20, replace = FALSE))

spp2 <- subset(spp, TaxonConceptID %in% list_spp[1:15], slot = "taxonRelations",
    keep_children = TRUE, keep_parents = TRUE)
spp2

indented_list(spp2)

# Connect to database
conn <- connect_db("example_db", user = "miguel")

# Create new taxonomy
dbSendQuery(conn, "drop schema if exists plant_taxonomy cascade")
dbSendQuery(conn, "drop schema if exists bib_references cascade")

schema = "plant_taxonomy"
schema_refs = "bib_references"
load("R/sysdata.rda")




new_taxonomy(conn, spp2, taxonomy = "wfo", schema = "plant_taxonomy",
    schema_refs = "bib_references")

# Add new columns in table "taxon_names"
dbSendQuery(conn, paste("alter table plant_taxonomy.taxon_names",
        "add column wfo_id numeric,", "add column url text"))

tax_names <- dbReadTable(conn, c("plant_taxonomy", "taxon_names"))

# New names from new taxonomy
spp3 <- subset(spp, TaxonConceptID %in% list_spp,
    slot = "taxonRelations", keep_children = TRUE, keep_parents = TRUE)
spp3

indented_list(spp3)

new_names <- spp3@taxonNames
colnames(new_names) <- replace_x(colnames(new_names),
    old = c("TaxonName", "AuthorName"),
    new = c("usage_name", "author_name"))


# Append new taxonomy




obj@taxonTraits <-
    data.frame(TaxonConceptID = obj@taxonRelations$TaxonConceptID,
        t1 = rep("fake trait", nrow(obj@taxonRelations)))

dbSendQuery(conn, paste("alter table plant_taxonomy.taxon_attributes",
        "add column t1 text"))



insert_names(conn, new_names, "plant_taxonomy")

dbGetQuery(conn, "select max(taxon_usage_id) from plant_taxonomy.taxon_names")
dbGetQuery(conn,
    "select nextval('plant_taxonomy.taxon_names_taxon_usage_id_seq')")
dbGetQuery(conn,
    "select setval('plant_taxonomy.taxon_names_taxon_usage_id_seq', (select max(taxon_usage_id) from plant_taxonomy.taxon_names))")

insert_names(conn, new_names, "plant_taxonomy")

tax_names <- dbReadTable(conn, c("plant_taxonomy", "taxon_names"))
