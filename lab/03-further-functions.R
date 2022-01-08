# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

library(devtools)
library(dbaccess)
library(RPostgreSQL)
library(rpostgis)

install_github("kamapu/vegtableDB", dependencies = TRUE, ref = "new-db-version")

library(vegtableDB)

# Connect DB
DB <- "veg_databases"

conn <- connect_db2(DB, user = "miguel")

Fobj <- db2taxlist(conn,
    taxon_names = c("tax_commons", "ecoveg_f_names"),
    taxon_relations = c("syntax_ecoveg_f", "taxon_concepts"),
    names2concepts = c("syntax_ecoveg_f", "names2concepts"),
    taxon_traits = c("syntax_ecoveg_f", "taxon_attributes"),
    taxon_views = c("bib_references", "main_table"),
    taxon_levels = c("tax_commons", "ecoveg_f_levels"))

# For function
taxon_names = c("tax_commons", "ecoveg_f_names")
taxon_relations = c("syntax_ecoveg_f", "taxon_concepts")
names2concepts = c("syntax_ecoveg_f", "names2concepts")
taxon_traits = c("syntax_ecoveg_f", "taxon_attributes")
taxon_views = c("bib_references", "main_table")
taxon_levels = c("tax_commons", "ecoveg_f_levels")
clean = TRUE

df = data.frame(taxon_concept_id = 74,
    usage_name = "Blabla bla",
    author_name = "author")







