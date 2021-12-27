# TODO:   Adapting import functions to new structure of database
# 
# Author: Miguel Alvarez
################################################################################

library(vegtable)
library(dbaccess)
library(RPostgreSQL)

conn <- connect_db2("veg_databases", user = "miguel")
taxon_names = c("tax_commons", "taxon_names")
taxon_relations = c("swea_dataveg","taxon_concepts")
taxon_traits = c("swea_dataveg","taxon_attributes")
taxon_views = c("bib_references", "main_table")
taxon_levels = c("tax_commons","taxon_levels")
names2concepts = c("swea_dataveg","names2concepts")
subset_levels = TRUE
as_list = FALSE

# Load function
