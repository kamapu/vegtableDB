# TODO:   Adapting import functions to new structure of database
# 
# Author: Miguel Alvarez
################################################################################

# New Session
library(vegtable)
library(dbaccess)
library(RPostgreSQL)
library(rpostgis)
library(biblioDB)

# For debug
conn <- connect_db2("veg_databases", user = "miguel")

header = c("swea_dataveg","header")
samples = c("swea_dataveg","samples")
relations = list(
    globe_plots = c("swea_dataveg","globe_plots"),
    swea1_code = c("swea_dataveg","swea1_code"),
    soil_moisture = c("swea_dataveg","soil_moisture"),
    soil_texture = c("swea_dataveg","soil_texture"),
    community_type = c("commons","community_type"),
    naturalness = c("swea_dataveg","naturalness"),
    record_type = c("swea_dataveg","record_type")
)
layers = list(
    veg_layer = c("swea_dataveg","veg_layer"),
    spec_miguel = c("specimens","specimens_miguel")
)
coverconvert = list(
    br_bl = c("coverconvert","br_bl"),
    b_bbds = c("coverconvert","b_bbds"),
    ordinal = c("coverconvert","ordinal")
)
geometry = "plot_centroid"
as_list = FALSE

veg_obj <- list()
veg_obj$species <- swea_tax(conn)

## taxon_names = c("tax_commons", "taxon_names")
## taxon_relations = c("swea_dataveg","taxon_concepts")
## taxon_traits = c("swea_dataveg","taxon_attributes")
## taxon_views = c("bib_references", "main_table")
## taxon_levels = c("tax_commons","taxon_levels")
## names2concepts = c("swea_dataveg","names2concepts")
## subset_levels = TRUE
## as_list = FALSE

# Test final function
library(devtools)

install()

rm(list=ls()[ls() != "conn"])

library(vegtableDB)

(Spp <- swea_tax(conn))

(Spp <- sam_tax(conn))

(Veg <- import_swea(conn))

(Veg <- import_sam(conn))

# Reimport function
source("R/db2vegtable.R")
