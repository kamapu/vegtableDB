# TODO:   Function db2taxlist() adapted to the new object structure
# 
# Author: Miguel Alvarez
################################################################################

library(RPostgreSQL)
library(dbaccess)
library(taxlist)
library(vegtable)
library(biblioDB)
library(sf)
library(stringr)

# Connect to DB
DB <- "vegetation_v3"
conn <- connect_db2(DB, user = "miguel")

source("R/db2taxlist.R")

# Parameters and arguments
database = "sudamerica"
geometry = "plot_centroid"


i <- 1
taxonomy = t_syntax$taxonomy[i]
schema = t_syntax$approach[i]

colnames(Test) <-
    replace_x(colnames(Test),
        old = "taxon_concept_id", new = "TaxonConceptID"
    )


summary(as.factor(unlist(dbGetQuery(conn,	
                paste("select top_view", "from syntax_bbl.taxon_concepts")))))

summary(as.factor(unlist(dbGetQuery(conn,	
                paste("select top_view", "from syntax_ecoveg.taxon_concepts")))))


schema = "plant_taxonomy"
taxonomy = "sam_splist"
subset_levels = TRUE
as_list = FALSE

Spp1 <- db2taxlist(conn, taxonomy = "sam_splist")
Spp2 <- db2taxlist(conn, taxonomy = "ea_splist")

summary(Spp1)
summary(Spp2)

summary(Spp1, "Juncus proc", secundum = "bibtexkey")
indented_list(Spp1, "Juncus proc")

summary(Spp2, "papyrus", secundum = "bibtexkey")
indented_list(Spp2, "papyrus")
