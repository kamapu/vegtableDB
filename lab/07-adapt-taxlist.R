# TODO:   Function db2taxlist() adapted to the new object structure
# 
# Author: Miguel Alvarez
################################################################################

library(RPostgreSQL)
library(dbaccess)
library(taxlist)
library(vegtable)
library(biblioDB)

# Connect to DB
DB <- "vegetation_v3"
conn <- connect_db2(DB, user = "miguel")

# Parameters and arguments
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
