# TODO:   Test function merging names
# 
# Author: Miguel Alvarez
################################################################################

# Load libraries
## library(dbaccess)
library(RPostgreSQL)
library(vegtableDB)

# Restore database
DB <- "vegetation_v3"

do_restore(dbname = DB,
    user = "miguel",
    filepath = file.path("../../db-dumps/00_dumps", DB),
    path_psql = "/usr/bin")

# Connect database
db <- connect_db(DB, user = "miguel")

query_names(db, "Bryophyta")
query_names(db, "Boraginaceae", concepts = TRUE)


# Import list with duplicated names
Doubles <- readRDS("../../db-dumps/taxasize/data/fix-doubles.rds")

# Reload the names
Query <- paste("select *", "from plant_taxonomy.taxon_names",
    paste0("where usage_name in ('", paste0(Doubles$usage_name,
            collapse = "','"),"')"))
D2 <- dbGetQuery(db, Query)

D2 <- split(D2, with(D2, is.na(author_name) | author_name == ""))
D2$"TRUE"$new_id <- with(D2$"FALSE",
    taxon_usage_id[match(D2$"TRUE"$usage_name, usage_name)])
D2 <- D2$"TRUE"

# 2: Fixind doubles
D3 <-with(D2, dbGetQuery(db, paste("select *",
            "from plant_taxonomy.names2concepts",
            paste0("where taxon_usage_id in (", paste0(c(new_id,
                        taxon_usage_id),
                    collapse = ","), ")"))))
D3$new_usage <- with(D2, replace_x(D3$taxon_usage_id, old = taxon_usage_id,
        new = new_id))
D32 <- D3[duplicated(D3[ , c("new_usage", "taxon_concept_id")]), ]
D2 <- subset(D2, !new_id %in% D32$new_usage)


conn = db
new_id = D2$new_id
old_id = D2$taxon_usage_id
schema = "plant_taxonomy"

query_names(db, D2$usage_name[1])
new_id[1];old_id[1]



fix_double(db, new_id = D2$new_id, old_id = D2$taxon_usage_id)

Spp <- db2taxlist(conn, taxonomy = "ea_splist")
Spp <- db2taxlist(conn, taxonomy = "sam_splist")

Spp1 <- db2taxlist(conn, taxonomy = "ea_splist", as_list = TRUE)
Spp2 <- db2taxlist(conn, taxonomy = "sam_splist", as_list = TRUE)


x_concepts1 <- Spp1$taxonRelations$TaxonConceptID[
    !Spp1$taxonRelations$TaxonConceptID %in% Spp1$taxonNames$TaxonConceptID]
x_concepts2 <- Spp2$taxonRelations$TaxonConceptID[
    !Spp2$taxonRelations$TaxonConceptID %in% Spp2$taxonNames$TaxonConceptID]


Query <- paste("select *",
    "from plant_taxonomy.names2concepts",
    paste0("where taxon_concept_id in (", paste0(x_concepts1, collapse = ","),
        ")"))
C1 <- dbGetQuery(db, Query)

Query <- paste("select *",
    "from plant_taxonomy.taxon_names",
    paste0("where taxon_usage_id in (", paste0(C1$taxon_usage_id,
            collapse = ","), ")"))
N1 <- dbGetQuery(db, Query)

subset(D2, new_id %in% C1$taxon_usage_id)

# Test using source
source("R/fix_double.R")

# Error by generated duplicates
fix_double(db, new_id = D2$new_id, old_id = D2$taxon_usage_id)

fix_double(db, new_id = D2$new_id, old_id = D2$taxon_usage_id)

# Before final execution, do overviews!


taxonomy = "ea_splist"

subset(species_obj$taxonRelations, TaxonConceptID %in% C1$taxon_concept_id)
subset(species_obj$taxonRelations, !is.na(Parent) & TaxonConceptID %in% Parent)

all(species_obj$taxonRelations$TaxonConceptID %in% concepts$TaxonConceptID)
all(concepts$TaxonConceptID %in% species_obj$taxonRelations$TaxonConceptID)

all(species_obj$taxonNames$TaxonUsagetID %in% concepts$TaxonUsageID)
all(concepts$TaxonUsageID %in% species_obj$taxonNames$TaxonUsagetID)

subset(species_obj$taxonNames, TaxonUsageID %in% C1$taxon_usage_id)


species_obj$taxonNames[!concepts$TaxonUsageID %in% species_obj$taxonNames$TaxonUsagetID, ]

length(unique(concepts$TaxonUsageID))

Dupl_n <- concepts[duplicated(concepts$TaxonUsageID), "TaxonUsageID"]
concepts[concepts$TaxonUsageID %in% Dupl_n, ]








