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

# Only names case-insensitve
Names <- query_name(conn, "cyper")
head(Names)

# Only names case-sensitive
Names <- query_name(conn, "cyper", case = TRUE)
head(Names)

Names <- query_name(conn, "Cyper", case = TRUE)
head(Names)

# Names and concepts
Names <- query_name(conn, "Cyperus el", concepts = TRUE)
head(Names)

Names <- query_name(conn, "Cyperus el", concepts = TRUE, accepted = TRUE)
head(Names)

# Test option for queried concepts using db2taxlist


new_id <- Names$taxon_usage_id

new_id <- c(new_id, 2000000)


old_id <- Names$taxon_usage_id
schema = "plant_taxonomy"

query <- "Cyperus el"
case = FALSE







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

# Test using source
source("R/fix_double.R")

# Error by generated duplicates
fix_double(db, new_id = D2$new_id, old_id = D2$taxon_usage_id)

# 2: Fixind doubles
TAX <-with(D2, dbGetQuery(db, paste("select *",
            "from plant_taxonomy.names2concepts",
            paste0("where taxon_usage_id in (", paste0(c(new_id,
                        taxon_usage_id),
                    collapse = ","), ")"))))
TAX$new_usage <- with(D2, replace_x(TAX$taxon_usage_id, old = taxon_usage_id,
        new = new_id))
TAX2 <- TAX[duplicated(TAX[ , c("new_usage", "taxon_concept_id")]), ]
D2 <- subset(D2, !new_id %in% TAX2$new_usage)

fix_double(db, new_id = D2$new_id, old_id = D2$taxon_usage_id)

# Before final execution, do overviews!
