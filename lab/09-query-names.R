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
Names <- query_names(conn, "cyper")
head(Names)

# Only names case-sensitive
Names <- query_names(conn, "cyper", case = TRUE)
head(Names)

Names <- query_names(conn, "Cyper", case = TRUE)
head(Names)

# Names and concepts
Names <- query_names(conn, "Cyperus el", concepts = TRUE)
head(Names)

Names <- query_names(conn, "Cyperus el", concepts = TRUE, accepted = TRUE)
head(Names)
