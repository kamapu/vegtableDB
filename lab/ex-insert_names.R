# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

library(vegtableDB)
library(backpackR)

# Restore database
db_path <- "../../db-dumps/00_dumps/vegetation-db/backups-v4"
db_name <- "vegetation-db"

## sort_releases(db_path, db_name)

cred <- credentials()

build_db(db_path, db_name, release = 6, user = cred["user"],
    password = cred["password"], auxiliar_db = "test-db", overwrite = TRUE)

db_name <- "test-db"

# Check taxonomic list
conn <- connect_db(db_name, user = cred["user"], password = cred["password"])
tax <- db2taxlist(conn, "sam_splist")

new_spp <- data.frame(
    usage_name = c("Gnaphalium uliginosum", "Pseudognaphalium exoticum"),
    author_name =c("L.", "M. Alvarez"))

# Arguments
#conn
df = new_spp
schema = "plant_taxonomy"
eval = TRUE
library(DBI)




summary(tax, "Gnaphalium u")
summary(tax, "Gnaphalium", exact = TRUE)

indented_list(tax, "Gnaphalium", exact = TRUE)




