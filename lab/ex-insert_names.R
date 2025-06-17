# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

library(vegtableDB)
library(backpackR)

# Request credentials
cred <- credentials()

# Restore database
db_path <- "../../db-dumps/00_dumps/vegetation-db/backups-v4"
db_name <- "vegetation-db"

## sort_releases(db_path, db_name)

build_db(db_path, db_name, release = 7, user = cred["user"],
    password = cred["password"], auxiliar_db = "test-db", overwrite = TRUE)

db_name <- "test-db"

# Check taxonomic list
conn <- connect_db(db_name, user = cred["user"], password = cred["password"])
tax <- db2taxlist(conn, "sam_splist")

new_spp <- data.frame(
    usage_name = c("Gnaphalium uliginosum", "Pseudognaphalium exoticum",
        "Gnaphalium communis"),
    author_name =c("L.", "M. Alvarez", "non L."),
    wfo_id = c(-1:-3))

# Do the query and execute
query <- insert_names(conn, new_spp, schema = "plant_taxonomy",
    eval = FALSE)
query

insert_names(conn, new_spp, schema = "plant_taxonomy")

# cross-check
for (i in new_spp$usage_name)
  print(query_names(conn, i))

# Including update of values
new_spp2 <- data.frame(
    usage_name = c("Gnaphalium uliginosum", "Pseudognaphalium rex"),
    author_name =c("L.", "M. Alvarez"),
    wfo_id = c(-1:-2),
    url = "Error 404")

# Do the query and execute
query <- insert_names(conn, new_spp2, schema = "plant_taxonomy",
    eval = FALSE, update = TRUE)
query

insert_names(conn, new_spp2, schema = "plant_taxonomy", update = TRUE)

# cross-check
for (i in new_spp2$usage_name)
  print(query_names(conn, i))
