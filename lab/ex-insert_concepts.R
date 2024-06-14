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

build_db(db_path, db_name, release = 6, user = cred["user"],
    password = cred["password"], auxiliar_db = "test-db", overwrite = TRUE)

db_name <- "test-db"

# Check taxonomic list
conn <- connect_db(db_name, user = cred["user"], password = cred["password"])
tax <- db2taxlist(conn, "sam_splist")

# Check occurrence in species list
summary(tax, "Gnaphalium uliginosum")

# New taxa
new_spp <- data.frame(
    usage_name = c("Gnaphalium uliginosum", "Gnaphalium exoticum",
        "Gnaphalium communis"),
    author_name =c("L.", "M. Alvarez", "non L."),
    rank = "species",
    parent_id = 59531)

# Try with no existing names
insert_concepts(conn, "sam_splist", df = new_spp, eval = FALSE)

# With existing name
query <- insert_concepts(conn, "sam_splist", df = new_spp[1, ], eval = FALSE)
query

insert_concepts(conn, "sam_splist", df = new_spp[1, ])

# Taxonomia
tax <- db2taxlist(conn, "sam_splist")
summary(tax, "Gnaphalium uliginosum")
