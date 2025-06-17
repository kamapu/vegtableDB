library(vegtableDB)
library(divDB)
library(DBI)

conn <- connect_db(dbname = "veg-test")

query_names(conn, "Boraginaceae")
query_names(conn, "Boraginaceae", concepts = TRUE)

# Importing to taxlist
spp1 <- db2taxlist(conn, "ea_splist")
summary(spp1, "Boraginaceae", exact = TRUE)

# Importing to taxlist
spp2 <- db2taxlist(conn, "sam_splist")
summary(spp1, "Boraginaceae", exact = TRUE)

# Merge names
query <- merge_names(conn, c(209700, 199326), eval = FALSE)
query

merge_names(conn, c(209700, 199326))

# Importing to taxlist
spp1 <- db2taxlist(conn, "ea_splist")
summary(spp1, "Boraginaceae", exact = TRUE)

spp2 <- db2taxlist(conn, "sam_splist")
summary(spp1, "Boraginaceae", exact = TRUE)

query_names(conn, "Boraginaceae")

disconnect_db(conn)
