library(vegtableDB)
library(divDB)
library(DBI)

conn <- connect_db(dbname = "veg-test")

query_names(conn, "Boraginaceae")
query_names(conn, "Boraginaceae", concepts = TRUE)

usage_id <- c(209700, 199326)
schema <- "plant_taxonomy"
relations <- NULL
eval <- TRUE
relations




query_concepts(conn, "Boraginaceae")





