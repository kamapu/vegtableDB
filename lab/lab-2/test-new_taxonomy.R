# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

library(vegtableDB)
library(RPostgreSQL)

# Import taxonomy
syntax <- readRDS("syntax/syntax.rds")

# Establish connection to test db
conn <- connect_db("tax_test", user = "miguel")
conn2 <- connect_db("vegetation_v3", user = "miguel")

schema = "syntax_bbl"
taxonomy = "alvarez2017"
schema_bib = "bib_references"
x = syntax

## S4 method

new_taxonomy <- function(conn, x, taxonomy, schema, schema_bib, 
    comment, description, bibtexkey, ...) {
  # Retrieve existing schemas
  sch_names <- unlist(dbGetQuery(conn, paste("select schema_name",
              "from information_schema.schemata")))
  if(!schema %in% sch_names) {
    message(paste0("Schema '", schema, "' will be created."))
    dbSendQuery(conn, paste0("create schema \"", schema, "\""))
    if(missing(comment))
      comment <- "Schema created by ''vegtableDB::new_taxonomy()''"
    dbSendQuery(conn, paste0("comment on schema \"", schema, "\" ",
            "is '", comment, "'"))
    for(i in 1:length(tax_sql)) {
      query <- gsub("<schema_name>", paste0("\"", schema, "\""), tax_sql[i],
          fixed = TRUE)
      query <- gsub("<schema_bib_references>", paste0("\"", schema_bib, "\""),
          query, fixed = TRUE)
      dbSendQuery(conn, paste0(query, collapse = ""))
    }
  }
  # Check existence of taxonomy
  tax_names <- unlist(dbGetQuery(conn, paste("select taxonomy",
              paste0("from \"", schema, "\".taxonomies"))))
  if(taxonomy %in% tax_names)
    stop(paste0("The taxonomy '", taxonomy, "' is already in the database."))
  # Insert new taxonomy
  if(missing(description)) description <- NA
  if(missing(bibtexkey)) bibtexkey <- NA
  dbWriteTable(conn, c(schema, "taxonomies"), data.frame(
          taxonomy = taxonomy,
          description = description,
          bibtexkey = bibtexkey), row.name = FALSE, append = TRUE)
  # TODO: write a names translator for taxlist to DB
  # TODO: Append the tables (eventually new colums are required in DB)
}
