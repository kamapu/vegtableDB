# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

library(vegtableDB)
## library(taxlist)
library(biblio)

# Import species list
spp <- readRDS("../../super-miguel/wfo/data/wfo_v1.7.rds")

# Import Bib and set as view
bibl <- read_bib("../../db-dumps/literatur_db/bib/MiguelReferences.bib")
bibl <- subset(bibl, bibtexkey == "WFOTeam2021")

spp@taxonRelations$ViewID <- 1
spp@taxonViews <- data.frame(ViewID = 1, bibl)
class(spp@taxonViews) <- c("lib_df", "data.frame")

# Select some species
list_spp <- with(spp@taxonRelations, sample(TaxonConceptID[Level == "species"],
        25, replace = FALSE))

spp2 <- subset(spp, TaxonConceptID %in% list_spp, slot = "taxonRelations",
    keep_children = TRUE, keep_parents = TRUE)
spp2

indented_list(spp2)

# Connect to database
conn <- connect_db("example_db", user = "miguel")

# Function ---------------------------------------------------------------------

library(RPostgreSQL)

schema = "plant_taxonomy"
schema_refs = "bib_references2"
obj = spp2
name = "wfo"

dbSendQuery(conn, paste("drop schema if exists", schema, "cascade"))
dbSendQuery(conn, paste("drop schema if exists", schema_refs, "cascade"))

# Import queries
load("R/sysdata.rda")

new_taxonomy <- function(conn, ...) {
  UseMethod("new_taxonomy", conn)
}

new_taxonomy.PostgreSQLConnection <- function(conn, obj, name, schema,
    schema_refs, ...) {
  # Add references
  bib2database(conn = conn, schema = schema_refs, bib = obj@taxonViews[ ,
          names(obj@taxonViews) != "ViewID"])
  # Create empty schema
  query <- paste0("create schema if not exists \"", schema, "\"")
  dbSendQuery(conn, query)
  # Create tables
  query <- gsub("<schema>", schema, new_taxonomy_sql)
  query <- gsub("<schema_bib_references>", schema_refs, query)
  dbSendQuery(conn, query)
  # Add taxonomy
  dbWriteTable(conn, c(schema, "taxonomies"), data.frame(taxonomy = name),
      append = TRUE, row.names = FALSE)
  # Add taxon names
  # TODO: consider adding new columns
  tn <- obj@taxonNames
  tn$taxon_usage_id <- seq_along(obj@taxonNames$TaxonUsageID)
  colnames(tn) <- replace_x(colnames(tn), old = c("TaxonName", "AuthorName"),
      new = c("usage_name", "author_name"))
  tn_col_names <- unlist(dbGetQuery(conn, paste(
              "select column_name",
              "from information_schema.columns",
              paste0("where table_schema = '", schema, "'"),
              "and table_name = 'taxon_names'")))
  dbWriteTable(conn, c(schema, "taxon_names"),
      tn[ , colnames(tn) %in% tn_col_names],
      append = TRUE, row.names = FALSE)
  # Add taxon levels
  dbWriteTable(conn, c(schema, "taxon_levels"),
      data.frame(rank = levels(obj), rank_idx = seq_along(levels(obj))),
      append = TRUE, row.names = FALSE)
  # Add taxon concepts
  tc <- obj@taxonRelations
  tc$taxon_concept_id <- seq_along(tc$TaxonConceptID)
  tc$parent_id <- tc$taxon_concept_id[match(tc$Parent, tc$TaxonConceptID)]
  tc$rank <- as.character(tc$Level)
  tc$view_key <- obj@taxonViews$bibtexkey[match(tc$ViewID,
          obj@taxonViews$ViewID)]
  tc$top_view <- name
  tc_col_names <- unlist(dbGetQuery(conn, paste(
              "select column_name",
              "from information_schema.columns",
              paste0("where table_schema = '", schema, "'"),
              "and table_name = 'taxon_concepts'")))
  dbWriteTable(conn, c(schema, "taxon_concepts"),
      tc[ , colnames(tc) %in% tc_col_names],
      append = TRUE, row.names = FALSE)
  # Add names2concepts
  tn$taxon_concept_id <- tc$taxon_concept_id[match(tn$TaxonConceptID,
          tc$TaxonConceptID)]
  tn$name_status <- c("accepted", "synonym")[match(tn$TaxonUsageID %in%
              obj@taxonRelations$AcceptedName, c(TRUE, FALSE))]
  n2c_col_names <- unlist(dbGetQuery(conn, paste(
              "select column_name",
              "from information_schema.columns",
              paste0("where table_schema = '", schema, "'"),
              "and table_name = 'names2concepts'")))
  dbWriteTable(conn, c(schema, "names2concepts"),
      tn[ , colnames(tn) %in% n2c_col_names],
      append = TRUE, row.names = FALSE)
  # TODO: Add attributes
  message("DONE!")
}

dbSendQuery(conn, paste("drop schema if exists", schema, "cascade"))
dbSendQuery(conn, paste("drop schema if exists", schema_refs, "cascade"))

new_taxonomy(conn, spp2, name = "wfo", schema = "plant_taxonomy",
    schema_refs = "bib_references2")

spp_new <- db2taxlist(conn, "wfo", schema_refs = "bib_references2")
spp_new

indented_list(spp_new)




T1 <- dbReadTable(conn, c("plant_taxonomy", "taxonomies"))

