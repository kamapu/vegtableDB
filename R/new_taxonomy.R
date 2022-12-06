#' @name new_taxonomy
#' @rdname new_taxonomy
#'
#' @title Create new database from a taxlist object
#'
#' @description
#' Create a new schema in a connected database containing a taxonomic list from
#' a taxlist object.
#'
#' @param conn A [PostgreSQLConnection-class] connecting to a database, where
#'     the new list will be stored.
#' @param obj A [taxlist-class] object containing the new database.
#' @param taxonomy A character value indicating the name (ID) of the new
#'     taxonomy in the database.
#' @param schema A character value with the name of the new schema containing
#'     the taxonomic list.
#' @param schema_refs A character value with the name of the new schema
#'     containing the references used as taxon views. It is recommended to have
#'     a separated schema for the references, especially if the views are
#'     formated as a BibTeX database ([lib_df-class] object).
#' @param ... Further arguments passed among methods (not in use).
#'
#' @exportMethod new_taxonomy
setGeneric(
  "new_taxonomy",
  function(conn, obj, ...) {
    standardGeneric("new_taxonomy")
  }
)

#' @rdname new_taxonomy
#' @aliases new_taxonomy,PostgreSQLConnection,taxlist-method
setMethod("new_taxonomy", signature(
  conn = "PostgreSQLConnection",
  obj = "taxlist"
), function(conn, obj, taxonomy, schema, schema_refs, ...) {
  # Add references
  bib2database(conn = conn, schema = schema_refs, bib = obj@taxonViews[
    ,
    names(obj@taxonViews) != "ViewID"
  ])
  # Create empty schema
  query <- paste0("create schema if not exists \"", schema, "\"")
  dbSendQuery(conn, query)
  # Create tables
  query <- gsub("<schema>", schema, new_taxonomy_sql)
  query <- gsub("<schema_bib_references>", schema_refs, query)
  dbSendQuery(conn, query)
  # Add taxonomy
  dbWriteTable(conn, c(schema, "taxonomies"), data.frame(taxonomy = taxonomy),
    append = TRUE, row.names = FALSE
  )
  # Add taxon names
  # TODO: consider adding new columns
  tn <- obj@taxonNames
  tn$taxon_usage_id <- seq_along(obj@taxonNames$TaxonUsageID)
  colnames(tn) <- replace_x(colnames(tn),
    old = c("TaxonName", "AuthorName"),
    new = c("usage_name", "author_name")
  )
  tn_col_names <- unlist(dbGetQuery(conn, paste(
    "select column_name",
    "from information_schema.columns",
    paste0("where table_schema = '", schema, "'"),
    "and table_name = 'taxon_names'"
  )))
  dbWriteTable(conn, c(schema, "taxon_names"),
    tn[, colnames(tn) %in% tn_col_names],
    append = TRUE, row.names = FALSE
  )
  # Add taxon levels
  dbWriteTable(conn, c(schema, "taxon_levels"),
    data.frame(rank = levels(obj), rank_idx = seq_along(levels(obj))),
    append = TRUE, row.names = FALSE
  )
  # Add taxon concepts
  tc <- obj@taxonRelations
  tc$taxon_concept_id <- seq_along(tc$TaxonConceptID)
  tc$parent_id <- tc$taxon_concept_id[match(tc$Parent, tc$TaxonConceptID)]
  tc$rank <- as.character(tc$Level)
  tc$view_key <- obj@taxonViews$bibtexkey[match(
    tc$ViewID,
    obj@taxonViews$ViewID
  )]
  tc$top_view <- taxonomy
  tc_col_names <- unlist(dbGetQuery(conn, paste(
    "select column_name",
    "from information_schema.columns",
    paste0("where table_schema = '", schema, "'"),
    "and table_name = 'taxon_concepts'"
  )))
  dbWriteTable(conn, c(schema, "taxon_concepts"),
    tc[, colnames(tc) %in% tc_col_names],
    append = TRUE, row.names = FALSE
  )
  # Add names2concepts
  tn$taxon_concept_id <- tc$taxon_concept_id[match(
    tn$TaxonConceptID,
    tc$TaxonConceptID
  )]
  tn$name_status <- c("accepted", "synonym")[match(tn$TaxonUsageID %in%
    obj@taxonRelations$AcceptedName, c(TRUE, FALSE))]
  n2c_col_names <- unlist(dbGetQuery(conn, paste(
    "select column_name",
    "from information_schema.columns",
    paste0("where table_schema = '", schema, "'"),
    "and table_name = 'names2concepts'"
  )))
  dbWriteTable(conn, c(schema, "names2concepts"),
    tn[, colnames(tn) %in% n2c_col_names],
    append = TRUE, row.names = FALSE
  )
  # TODO: Add attributes
  message("DONE!")
})
