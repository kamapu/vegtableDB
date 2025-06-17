#' @name append_taxonomy
#' @rdname append_taxonomy
#'
#' @title Append a new taxonomy in existing database
#'
#' @description
#' New taxonomies can be appended to the same database structure.
#'
#' @param conn A [RPostgreSQL::PostgreSQLConnection-class] connecting to a
#'     database, where the new list will be stored.
#' @param obj A [taxlist::taxlist-class] object containing the new taxonomy.
#' @param taxonomy A character value indicating the name (ID) of the new
#'     taxonomy in the database.
#' @param schema A character value with the name of the new schema containing
#'     the taxonomic list.
#' @param schema_refs A character value with the name of the new schema
#'     containing the references used as taxon views. It is recommended to have
#'     a separated schema for the references, especially if the views are
#'     formated as a BibTeX database ([biblio::lib_df-class] object).
#' @param ... Further arguments passed among methods (not in use).
#'
#' @exportMethod append_taxonomy
setGeneric("append_taxonomy", function(conn, obj, ...) {
  standardGeneric("append_taxonomy")
})

#' @rdname append_taxonomy
#' @aliases append_taxonomy,PostgreSQLConnection,taxlist-method
setMethod("append_taxonomy", signature(
  conn = "PostgreSQLConnection",
  obj = "taxlist"
), function(conn, obj, taxonomy, schema, schema_refs,
            ...) {
  # Check format of references
  if (!"lib_df" %in% class(obj@taxonViews)) {
    stop("Slot 'taxonViews' have to be formatted as 'lib_df'.")
  }
  # Do taxonomy already exists?
  db_taxonomies <- unlist(dbGetQuery(conn, paste(
    "select taxonomy",
    paste0("from \"", schema, "\".taxonomies")
  )))
  if (taxonomy %in% db_taxonomies) {
    stop(paste0(
      "The taxonomy '", taxonomy,
      "' is already in the database."
    ))
  }
  # Check for taxonomic ranks
  db_ranks <- unlist(dbGetQuery(conn, paste(
    "select rank",
    paste0("from \"", schema, "\".taxon_levels")
  )))
  if (length(db_ranks) == 0) {
    dbWriteTable(conn, c(schema, "taxon_levels"),
      data.frame(rank = levels(obj), rank_idx = seq_along(levels(obj))),
      append = TRUE, row.names = FALSE
    )
  } else {
    db_ranks <- levels(obj)[!levels(obj) %in% db_ranks]
    if (length(db_ranks) > 0) {
      stop(paste0(
        "Following ranks are missing in database: '",
        paste0(db_ranks, collapse = "', '"), "'."
      ))
    }
  }
  # Append references
  suppressMessages(suppressWarnings(update_data(conn,
    revision = obj@taxonViews, schema = schema_refs, add = TRUE
  )))
  # Write taxonomy
  dbWriteTable(conn, c(schema, "taxonomies"),
    data.frame(taxonomy = taxonomy),
    append = TRUE, row.names = FALSE
  )
  # Append taxon names
  tn <- obj@taxonNames
  colnames(tn) <- replace_x(colnames(tn),
    old = c("TaxonName", "AuthorName"),
    new = c("usage_name", "author_name")
  )
  insert_names(conn, tn, schema)
  # Insert taxon concepts
  tc <- obj@taxonRelations
  concept_id <- unlist(dbGetQuery(
    conn,
    paste(
      "select max(taxon_concept_id)",
      paste0("from \"", schema, "\".taxon_concepts")
    )
  ))
  if (is.na(concept_id)) concept_id <- 0
  tc$taxon_concept_id <- concept_id + seq_along(tc$TaxonConceptID)
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
  db_names <- dbGetQuery(conn, paste(
    "select taxon_usage_id,usage_name,author_name",
    paste0("from \"", schema, "\".taxon_names")
  ))
  tn$taxon_usage_id <- db_names$taxon_usage_id[match(
    with(tn, paste(usage_name, author_name)),
    with(db_names, paste(usage_name, author_name))
  )]
  tax_id <- unlist(dbGetQuery(
    conn,
    paste(
      "select max(tax_id)",
      paste0("from \"", schema, "\".names2concepts")
    )
  ))
  if (is.na(tax_id)) tax_id <- 0
  tn$tax_id <- tax_id + seq_along(tn$TaxonUsageID)
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
  # Add attributes
  if (nrow(obj@taxonTraits) > 0) {
    ta <- obj@taxonTraits
    ta$taxon_concept_id <- tc$taxon_concept_id[match(
      ta$taxon_concept_id,
      tc$TaxonConceptID
    )]
    ta_col_names <- unlist(dbGetQuery(conn, paste(
      "select column_name",
      "from information_schema.columns",
      paste0("where table_schema = '", schema, "'"),
      "and table_name = 'taxon_attributes'"
    )))
    dbWriteTable(conn, c(schema, "names2concepts"),
      ta[, colnames(ta) %in% ta_col_names],
      append = TRUE, row.names = FALSE
    )
  }
  # Finish it
  message("DONE!")
})
