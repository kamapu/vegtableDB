#' @name insert_synonym
#'
#' @title Insert synonyms in PostgreSQL taxonomic lists
#'
#' @description
#' Insert synonyms to existing taxa in a PostgreSQL version of [taxlist-class]
#' objects.
#'
#' This function is updating the tables `taxonNames` and `names2concepts` in
#' the PostgreSQL version of the database.
#'
#' @param conn A database connection provided by [dbConnect()].
#' @param taxonomy Character value with the name of the taxonomy in the
#'     database.
#' @param df A data frame with new names and related information. Three columns
#'     are mandatory, **usage_name** and **author_name** as character vectors,
#'     and **taxon_concept_id** as integer.
#' @param clean A logical value, whether strings in input 'df' should be cleaned
#'     or not (see [clean_strings()]).
#' @param ... Further arguments passed among methods.
#'
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#'
#' @rdname insert_synonym
#'
#' @export
insert_synonym <- function(conn, ...) {
  UseMethod("insert_synonym", conn)
}

#' @rdname insert_synonym
#'
#' @aliases insert_synonym,PostgreSQLConnection-method
#'
#' @export
insert_synonym.PostgreSQLConnection <- function(conn,
                                                taxonomy,
                                                df,
                                                clean = TRUE,
                                                ...) {
  if (any(!c("taxon_concept_id", "usage_name", "author_name") %in%
    colnames(df))) {
    stop(paste(
      "Columns 'taxon_concept_id', 'usage_name' and 'author_name'",
      "are mandatory in 'df'."
    ))
  }
  if (clean) {
    df <- clean_strings(df)
  }
  # Import catalog
  if (!taxonomy %in% db_catalog$taxonomy$db) {
    stop("The requested taxonomic list is not in the catalog.")
  }
  db_catalog <- db_catalog$taxonomy[
    db_catalog$taxonomy$db == taxonomy,
    c("slot", "name")
  ]
  taxon_names <- db_catalog[db_catalog$slot == "taxon_names", "name"]
  taxon_relations <- db_catalog[db_catalog$slot == "taxon_concepts", "name"]
  names2concepts <- db_catalog[db_catalog$slot == "names2concepts", "name"]
  Descr <- with(get_description(conn), paste(table_schema, table_name))
  # TODO: taxonomy.taxon_levels was not appearing in the decription
  ## db_catalog <- as.data.frame(rbind(
  ##         taxon_names, taxon_relations, taxon_traits,
  ##         taxon_levels, names2concepts, taxon_views
  ##     ))
  ## db_catalog <- paste(db_catalog[, 1], db_catalog[, 2])
  ## if(!all(db_catalog %in% Descr))
  ##   stop("Some tables from the catalog are not occurring in the database.")
  ## Cross-check
  # Required assets
  Query <- paste0(
    "SELECT taxon_usage_id, taxon_concept_id\n",
    "FROM \"", paste0(names2concepts, collapse = "\".\""), "\";\n"
  )
  n2c <- dbGetQuery(conn, Query)
  Query <- paste0(
    "SELECT *\n",
    "FROM \"", paste0(taxon_names, collapse = "\".\""), "\";\n"
  )
  t_names <- dbGetQuery(conn, Query)
  ## Cross-check
  # 1: Check duplicated combinations in 'df'
  if (any(duplicated(df[, c("usage_name", "author_name")]))) {
    stop("Duplicated combinations detected in 'df'.")
  }
  # 2: Check combinations already existing in database and add IDs
  if (any(with(df, paste(usage_name, author_name)) %in%
    with(t_names, paste(usage_name, author_name)))) {
    message(paste(
      "Some combinations in 'df' already exist in the database",
      "and may be recycled."
    ))
  }
  df$taxon_usage_id <- with(t_names, taxon_usage_id[
    match(
      paste(df$usage_name, df$author_name),
      paste(usage_name, author_name)
    )
  ])
  new_ids <- sum(is.na(df$taxon_usage_id))
  if (new_ids > 0) {
    df$taxon_usage_id[is.na(df$taxon_usage_id)] <-
      max(t_names$taxon_usage_id) + 1:new_ids
  }
  # 3: Check that usage names are not already in use
  if (any(df$taxon_usage_id %in% n2c$taxon_usage_id)) {
    stop("Some usage names are already in use in the database.")
  }
  # 4: Check existence of concepts in database
  if (!all(df$taxon_concept_id %in% n2c$taxon_concept_id)) {
    stop(paste(
      "Some entries for 'taxon_concept_id' in 'df' are not",
      "occurring in the database."
    ))
  }
  # Add status of name
  df$name_status <- "synonym"
  # 1: insert names
  pgInsert(
    conn, taxon_names,
    df[!df$taxon_usage_id %in% t_names$taxon_usage_id, colnames(df) %in%
      c("taxon_usage_id", "usage_name", "author_name")]
  )
  # 2: insert names2concepts
  pgInsert(conn, names2concepts, df[, colnames(df) %in%
    c("taxon_usage_id", "taxon_concept_id", "name_status")])
  message("DONE!")
}
