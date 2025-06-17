#' @name query_concepts
#'
#' @title Query taxon concepts
#'
#' @description
#' Extract taxonomies by matching character strings.
#'
#' @param conn Connection to the database as
#'     [RPostgreSQL::PostgreSQLConnection-class].
#' @param query A character value that will be matched with the names stored in
#'     the database.
#' @param taxonomy Character value with the name of the taxonomy in the
#'     database.
#' @param schema Character value indicating the name of the schema containing
#'     taxonomic information within the database.
#' @param case A logical value indicating whether the match should be case
#'     sensitive (`TRUE`) or not (`FALSE`, the default).
#' @param exact A logical value indicating whether the exact string is queried.
#' @param ... Further arguments passed to [db2taxlist()].
#'
#' @return
#' A [taxlist::taxlist-class] object with the queried taxon concepts.
#'
#' @rdname query_concepts
#'
#' @export
query_concepts <- function(conn, ...) {
  UseMethod("query_concepts", conn)
}

#' @rdname query_concepts
#' @aliases query_concepts,PostgreSQLConnection-method
#' @method query_concepts PostgreSQLConnection
#' @export
query_concepts.PostgreSQLConnection <- function(conn, query, taxonomy,
                                                schema = "plant_taxonomy", case = FALSE, exact = FALSE, ...) {
  if (length(query) > 1) {
    warning(paste(
      "Only the first element of 'query' will be matched",
      "with the database"
    ))
  }
  if (exact) {
    query <- paste(
      "select taxon_usage_id",
      paste0("from ", schema, ".taxon_names"),
      paste0("where usage_name = '", query[1], "'")
    )
  } else {
    if (case) {
      query <- paste(
        "select taxon_usage_id",
        paste0("from ", schema, ".taxon_names"),
        paste0("where usage_name ~ '", query[1], "'")
      )
    } else {
      query <- paste(
        "select taxon_usage_id",
        paste0("from ", schema, ".taxon_names"),
        paste0("where usage_name ~* '", query[1], "'")
      )
    }
  }
  usages <- unlist(dbGetQuery(conn, query))
  query <- paste(
    "select taxon_concept_id",
    paste0("from ", schema, ".names2concepts"),
    paste0("where taxon_usage_id in (", paste0(usages, collapse = ","), ")")
  )
  concepts <- unlist(dbGetQuery(conn, query))
  # Filter taxonomy
  query <- paste(
    "select taxon_concept_id",
    paste0("from ", schema, ".taxon_concepts"),
    paste0(
      "where taxon_concept_id in (", paste0(concepts, collapse = ","),
      ")"
    ),
    paste0("and top_view = '", taxonomy[1], "'")
  )
  concepts <- unlist(dbGetQuery(conn, query))
  db2taxlist(conn, taxonomy[1], concepts, schema, ...)
}
