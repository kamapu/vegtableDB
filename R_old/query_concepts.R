#' @name query_concepts
#'
#' @title Query taxon usage names
#'
#' @description
#' A quick display of stored taxon names by matching them with a character
#' string.
#'
#' This function can also provide details about the associated taxon concpets,
#' taxonomies and accepted names for each taxonomy.
#'
#' @param conn Connection to the database as [PostgreSQLConnection-class].
#' @param query A character value that will be matched with the names stored in
#'     the database.
#' @param schema Character value indicating the name of the schema containing
#'     taxonomic information within the database.
#' @param case A logical value indicating whether the match should be case
#'     sensitive (`TRUE`) or not (`FALSE`, the default).
#' @param concepts A logical value indicating whether taxon concepts should be
#'     displayed (`TRUE`) or just the names (`FALSE`, the default).
#' @param accepted A logical value indicating whether the respective accepted
#'     names should be displayed (`TRUE`) or not (`FALSE`, the default). This
#'     works only if `'concepts = TRUE'`.
#' @param ... Further arguments passed among methods.
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
query_concepts.PostgreSQLConnection <- function(conn, query, c) {

}
