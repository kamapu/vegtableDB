#' @name query_name
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
#' @param case A logical value indicating whether the match should be case
#'     sensitive (`TRUE`) or not (`FALSE`, the default).
#' @param concepts A logical value indicating whether taxon concepts should be
#'     displayed (`TRUE`) or just the names (`FALSE`, the default).
#' @param accepted A logical value indicating whether the respective accepted
#'     names should be displayed (`TRUE`) or not (`FALSE`, the default). This
#'     works only if `'concepts = TRUE'`.
#' @param ... Further arguments passed among methods.
#'
#' @rdname query_name
#'
#' @export
query_name <- function(conn, ...) {
  UseMethod("query_name", conn)
}

#' @rdname query_name
#' @aliases query_name,PostgreSQLConnection-method
#' @method query_name PostgreSQLConnection
#' @export
query_name.PostgreSQLConnection <- function(conn, query, case = FALSE,
                                            concepts = FALSE, accepted = FALSE,
                                            ...) {
  if (length(query) > 1) {
    warning(paste(
      "Only the first element of 'query' will be matched",
      "with the database"
    ))
  }
  if (case) {
    query <- paste(
      "select *", "from plant_taxonomy.taxon_names",
      paste0("where usage_name ~ '", query[1], "'")
    )
  } else {
    query <- paste(
      "select *", "from plant_taxonomy.taxon_names",
      paste0("where usage_name ~* '", query[1], "'")
    )
  }
  Names <- dbGetQuery(conn, query)
  if (concepts) {
    query <- paste(
      "select taxon_usage_id,taxon_concept_id,name_status",
      "from plant_taxonomy.names2concepts",
      paste0("where taxon_usage_id in (", paste0(Names$taxon_usage_id,
        collapse = ","
      ), ")")
    )
    Names <- merge(Names, dbGetQuery(conn, query), all = TRUE, sort = FALSE)
    query <- paste(
      "select taxon_concept_id,rank,top_view taxonomy",
      "from plant_taxonomy.taxon_concepts",
      paste0("where taxon_concept_id in (", paste0(Names$taxon_concept_id,
        collapse = ","
      ), ")")
    )
    Names <- merge(Names, dbGetQuery(conn, query), all = TRUE, sort = FALSE)
    if (accepted) {
      query <- paste(
        "select taxon_concept_id,taxon_usage_id",
        "from plant_taxonomy.names2concepts",
        paste0("where taxon_concept_id in (", paste0(Names$taxon_concept_id,
          collapse = ","
        ), ")"),
        "and name_status = 'accepted'"
      )
      Acc <- dbGetQuery(conn, query)
      query <- paste(
        paste0(
          "select taxon_usage_id,usage_name accepted_name,",
          "author_name accepted_author"
        ),
        "from plant_taxonomy.taxon_names",
        paste0("where taxon_usage_id in (", paste0(Acc$taxon_usage_id,
          collapse = ","
        ), ")")
      )
      Acc <- merge(Acc, dbGetQuery(conn, query))
      colnames(Acc)[colnames(Acc) == "taxon_usage_id"] <- "accepted_usage_id"
      Names <- merge(Names, Acc, all = TRUE, sort = FALSE)
    }
  }
  return(Names)
}
