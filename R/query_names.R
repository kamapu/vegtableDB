#' @name query_names
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
#'     sensitive (`TRUE`) or not (`FALSE`, the default). I does not apply if
#'     `'exact = TRUE'`.
#' @param exact A logical value indicating whether the exact string is queried.
#' @param concepts A logical value indicating whether taxon concepts should be
#'     displayed (`TRUE`) or just the names (`FALSE`, the default).
#' @param accepted A logical value indicating whether the respective accepted
#'     names should be displayed (`TRUE`) or not (`FALSE`, the default). This
#'     works only if `'concepts = TRUE'`.
#' @param ... Further arguments passed among methods.
#'
#' @rdname query_names
#'
#' @export
query_names <- function(conn, ...) {
  UseMethod("query_names", conn)
}

#' @rdname query_names
#' @aliases query_names,PostgreSQLConnection-method
#' @method query_names PostgreSQLConnection
#' @export
query_names.PostgreSQLConnection <- function(conn, query,
                                             schema = "plant_taxonomy",
                                             case = FALSE, exact = FALSE,
                                             concepts = FALSE, accepted = FALSE,
                                             ...) {
  if (length(query) > 1) {
    warning(paste(
      "Only the first element of 'query' will be matched",
      "with the database"
    ))
  }
  if (exact) {
    query <- paste(
      "select *",
      paste0("from ", schema, ".taxon_names"),
      paste0("where usage_name = '", query[1], "'")
    )
  } else {
    if (case) {
      query <- paste(
        "select *",
        paste0("from ", schema, ".taxon_names"),
        paste0("where usage_name ~ '", query[1], "'")
      )
    } else {
      query <- paste(
        "select *",
        paste0("from ", schema, ".taxon_names"),
        paste0("where usage_name ~* '", query[1], "'")
      )
    }
  }
  Names <- dbGetQuery(conn, query)
  if (concepts) {
    query <- paste(
      "select taxon_usage_id,taxon_concept_id,name_status",
      paste0("from ", schema, ".names2concepts"),
      paste0("where taxon_usage_id in (", paste0(Names$taxon_usage_id,
        collapse = ","
      ), ")")
    )
    Names <- merge(Names, dbGetQuery(conn, query), all = TRUE, sort = FALSE)
    query <- paste(
      "select taxon_concept_id,rank,top_view taxonomy",
      paste0("from ", schema, ".taxon_concepts"),
      paste0("where taxon_concept_id in (", paste0(Names$taxon_concept_id,
        collapse = ","
      ), ")")
    )
    Names <- merge(Names, dbGetQuery(conn, query), all = TRUE, sort = FALSE)
    if (accepted) {
      query <- paste(
        "select taxon_concept_id,taxon_usage_id",
        paste0("from ", schema, ".names2concepts"),
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
        paste0("from ", schema, ".taxon_names"),
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
