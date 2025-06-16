#' @name merge_names
#'
#' @title Replace names across taxonomies
#'
#' @description
#' Correcting or completing names within a taxonomic list may cause
#' duplicated names in a common list of names. This situation is not allowed
#' by the used Postgres model. Thus, you may requery to replace occurrences of a
#' name with existing entries.
#'
#' The raplacement need to consider potential issues caused by inserting the new
#' name and do updates in multiple tables.
#'
#' @param conn Connection to the database as [PostgreSQLConnection-class].
#' @param usage_id A vector of Identifiers for the name. The first value will be
#'     used as the new name and any other will be replaced by the first.
#'     Note that at the end of this process all names but the first will be
#'     deleted.
#' @param schema A character value indicating the name of the schema containing
#'     taxonomies in the database.
#' @param relations Either a data frame with the columns **schema**, **table**
#'     and **column** or a list of character vectors including the name of
#'     schema, table, and column, where the names need to be changed.
#' @param eval A logical value, whether the function will execute the resulting
#'     sql or not.
#' @param ... Further arguments passed among methods.
#'
#' @return
#' An invisible [divDB::sql-class] object.
#'
#' @rdname merge_names
#'
#' @export
merge_names <- function(conn, ...) {
  UseMethod("merge_names", conn)
}

#' @rdname merge_names
#' @aliases merge_names,PostgreSQLConnection-method
#' @method merge_names PostgreSQLConnection
#' @export
merge_names.PostgreSQLConnection <- function(
    conn, usage_id,
    schema = "plant_taxonomy", relations = NULL, eval = TRUE, ...) {
  # Check replacement on taxonomy
  q1 <- paste(
    "select t1.taxon_usage_id,t2.taxon_concept_id,t2.top_view\n",
    paste0("from \"", schema, "\".names2concepts t1\n"),
    paste0("inner join \"", schema, "\".taxon_concepts t2"),
    "on t1.taxon_concept_id = t2.taxon_concept_id\n",
    paste0(
      "where t1.taxon_usage_id in (",
      paste0(do_character(usage_id), collapse = ","), ")"
    )
  )
  n_taxonomies <- dbGetQuery(conn, as(q1, "sql"))
  d_taxonomies <- n_taxonomies$top_view[duplicated(n_taxonomies$top_view)]
  if (length(d_taxonomies)) {
    stop(paste0(
      "Merging names to '", usage_id[1],
      "' will cause duplicated entries in following taxonomies: '",
      paste0(unique(d_taxonomies), collapse = "', '"), "'."
    ))
  }
  query <- character(0)
  # As character
  usage_id <- do_character(usage_id)
  # TODO: Replace in relations
  # Replace  in names2concepts
  query <- c(query, paste(
    paste0("update \"", schema, "\".names2concepts"),
    paste0("set taxon_usage_id = ", usage_id[1]),
    paste0(
      "where taxon_usage_id in (",
      paste0(usage_id[-1], collapse = ","), ")"
    )
  ))
  # Delete old names
  query <- c(query, paste(
    paste0("delete from \"", schema, "\".taxon_names"),
    paste0("where taxon_usage_id in (", paste0(usage_id[-1],
      collapse = ","
    ), ")")
  ))
  query <- as(query, "sql")
  if (eval) {
    dbSendQuery(conn, query)
    message("DONE!")
  }
  invisible(query)
}
