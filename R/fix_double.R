#' @name fix_double
#'
#' @title Combine names across data sets
#'
#' @description
#' When names inserted with no authors or with misspelled authors, duplicated
#' entries will be created, which may cause conflicts if one name is corrected
#' since the database does not allow duplicated combinations of names and
#' authors.
#'
#' In this routines one name (parameter `'old_id'`) will be replaced by another
#' alternative name (parameter `'new_id'`) in all usages and the first will be
#' finally deleted from the database.
#'
#' If the two alternatives are already used within the same taxonomy (another
#' constraint of the database), this function retrieves an error message.
#'
#' @param conn Connection to the database as [PostgreSQLConnection-class].
#' @param new_id Vector with IDs of the correct name.
#' @param old_id Vector with IDs of the names that have to be replaced and
#'     deleted.
#' @param ... Further arguments passed among methods.
#'
#' @rdname fix_double
#'
#' @export
fix_double <- function(conn, ...) {
  UseMethod("fix_double", conn)
}

#' @rdname fix_double
#' @aliases fix_double,PostgreSQLConnection-method
#' @method fix_double PostgreSQLConnection
#' @export
fix_double.PostgreSQLConnection <- function(conn, new_id, old_id, ...) {
  if (length(new_id) != length(old_id)) {
    stop("Arguments for 'new_id' and 'old_id' have to be of the same length.")
  }
  TAX <- dbGetQuery(conn, paste(
    "select *",
    "from plant_taxonomy.names2concepts",
    paste0("where taxon_usage_id in (", paste0(c(new_id, old_id),
      collapse = ","
    ), ")")
  ))
  # Test for generated duplicates
  TAX$new_usage <- replace_x(TAX$taxon_usage_id, old = old_id, new = new_id)
  Err <- TAX[duplicated(TAX[, c("new_usage", "taxon_concept_id")]), ]
  if (nrow(Err) > 0) {
    stop(paste0(
      "Replacing following usage ID's will cause duplicated ",
      "names in concepts:\n",
      paste0(TAX$taxon_usage_id, collapse = ","), "."
    ))
  }
  for (i in 1:length(new_id)) {
    query <- paste(
      "update plant_taxonomy.names2concepts",
      "set taxon_usage_id =", new_id[i],
      "where taxon_usage_id =", old_id[i]
    )
    dbSendQuery(conn, query)
    query <- paste(
      "delete from plant_taxonomy.taxon_names",
      "where taxon_usage_id =", old_id[i]
    )
    dbSendQuery(conn, query)
  }
  message("DONE!")
}
