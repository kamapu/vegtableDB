#' @name new_coll
#'
#' @title Insert new collections to database.
#'
#' @description
#' A straight forward method to insert new collections in the specimens
#' database.
#' This will automatically create the respective specimens.
#'
#' @param db Connections to the database as [PostgreSQLConnection-class].
#' @param sf An object of class [sf][sf::sf] to be appended into the table
#'     **collections** (schema **specimens**).
#' @param bulk Either an integer or a character value indicating the ID or the
#'     name of the corresponding bulk, respectively. Note that you can insert
#'     new records to only one bulk at once. If you use a character value, the
#'     ID will be retrieved from the database (table **projects**, schema
#'     **specimens**). If the name is not in database, it will be considered as
#'     a new bulk and inserted accordingly.
#' @param ... Further arguments passed among methods.
#'
#' @author Miguel Alvarez \email{kamapu@@posteo.com}
#'
#' @rdname new_coll
#'
#' @exportMethod new_coll
setGeneric(
  "new_coll",
  function(db, sf, bulk, ...) {
    standardGeneric("new_coll")
  }
)

#' @rdname new_coll
#' @aliases new_coll,PostgreSQLConnection,data.frame,integer-method
setMethod(
  "new_coll", signature(
    db = "PostgreSQLConnection", sf = "sf",
    bulk = "integer"
  ),
  function(db, sf, bulk, ...) {
    # In case of a coll_nr column
    sf <- sf[, !names(sf) %in% c("coll_nr", "bulk", "spec_id")]
    # TODO: Define and check mandatory columns (excludes coll_nr)
    if (length(bulk) > 1) {
      warning("Only the first element of 'bulk' will be used.")
    }
    if (!"field_nr" %in% names(sf)) {
      message("Column 'field_nr' automatically added to 'sf'.")
      sf$field_nr <- 1:nrow(sf)
    }
    if (any(duplicated(sf$field_nr))) {
      stop("Duplicated values in column 'field_nr' in 'sf' are not allowed.")
    }
    sf$bulk <- bulk[1]
    query <- paste(
      "select bulk", "from specimens.projects",
      paste("where bulk =", bulk[1])
    )
    if (length(unlist(dbGetQuery(db, query))) < 1) {
      stop("The target 'bulk' does not exist in the database.")
    }
    sf <- as(sf, "Spatial")
    # Collect IDs and insert new entries
    old_ids <- unlist(dbGetQuery(db, paste(
      "select coll_nr",
      "from specimens.collections"
    )))
    pgInsert(db, c("specimens", "collections"), sf, "geom_point",
      partial.match = TRUE
    )
    sf <- sf@data
    new_ids <- dbGetQuery(db, paste(
      "select coll_nr,field_nr",
      "from specimens.collections",
      paste0("where coll_nr not in (", paste0(old_ids, collapse = ","), ")")
    ))
    sf$coll_nr <- with(new_ids, coll_nr[match(sf$field_nr, field_nr)])
    pgInsert(db, c("specimens", "specimens"), sf, partial.match = TRUE)
    message("\nDONE!")
  }
)

#' @rdname new_coll
#' @aliases new_coll,PostgreSQLConnection,data.frame,numeric-method
setMethod(
  "new_coll", signature(
    db = "PostgreSQLConnection", sf = "sf",
    bulk = "numeric"
  ),
  function(db, sf, bulk, ...) new_coll(db, sf, as.integer(bulk), ...)
)

#' @rdname new_coll
#' @aliases new_coll,PostgreSQLConnection,data.frame,character-method
setMethod(
  "new_coll", signature(
    db = "PostgreSQLConnection", sf = "sf",
    bulk = "character"
  ),
  function(db, sf, bulk, ...) {
    if (length(bulk) > 1) {
      warning("Only the first element of 'bulk' will be used.")
    }
    query <- paste(
      "select bulk", "from specimens.projects",
      paste0("where project_name = '", bulk[1], "'")
    )
    bulk_id <- unlist(dbGetQuery(db, query))
    if (length(bulk_id) == 0) {
      message(paste0(
        "A new bulk '", bulk[1],
        "' will be created in the project.\n"
      ))
      old_ids <- unlist(dbGetQuery(db, paste(
        "select bulk",
        "from specimens.projects"
      )))
      pgInsert(
        db, c("specimens", "projects"),
        data.frame(project_name = bulk[1])
      )
      new_ids <- unlist(dbGetQuery(db, paste(
        "select bulk",
        "from specimens.projects"
      )))
      bulk_id <- new_ids[!new_ids %in% old_ids]
    } else {
      message(paste0(
        "The collection will be appended to bulk '", bulk[1],
        "'.\n"
      ))
    }
    new_coll(db, sf, bulk_id, ...)
  }
)
