#' @name split_spec
#'
#' @title Split specimens
#'
#' @description
#' Single collections may be split into multiple specimens, either stored in the
#' same herbarium or distributed to different locations.
#' This function duplicates specimens belonging to the same collection.
#'
#' @param db Connection to the database as [PostgreSQLConnection-class].
#' @param spec_id Integer value indicating the ID of the specimen that will be
#'     split.
#' @param add Integer value indicating the number of duplicates that need to be
#'     created.
#' @param ... Further arguments. Parameters added here will be used to set
#'     values in the specimens table that are specific for duplicates. Such
#'     values will be recycled in the case of multiple duplicates.
#'
#' @author Miguel Alvarez \email{kamapu@@posteo.com}
#'
#' @rdname split_spec
#'
#' @export
split_spec <- function(db, ...) {
  UseMethod("split_spec", db)
}

#' @rdname split_spec
#' @aliases split_spec,PostgreSQLConnection-method
#' @method split_spec PostgreSQLConnection
#' @export
split_spec.PostgreSQLConnection <- function(db, spec_id, add = 1, ...) {
  # Check existing specimen
  if (length(spec_id) > 1) {
    warning("Only the first value of 'spec_id' will be split.")
  }
  query <- paste(
    "select *", "from specimens.specimens", "where spec_id =",
    spec_id[1]
  )
  Spec <- dbGetQuery(db, query)
  if (nrow(Spec) == 0) {
    stop("Specimen referred in 'spec_id' does not exist in database.")
  }
  DF <- list()
  for (i in 1:add) {
    DF[[i]] <- Spec
  }
  DF <- do.call(rbind, DF)
  new_cols <- list(...)
  for (i in names(new_cols)[names(new_cols) != "coll_nr"]) {
    DF[[i]] <- rep_len(new_cols[[i]], add)
  }
  DF <- DF[, names(DF) != "spec_id"]
  old_ids <- unlist(dbGetQuery(db, paste(
    "select spec_id",
    "from specimens.specimens"
  )))
  pgInsert(db, c("specimens", "specimens"), DF, partial.match = TRUE)
  new_ids <- unlist(dbGetQuery(db, paste(
    "select spec_id",
    "from specimens.specimens"
  )))
  new_ids <- new_ids[!new_ids %in% old_ids]
  # Copy determination history
  query <- paste(
    "select *", "from specimens.history", "where spec_id =",
    spec_id[1]
  )
  Det <- dbGetQuery(db, query)
  if (nrow(Det) > 0) {
    DF <- list()
    for (i in 1:length(new_ids)) {
      Det$spec_id <- new_ids[i]
      DF[[i]] <- Det
    }
    DF <- do.call(rbind, DF)
    DF <- DF[, colnames(DF) != "fid"]
    pgInsert(db, c("specimens", "history"), DF, partial.match = TRUE)
  }
  message("\nDONE!")
}
