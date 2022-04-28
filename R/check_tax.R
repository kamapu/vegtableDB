#' @name check_tax
#'
#' @title Additional tests of consistency for taxonomic lists
#'
#' @description
#' Some restrictions cannot be implemented by constraints into the database
#' but may affect the validity of information and exported objects.
#' This function carry out some additional checks to ensure consistent
#' taxonomic data.
#'
#' @param conn Connection to the database as [PostgreSQLConnection-class].
#' @param schema Character value indicating the name of the schema containing
#'     taxonomic information within the database.
#' @param ... Further arguments passed among methods.
#'
#' @rdname check_tax
#'
#' @export
check_tax <- function(conn, ...) {
  UseMethod("check_tax", conn)
}

#' @rdname check_tax
#' @aliases check_tax,PostgreSQLConnection-method
#' @method check_tax PostgreSQLConnection
#' @export
check_tax.PostgreSQLConnection <- function(conn, schema = "plant_taxonomy",
                                           ...) {
  # no more than one accepted name per concept
  message("Chech for multiple accepted names ... ", appendLF = FALSE)
  TAX <- dbGetQuery(conn, paste(
    "select *",
    paste0("from ", schema, ".names2concepts")
  ))
  Err <- TAX[TAX$name_status == "accepted", c(
    "taxon_usage_id",
    "taxon_concept_id"
  )]
  Err <- Err[duplicated(Err), ]
  if (nrow(Err > 0)) {
    cat("\n")
    stop(paste0(
      "Following taxon concepts have more than one ",
      "accepted name:\n",
      paste0(unique(Err$taxon_concept_id), collapse = ", ")
    ))
  }
  # no accepted name in a concept at all
  message("OK\nChech for missing accepted names ... ", appendLF = FALSE)
  Err <- with(TAX, {
    concepts <- taxon_concept_id[name_status == "accepted"]
    concepts <- unique(taxon_concept_id[!taxon_concept_id %in% concepts])
    concepts
  })
  if (length(Err) > 0) {
    cat("\n")
    stop(paste0(
      "Following taxon concepts have no accepted name:\n",
      paste0(unique(Err), collapse = ", ")
    ))
  }
  # multiple usage of a name within a taxonomy
  message("OK\nChech multiple uses of a name within a taxonomy ... ",
    appendLF = FALSE
  )
  TAX <- merge(TAX, dbGetQuery(conn, paste(
    "select taxon_concept_id,top_view",
    paste0("from ", schema, ".taxon_concepts"),
    paste0("where taxon_concept_id in (", paste0(TAX$taxon_concept_id,
      collapse = ","
    ), ")")
  )))
  Err <- TAX[duplicated(TAX[, c("taxon_usage_id", "top_view")]), ]
  if (nrow(Err > 0)) {
    cat("\n")
    stop(paste0(
      "Following taxon usage names appear more than once ",
      "between the same taxonomy:\n",
      paste0(unique(Err$taxon_usage_id), collapse = ", ")
    ))
  }
  # I everything OK
  message("OK\nDONE!")
}
