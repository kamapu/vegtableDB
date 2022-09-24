#' @name read_spec
#'
#' @title Import Specimen Information.
#'
#' @description
#' Import function for herbarium specimens stored in own database.
#' Information is arranged into three tables and formatted as an object of class
#' [specimens-class].
#'
#' For convenience the function will display the list of updates and prompt a
#' yes/no/cancel question. Here you should check the printed names.
#'
#' @param db Connection to the database as [PostgreSQLConnection-class].
#' @param adm Connection to the database containing administrative units as
#'     [PostgreSQLConnection-class]. If not provided, no administrative units
#'     will be included in the output.
#' @param bulk Integer vector including the ID's of the requested bulks
#'     (campaigns or projects).
#' @param get_coords Logical values indicating whether formatted coordinates
#'     should be extracted from geometry or not.The default is
#'     `'get_coords = TRUE'` but it may cause an error if some cells in the
#'     geometry are empty.
#' @param ... Further arguments passed among methods (not yet used).
#'
#' @return An S3 object of class `specimens`.
#'
#' @author Miguel Alvarez \email{kamapu@@posteo.com}
#'
#' @rdname read_spec
#'
#' @export
read_spec <- function(db, ...) {
  UseMethod("read_spec", db)
}

#' @rdname read_spec
#' @aliases read_spec,PostgreSQLConnection-method
#' @method read_spec PostgreSQLConnection
#' @export
read_spec.PostgreSQLConnection <- function(db, adm, bulk, get_coords = TRUE,
                                           ...) {
  # Main table
  message("Importing main tables ... ", appendLF = FALSE)
  if (missing(bulk)) {
    Coll <- st_read(db, query = paste(
      "select *",
      "from specimens.collections;"
    ))
  } else {
    Coll <- st_read(db, query = paste(
      "select *",
      "from specimens.collections",
      paste0("where bulk in (", paste0(bulk, collapse = ","), ");")
    ))
  }
  # Add project name (campaign)
  if ("bulk" %in% colnames(Coll)) {
    query <- paste(
      "select *",
      "from specimens.projects",
      paste0("where bulk in (", paste0(unique(Coll$bulk[!is.na(Coll$bulk)]),
        collapse = ","
      ), ");")
    )
    Coll <- merge(Coll, dbGetQuery(db, query), all = TRUE, sort = FALSE)
  }
  # Import Specimens
  query <- paste(
    "select *", "from specimens.specimens",
    paste0(
      "where coll_nr in (", paste0(Coll$coll_nr, collapse = ","),
      ")"
    )
  )
  Spec <- dbGetQuery(db, query)
  # Extract determined vouchers
  message("OK\nImporting taxonomic information ... ", appendLF = FALSE)
  Det <- dbGetQuery(db, paste(
    "select *",
    "from specimens.history",
    paste0("where spec_id in (", paste0(Spec$spec_id,
      collapse = ","
    ), ")"),
    "order by det_date desc;"
  ))
  if (nrow(Det) > 0) {
    # Add names
    query <- paste(
      "select *", "from plant_taxonomy.names2concepts",
      paste0(
        "where tax_id in (", paste0(unique(Det$tax_id), collapse = ","),
        ")"
      )
    )
    tax_names <- dbGetQuery(db, query)
    query <- paste(
      "select taxon_usage_id,usage_name taxon_name,author_name taxon_author",
      "from plant_taxonomy.taxon_names",
      paste0(
        "where taxon_usage_id in (",
        paste0(tax_names$taxon_usage_id, collapse = ","), ")"
      )
    )
    tax_names <- merge(tax_names, dbGetQuery(db, query),
      all = TRUE,
      sort = FALSE
    )
    Det <- merge(Det, tax_names[, c("tax_id", "taxon_name", "taxon_author")],
      all = TRUE, sort = FALSE
    )
    # Get genus
    Det$genus <- dissect_name(Det$taxon_name, repaste = 1)

    # Get families
    Levels <- dbGetQuery(db, "select * from plant_taxonomy.taxon_levels")
    query <- paste(
      "select *", "from plant_taxonomy.taxon_concepts",
      paste0(
        "where taxon_concept_id in (",
        paste0(unique(tax_names$taxon_concept_id), collapse = ","), ")"
      )
    )
    TAX <- dbGetQuery(db, query)
    Steps <- with(Levels, {
      Min <- min(rank_idx[rank %in% unique(TAX$rank)])
      Max <- rank_idx[rank == "family"]
      rank[Min:Max]
    })
    # Convenience function to distribute taxa
    Distr <- function(table, id_name, id, new, rank) {
      for (i in 1:length(id)) {
        if (!is.na(new[i])) {
          table[
            table[, id_name] == id[i] & !is.na(table[, id_name]),
            rank[i]
          ] <- new[i]
        }
      }
      return(table)
    }
    for (i in Steps) {
      TAX[, i] <- NA
    }
    TAX <- Distr(
      TAX, "taxon_concept_id", TAX$taxon_concept_id,
      TAX$taxon_concept_id, TAX$rank
    )
    for (i in Steps[-length(Steps)]) {
      if (!all(is.na(TAX[, i]))) {
        query <- paste(
          paste0("select taxon_concept_id ", i, ",parent_id"),
          "from plant_taxonomy.taxon_concepts",
          paste0(
            "where taxon_concept_id in (",
            paste0(TAX[!is.na(TAX[, i]), i], collapse = ","), ")"
          )
        )
        Parent <- dbGetQuery(db, query)
        query <- paste(
          "select taxon_concept_id parent_id,rank parent_rank",
          "from plant_taxonomy.taxon_concepts",
          paste0(
            "where taxon_concept_id in (",
            paste0(Parent$parent_id[!is.na(Parent$parent_id)], collapse = ","),
            ")"
          )
        )
        Parent <- merge(Parent, dbGetQuery(db, query), all = TRUE, sort = FALSE)
        TAX <- Distr(TAX, i, Parent[, i], Parent$parent_id, Parent$parent_rank)
      }
    }
    query <- paste(
      "select tax_id,taxon_concept_id,taxon_usage_id",
      "from plant_taxonomy.names2concepts",
      paste0(
        "where taxon_concept_id in (",
        paste0(TAX$family[!is.na(TAX$family)], collapse = ","), ")"
      ),
      "and name_status = 'accepted'"
    )
    Families <- dbGetQuery(db, query)
    query <- paste(
      "select taxon_usage_id,usage_name",
      "from plant_taxonomy.taxon_names",
      paste0(
        "where taxon_usage_id in (",
        paste0(Families$taxon_usage_id, collapse = ","), ")"
      )
    )
    Families <- merge(Families, dbGetQuery(db, query))
    TAX$family_name <- with(Families, usage_name[match(
      TAX$family,
      taxon_concept_id
    )])
    query <- paste(
      "select tax_id,taxon_concept_id",
      "from plant_taxonomy.names2concepts",
      paste0("where tax_id in (", paste0(Det$tax_id, collapse = ","), ")")
    )
    Det <- merge(Det, dbGetQuery(db, query), all = TRUE, sort = FALSE)
    Det$family <- with(TAX, family_name[match(
      Det$taxon_concept_id,
      taxon_concept_id
    )])
  } else {
    Det$taxon_name <- character()
    Det$det_date <- as.Date(NULL)
  }
  # Coordinates for Bonn
  message("OK\nImporting geographic information ... ", appendLF = FALSE)
  if (get_coords) {
    n_digits <- 4
    Coords <- st_coordinates(Coll)
    c_suffix <- cbind(
      c("E", "W")[match(Coords[, 1] >= 0, c(TRUE, FALSE))],
      c("N", "S")[match(Coords[, 2] >= 0, c(TRUE, FALSE))]
    )
    Coll$coord_bonn <- paste(
      c_suffix[, 2], format(round(Coords[, 2],
        digits = n_digits
      ), nsmall = n_digits),
      c_suffix[, 1], format(round(Coords[, 1], digits = n_digits),
        nsmall = n_digits
      )
    )
  }
  # Get country codes
  Countries_map <- st_read(db, query = paste(
    "select *",
    "from environment.countries_map;"
  ))
  Coll$country <- Countries_map$adm0_a3[st_nearest_feature(
    Coll,
    Countries_map
  )]
  # Import GADM
  if (!missing(adm)) {
    gadm <- st_read(adm, query = paste(
      "select name_0,name_1,name_2,geom",
      "from gadm",
      paste0(
        "where gid_0 in ('",
        paste0(unique(Coll$country), collapse = "','"), "')"
      )
    ))
    for (i in c("name_0", "name_1", "name_2")) {
      Coll[[i]] <- gadm[[i]][st_nearest_feature(Coll, gadm)]
    }
  }
  message("DONE!")
  return(new("specimens",
    collections = Coll, specimens = Spec,
    history = Det
  ))
}
