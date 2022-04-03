#' @name db2taxlist
#'
#' @title Import relational databases into taxlist objects
#'
#' @description
#' Ad-hoc function for importing Postgres tables into objects of class
#' [taxlist-class].
#'
#' This function has been modified to a new version of the database.
#'
#' @param conn A database connection provided by [dbConnect()].
#' @param taxonomy Character value with the name of the taxonomy in the
#'     database.
#' @param concepts A vector with taxon concept IDs to be included in the output.
#'     If not provided the whole taxonomy will be imported. IDs belonging to a
#'     different taxonomy will cause an error message.
#' @param schema Character value indicating the name of the schema containing
#'     taxonomic information within the database.
#' @param keep_parents A logical value indicating whether parents of queried
#'     concepts should be included in the output or not. It works only if an
#'     argument is provided for the parameter `'concepts'`.
#' @param keep_children A logical value indicating whether children of queried
#'     concepts should be included in the output or not. It works only if an
#'     argument is provided for the parameter `'concepts'`.
#' @param subset_levels Logical value indicating whether taxonomic ranks should
#'     be restricted to the used ones or all ranks available in the database.
#' @param as_list Logical value indicating whether the output should be a list
#'     or a [taxlist-class] object.
#' @param ... Further arguments passed among methods (not used here).
#'
#' @rdname db2taxlist
#'
#' @export
db2taxlist <- function(conn, ...) {
  UseMethod("db2taxlist", conn)
}

#' @rdname db2taxlist
#' @aliases db2taxlist,PostgreSQLConnection-method
#' @export
db2taxlist.PostgreSQLConnection <- function(conn,
                                            taxonomy,
                                            concepts,
                                            schema = "plant_taxonomy",
                                            subset_levels = TRUE,
                                            keep_parents = FALSE,
                                            keep_children = FALSE,
                                            as_list = FALSE, ...) {
  species_obj <- list()
  # Import catalog
  message("Check conditions ... ", appendLF = FALSE)
  db_names <- unique(unlist(dbGetQuery(
    conn,
    paste(
      "select top_view",
      paste0("from \"", schema, "\".taxon_concepts")
    )
  )))
  if (!taxonomy %in% db_names) {
    message("\n\n")
    stop("The requested taxonomic list is not in the connected database.")
  }
  # Import taxon concepts
  message("OK\nImporting taxon concepts ... ", appendLF = FALSE)
  if (missing(concepts)) {
    Query <- paste(
      paste0(
        "select ",
        "taxon_concept_id \"TaxonConceptID\",",
        "parent_id \"Parent\",",
        "rank \"Level\",",
        "view_key"
      ),
      paste0("from \"", schema, "\".taxon_concepts"),
      paste0("where top_view = '", taxonomy, "'")
    )
  } else {
    Query <- paste(
      "select taxon_concept_id,top_view taxonomy",
      paste0("from \"", schema, "\".taxon_concepts"),
      paste0(
        "where taxon_concept_id in (", paste0(concepts, collapse = ","),
        ")"
      )
    )
    check_concepts <- dbGetQuery(conn, Query)
    check_concepts <- check_concepts[check_concepts$taxonomy != taxonomy, ]
    if (nrow(check_concepts) > 0) {
      message("\n\n")
      stop(
        paste0(
          "Following queried concepts are in a different taxonomy ",
          "as the requested one:\n"
        ),
        paste0(check_concepts$taxon_concept_id, collapse = ", ")
      )
    }
    if (!missing(concepts) & keep_children) {
      repeat {
        Query <- paste(
          "select taxon_concept_id",
          paste0("from \"", schema, "\".taxon_concepts"),
          paste0(
            "where parent_id in (", paste0(concepts, collapse = ","),
            ")"
          )
        )
        add_concepts <- unlist(dbGetQuery(conn, Query))
        if (length(add_concepts) == 0) {
          break
        }
        concepts <- unique(c(concepts, add_concepts))
      }
    }
    Query <- paste(
      paste0(
        "select ",
        "taxon_concept_id \"TaxonConceptID\",",
        "parent_id \"Parent\",",
        "rank \"Level\",",
        "view_key"
      ),
      paste0("from \"", schema, "\".taxon_concepts"),
      paste0(
        "where taxon_concept_id in (", paste0(concepts, collapse = ","),
        ")"
      )
    )
  }
  species_obj$taxonRelations <- dbGetQuery(conn, Query)
  if (!missing(concepts) & keep_parents) {
    repeat {
      if (with(species_obj$taxonRelations, all(Parent[!is.na(Parent)] %in%
                  TaxonConceptID))) {
        break
      }
      add_concepts <- with(
        species_obj$taxonRelations,
        Parent[!Parent %in% TaxonConceptID]
      )
      Query <- paste(
        paste0(
          "select ",
          "taxon_concept_id \"TaxonConceptID\",",
          "parent_id \"Parent\",",
          "rank \"Level\",",
          "view_key"
        ),
        paste0("from \"", schema, "\".taxon_concepts"),
        paste0(
          "where taxon_concept_id in (", paste0(add_concepts,
            collapse = ","
          ),
          ")"
        )
      )
      species_obj$taxonRelations <- do.call(
        rbind,
        list(species_obj$taxonRelations, dbGetQuery(conn, Query))
      )
    }
  }
  # delete missing parents
  species_obj$taxonRelations$Parent <- with(species_obj$taxonRelations, {
    Parent[!Parent %in% TaxonConceptID] <- NA
    Parent
  })
  # Link names and concepts
  Query <- paste(
    "select",
    "taxon_usage_id \"TaxonUsageID\",",
    "taxon_concept_id \"TaxonConceptID\",",
    "name_status \"NameStatus\"",
    paste0("from \"", schema, "\".names2concepts"),
    paste0(
      "where taxon_concept_id in (",
      paste0(species_obj$taxonRelations$TaxonConceptID, collapse = ","),
      ")"
    )
  )
  concepts <- dbGetQuery(conn, Query)
  # Import taxon names
  message("OK\nImporting taxon names ... ", appendLF = FALSE)
  Query <- paste(
    "select",
    "taxon_usage_id \"TaxonUsageID\",",
    "usage_name \"TaxonName\",",
    "author_name \"AuthorName\"",
    paste0("from \"", schema, "\".taxon_names"),
    paste0(
      "where taxon_usage_id in (",
      paste0(concepts$TaxonUsageID, collapse = ","),
      ")"
    )
  )
  species_obj$taxonNames <- dbGetQuery(conn, Query)
  # Link names and concepts
  species_obj$taxonNames$TaxonConceptID <-
    concepts$TaxonConceptID[match(
      species_obj$taxonNames$TaxonUsageID,
      concepts$TaxonUsageID
    )]
  # Add status (accepted names)
  species_obj$taxonRelations$AcceptedName <-
    with(
      concepts[concepts$NameStatus == "accepted", ],
      TaxonUsageID[
        match(
          species_obj$taxonRelations$TaxonConceptID,
          TaxonConceptID
        )
      ]
    )
  species_obj$taxonRelations$Basionym <-
    with(
      concepts[concepts$NameStatus == "basionym", ],
      TaxonUsageID[
        match(
          species_obj$taxonRelations$TaxonConceptID,
          TaxonConceptID
        )
      ]
    )
  # Retrieve levels
  Query <- paste(
    "select",
    "rank \"Level\",",
    "rank_idx rank",
    paste0("from \"", schema, "\".taxon_levels")
  )
  tax_levels <- dbGetQuery(conn, Query)
  if (subset_levels) {
    tax_levels <- tax_levels[tax_levels$Level %in%
      species_obj$taxonRelations$Level, ]
  }
  tax_levels <- tax_levels[order(tax_levels$rank), ]
  species_obj$taxonRelations$Level <- factor(
    species_obj$taxonRelations$Level,
    tax_levels$Level
  )
  # Retrieve taxon traits
  Query <- paste(
    "select *",
    paste0("from \"", schema, "\".taxon_attributes"),
    paste0(
      "where taxon_concept_id in (",
      paste0(species_obj$taxonRelations$TaxonConceptID, collapse = ","),
      ")"
    )
  )
  species_obj$taxonTraits <- dbGetQuery(conn, Query)
  colnames(species_obj$taxonTraits) <-
    replace_x(colnames(species_obj$taxonTraits),
      old = "taxon_concept_id", new = "TaxonConceptID"
    )
  if (nrow(species_obj$taxonTraits) > 0) {
    species_obj$taxonTraits <- with(
      species_obj,
      taxonTraits[, apply(taxonTraits, 2, function(x) !all(is.na(x)))]
    )
  }
  # Import taxon views
  message("OK\nImporting taxon views ... ", appendLF = FALSE)
  # TODO: Next command may need more arguments to be set
  species_obj$taxonViews <- read_pg(conn,
    name = "bib_references",
    main_table = "main_table"
  )
  species_obj$taxonViews <- with(species_obj, {
    taxonViews <- taxonViews[taxonViews$bibtexkey %in%
      taxonRelations$view_key, ]
    taxonViews <- taxonViews[, apply(
      taxonViews, 2,
      function(x) !all(is.na(x))
    )]
    taxonViews
  })
  # Replace idx for taxon views
  species_obj$taxonViews$ViewID <- seq_along(species_obj$taxonViews[, 1])
  species_obj$taxonRelations$ViewID <- with(
    species_obj,
    taxonViews[
      match(taxonRelations$view_key, taxonViews$bibtexkey),
      "ViewID"
    ]
  )
  # Delete column view_key from output
  species_obj$taxonRelations <- with(species_obj, taxonRelations[
    ,
    colnames(taxonRelations) != "view_key"
  ])
  # Set ViewID at the beginning of table
  species_obj$taxonViews <- with(species_obj, taxonViews[
    ,
    c("ViewID", colnames(taxonViews)[colnames(taxonViews) !=
      "ViewID"])
  ])
  message("OK\nDONE!\n")
  if (as_list) {
    invisible(species_obj)
  } else {
    species_obj <- with(
      species_obj,
      new("taxlist",
        taxonNames = clean_strings(taxonNames),
        taxonRelations = clean_strings(taxonRelations),
        ## TODO: clean_strings to other objects
        ## taxonViews=clean_strings(taxonViews),
        taxonViews = taxonViews,
        taxonTraits = clean_strings(taxonTraits)
      )
    )
    return(species_obj)
  }
}
