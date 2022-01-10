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
#' @param subset_levels Logical value indicating whether taxonomic ranks should
#'     be restricted to the used ones or all ranks available in the database.
#' @param as_list Logical value indicating whether the output should be a list
#'     or a [taxlist-class] object.
#' @param ... Further arguments passed among methods. In the two wrappers the
#'     arguments are passed to `db2taxlist`.
#'
#' @rdname db2taxlist
#'
#' @export
db2taxlist <- function(conn, ...) {
  UseMethod("db2taxlist", conn)
}

#' @rdname db2taxlist
#' @export
db2taxlist.PostgreSQLConnection <- function(conn,
                                            taxonomy,
                                            subset_levels = TRUE,
                                            as_list = FALSE,
                                            ...) {
  species_obj <- list()
  # Import catalog
  message("Importing catalog ... ", appendLF = FALSE)
  if (!taxonomy %in% db_catalog$taxonomy$db) {
    stop("The requested taxonomic list is not in the catalog.")
  }
  db_catalog <- db_catalog$taxonomy[
    db_catalog$taxonomy$db == taxonomy,
    c("slot", "name")
  ]
  taxon_names <- db_catalog[db_catalog$slot == "taxon_names", "name"]
  taxon_relations <- db_catalog[db_catalog$slot == "taxon_concepts", "name"]
  taxon_traits <- db_catalog[db_catalog$slot == "taxon_attributes", "name"]
  taxon_levels <- db_catalog[db_catalog$slot == "taxon_levels", "name"]
  names2concepts <- db_catalog[db_catalog$slot == "names2concepts", "name"]
  taxon_views <- db_catalog[db_catalog$slot == "taxon_views", "name"]
  Descr <- with(get_description(conn), paste(table_schema, table_name))
  # TODO: taxonomy.taxon_levels was not appearing in the decription
  ## db_catalog <- as.data.frame(rbind(
  ##   taxon_names, taxon_relations, taxon_traits,
  ##   taxon_levels, names2concepts, taxon_views
  ## ))
  ## db_catalog <- paste(db_catalog[, 1], db_catalog[, 2])
  ## if(!all(db_catalog %in% Descr))
  ##   stop("Some tables from the catalog are not occurring in the database.")
  # Import taxon names
  message("OK\nImporting taxon names ... ", appendLF = FALSE)
  Query <- paste0(
    "SELECT *\n",
    "FROM \"", paste(taxon_names, collapse = "\".\""), "\";\n"
  )
  species_obj$taxonNames <- dbGetQuery(conn, Query)
  colnames(species_obj$taxonNames) <-
    replace_x(colnames(species_obj$taxonNames),
      old = c("taxon_usage_id", "usage_name", "author_name"),
      new = c("TaxonUsageID", "TaxonName", "AuthorName")
    )
  # Import taxon concepts
  message("OK\nImporting taxon concepts ... ", appendLF = FALSE)
  Query <- paste0(
    "SELECT *\n",
    "FROM \"", paste(taxon_relations, collapse = "\".\""), "\";\n"
  )
  species_obj$taxonRelations <- dbGetQuery(conn, Query)
  colnames(species_obj$taxonRelations) <-
    replace_x(colnames(species_obj$taxonRelations),
      old = c("taxon_concept_id", "parent_id", "rank"),
      new = c("TaxonConceptID", "Parent", "Level")
    )
  # Link names and concepts
  Query <- paste0(
    "SELECT *\n",
    "FROM \"", paste(names2concepts, collapse = "\".\""), "\";\n"
  )
  concepts <- dbGetQuery(conn, Query)
  colnames(concepts) <- replace_x(colnames(concepts),
    old = c("taxon_usage_id", "taxon_concept_id", "name_status"),
    new = c("TaxonUsageID", "TaxonConceptID", "NameStatus")
  )
  species_obj$taxonNames$TaxonConceptID <-
    concepts$TaxonConceptID[match(
      species_obj$taxonNames$TaxonUsageID,
      concepts$TaxonUsageID
    )]
  species_obj$taxonNames <-
    species_obj$taxonNames[
      !is.na(species_obj$taxonNames$TaxonConceptID),
    ]
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
  Query <- paste0(
    "SELECT *\n",
    "FROM \"", paste(taxon_levels, collapse = "\".\""), "\";\n"
  )
  tax_levels <- dbGetQuery(conn, Query)
  colnames(tax_levels) <- replace_x(colnames(tax_levels),
    old = c("rank", "rank_idx"),
    new = c("Level", "rank")
  )
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
  if (!missing(taxon_traits)) {
    Query <- paste0(
      "SELECT *\n",
      "FROM \"", paste(taxon_traits, collapse = "\".\""), "\";\n"
    )
    species_obj$taxonTraits <- dbGetQuery(conn, Query)
    colnames(species_obj$taxonTraits) <-
      replace_x(colnames(species_obj$taxonTraits),
        old = "taxon_concept_id", new = "TaxonConceptID"
      )
  } else {
    species_obj$taxonTraits <- data.frame(TaxonConceptID = integer(0))
  }
  # Import taxon views
  message("OK\nImporting taxon views ... ", appendLF = FALSE)
  # TODO: Next command may need more arguments to be set
  species_obj$taxonViews <- read_pg(conn,
    name = taxon_views[1],
    main_table = taxon_views[2]
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
