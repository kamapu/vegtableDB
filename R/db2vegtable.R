#' @name db2vegtable
#'
#' @title Import PostgreSQL databases into vegtable objects
#'
#' @description
#' Import and adaption of Postgres tables into objects of class
#' [vegtable-class].
#'
#' In the case that some schemas are not mentioned, the function assumes such
#' tables are located in the same schema as the table header. Thus for
#' databases placed in just one schema, this need to be set only in argument
#' `header_schema`.
#'
#' @param conn A database connection provided by [dbConnect()].
#' @param database A character value indicating the name of the database to be
#'     imported.
#' @param description Named vector with metadata.
#' @param geometry Name of the variable in header containing the geometry of
#'     the plots.
#' @param header_cols A character vector with the names of the columns required.
#' @param simplify_header A logical value indicating whether empty columns
#'     should be deleted from slot header or not.
#' @param as_list Logical value indicating whether a list or an object of class
#'     [vegtable-class] should be returned.
#' @param ... Further arguments passed to [db2taxlist()].
#'
#' @rdname db2vegtable
#'
#' @export
db2vegtable <- function(conn, ...) {
  UseMethod("db2vegtable", conn)
}

#' @rdname db2vegtable
#' @export
db2vegtable.PostgreSQLConnection <- function(conn,
                                             database,
                                             description,
                                             geometry = "plot_centroid",
                                             header_cols,
                                             simplify_header = TRUE,
                                             as_list = FALSE,
                                             ...) {
  veg_obj <- list()
  # description
  # TODO: Description can be also considered in the database
  if (!missing(description)) {
    veg_obj$description <- description
  } else {
    veg_obj$description <- c(remark = "Object imported by 'db2vegtable()'.")
  }
  # Import catalog
  message("Importing catalog ... ", appendLF = FALSE)
  if (!database %in% db_catalog$databases$db) {
    stop("The requested taxonomic list is not in the catalog.")
  }
  taxonomy <- with(db_catalog$databases, name[db == database &
    slot == "taxonomy"])
  db_catalog$databases <- with(db_catalog, databases[
    databases$db == database & databases$slot != "taxonomy",
    c("slot", "name")
  ])
  db_catalog$relations <- with(db_catalog, relations[
    relations$db == database,
    c("slot", "subslot", "name")
  ])
  db_catalog$relations <- with(
    db_catalog,
    split(relations[, c("subslot", "name")], relations$slot)
  )
  header <- with(db_catalog, databases[databases$slot == "header", "name"])
  samples <- with(db_catalog, databases[databases$slot == "samples", "name"])
  if ("veg_relations" %in% names(db_catalog$relations)) {
    relations <- with(db_catalog$relations, {
      relations <- list()
      for (i in unique(veg_relations$subslot)) {
        relations[[i]] <- veg_relations[veg_relations$subslot == i, "name"]
      }
      relations
    })
  } else {
    relations <- list()
  }
  if ("veg_layers" %in% names(db_catalog$relations)) {
    layers <- with(db_catalog$relations, {
      layers <- list()
      for (i in unique(veg_layers$subslot)) {
        layers[[i]] <- veg_layers[veg_layers$subslot == i, "name"]
      }
      layers
    })
  } else {
    layers <- list()
  }
  Descr <- get_description(conn)
  # TODO: Test occurrence of requested tables
  # species --------------------------------------------------------------------
  message(paste0("OK\nImporting taxonomy from '", taxonomy, "' ... "),
    appendLF = FALSE
  )
  suppressMessages(veg_obj$species <- db2taxlist(conn, taxonomy, ...))
  # header ---------------------------------------------------------------------
  message("OK\nImporting vegtable body ... ", appendLF = FALSE)
  if (missing(header_cols)) {
    header_cols <- with(Descr, column_name[table_schema == header[1] &
      table_name == header[2]])
  }
  header_cols <- unique(c("releve_id", header_cols))
  if (!missing(geometry)) {
    header_cols <- header_cols[!header_cols %in% geometry]
  }
  Query <- paste0(
    "SELECT \"", paste0(header_cols, collapse = "\",\""), "\"\n",
    "FROM \"", paste0(header, collapse = "\".\""), "\";\n"
  )
  veg_obj$header <- dbGetQuery(conn, Query)
  if (simplify_header) {
    veg_obj$header <- veg_obj$header[, apply(
      veg_obj$header, 2,
      function(x) !all(is.na(x))
    )]
  }
  # samples --------------------------------------------------------------------
  Query <- paste0(
    "SELECT *\n",
    "FROM \"", paste(samples, collapse = "\".\""), "\"\n",
    "WHERE releve_id IN (", paste0(veg_obj$header$releve_id,
      collapse = ","
    ), ");\n"
  )
  veg_obj$samples <- dbGetQuery(conn, Query)
  # layers ---------------------------------------------------------------------
  if (length(layers) > 0) {
    veg_obj$layers <- list()
    for (i in names(layers)) {
      Query <- paste0(
        "SELECT *\n",
        "FROM \"", paste(layers[[i]], collapse = "\".\""), "\";\n"
      )
      veg_obj$layers[[i]] <- dbGetQuery(conn, Query)
    }
  } else {
    veg_obj$layers <- list()
  }
  # relations ------------------------------------------------------------------
  if (length(relations) > 0) {
    veg_obj$relations <- list()
    for (i in names(relations)) {
      Query <- paste0(
        "SELECT *\n",
        "FROM \"", paste(relations[[i]], collapse = "\".\""),
        "\";\n"
      )
      veg_obj$relations[[i]] <- dbGetQuery(conn, Query)
    }
  } else {
    veg_obj$relations <- list()
  }
  # data sources ---------------------------------------------------------------
  if ("bibtexkey" %in% colnames(veg_obj$header)) {
    message("OK\nImporting source references ... ", appendLF = FALSE)
    veg_obj$relations$data_source <- cbind(
      data_source = NA,
      read_pg(conn, name = "bib_references", main_table = "main_table")
    )
    veg_obj$relations$data_source <- with(veg_obj$relations, {
      data_source <- data_source[data_source$bibtexkey %in%
        veg_obj$header$bibtexkey, ]
      data_source$data_source <- 1:nrow(data_source)
      data_source
    })
    veg_obj$header$data_source <- with(
      veg_obj$relations$data_source,
      data_source[match(veg_obj$header$bibtexkey, bibtexkey)]
    )
    veg_obj$header <- veg_obj$header[, colnames(veg_obj$header) != "bibtexkey"]
  }
  # coverconvert ---------------------------------------------------------------
  message("OK\nImporting cover conversion tables ... ", appendLF = FALSE)
  c_tables <- with(Descr, unique(table_name[table_schema == "coverconvert" &
    table_name %in% colnames(veg_obj$samples)]))
  veg_obj$coverconvert <- new("coverconvert")
  if (length(c_tables) > 0) {
    for (i in c_tables) {
      Query <- paste0(
        "SELECT *\n",
        "FROM \"coverconvert\".\"", i, "\";\n"
      )
      cover_tab <- dbGetQuery(conn, Query)
      # TODO: replace next code by a function of vegtable 'df2codeconvert()'
      veg_obj$coverconvert@value[[i]] <- with(
        cover_tab,
        factor(symbol, levels = symbol)
      )
      veg_obj$coverconvert@conversion[[i]] <- with(
        cover_tab,
        c(bottom[1], top)
      )
    }
  }
  # geometry -------------------------------------------------------------------
  # Code for renaming geometry in sf objects
  # https://gis.stackexchange.com/questions/386584/sf-geometry-column-naming-differences-r
  rename_geometry <- function(g, name) {
    current <- attr(g, "sf_column")
    names(g)[names(g) == current] <- name
    st_geometry(g) <- name
    return(g)
  }
  if (!is.null(geometry)) {
    message("OK\nImporting geometries ... ", appendLF = FALSE)
    for (i in geometry) {
      # TODO: Test table with empty cells for geometry
      Query <- paste0(
        "SELECT releve_id,\"", geometry, "\"\n",
        "FROM \"", paste0(header, collapse = "\".\""), "\"\n",
        "WHERE releve_id IN (",
        paste0(veg_obj$header$releve_id, collapse = ","), ");\n"
      )
      t_geometry <- st_read(conn, query = Query)
      t_geometry <- rename_geometry(t_geometry, "the_geometry")
      t_geometry[[i]] <- 1:nrow(t_geometry)

      veg_obj$header[, i] <- with(
        t_geometry,
        get(i)[match(veg_obj$header$releve_id, releve_id)]
      )
      veg_obj$relations[[i]] <- t_geometry[, c(i, "the_geometry")]
    }
  }
  # syntax ---------------------------------------------------------------------
  t_syntax <- with(veg_obj, gsub("syntax_", "", colnames(header)[
    grepl("syntax_", colnames(header),
      fixed = TRUE
    )
  ], fixed = TRUE))
  t_syntax <- t_syntax[t_syntax %in% db_catalog$taxonomy$db]
  veg_obj$syntax <- list()
  if (length(t_syntax) > 0) {
    message("OK\nImporting syntaxonomies ... ", appendLF = FALSE)
    for (i in t_syntax) {
      suppressMessages(veg_obj$syntax[[i]] <- db2taxlist(conn, i))
    }
  }
  # replace names --------------------------------------------------------------
  colnames(veg_obj$header) <- replace_x(colnames(veg_obj$header),
    old = "releve_id", new = "ReleveID"
  )
  colnames(veg_obj$samples) <- replace_x(colnames(veg_obj$samples),
    old = c("releve_id", "taxon_usage_id"),
    new = c("ReleveID", "TaxonUsageID")
  )
  # final output ---------------------------------------------------------------
  message("OK\nDONE!\n")
  if (as_list) {
    invisible(veg_obj)
  } else {
    veg_obj <- new("vegtable",
      description = clean_strings(veg_obj$description),
      samples = veg_obj$samples,
      header = clean_strings(veg_obj$header),
      species = veg_obj$species,
      relations = veg_obj$relations,
      syntax = veg_obj$syntax,
      coverconvert = veg_obj$coverconvert
    )
    return(veg_obj)
  }
}
