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
#' @param database Character value indicating the name of the database to be
#'     imported.
#' @param geometry Character vectors indicating the name of the variables
#'     stored as geometries in PostGIS.
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
                                             geometry = "plot_centroid",
                                             as_list = FALSE,
                                             ...) {
  veg_obj <- list()
  # get tables and schemas
  message("Importing metadata ... ", appendLF = FALSE)
  Query <- paste(
    "select table_schema schema,table_name table",
    "from information_schema.tables", "where is_insertable_into = 'YES'"
  )
  db_tables <- dbGetQuery(conn, Query)
  # description ----------------------------------------------------------------
  Query <- paste(
    "select *", "from environment.databases",
    paste0("where db_name = '", database, "'")
  )
  veg_obj$description <- unlist(dbGetQuery(conn, Query))
  # species --------------------------------------------------------------------
  message("OK\nImporting taxonomic list ... ", appendLF = FALSE)
  suppressMessages(veg_obj$species <- db2taxlist(conn,
    taxonomy = veg_obj$description["taxonomy"], ...
  ))
  # header ---------------------------------------------------------------------
  message("OK\nImporting header table and relations ... ", appendLF = FALSE)
  Query <- paste(
    "select column_name", "from information_schema.columns",
    "where table_schema = 'environment'", "and table_name = 'header'"
  )
  header_cols <- unlist(dbGetQuery(conn, Query))
  if (!geometry %in% header_cols) {
    stop(paste0(
      "Wrong value for 'geometry': Variable '", geometry,
      "' not occurring in table 'header'."
    ))
  }
  header_cols <- header_cols[header_cols != geometry]
  Query <- paste(
    "select", paste0(header_cols, collapse = ","),
    "from environment.\"header\"",
    paste0("where db_name ='", database, "'")
  )
  veg_obj$header <- dbGetQuery(conn, Query)
  veg_obj$header <- veg_obj$header[, apply(
    veg_obj$header, 2,
    function(x) !all(is.na(x))
  )]
  colnames(veg_obj$header)[colnames(veg_obj$header) == "releve_id"] <-
    "ReleveID"
  # Importing relations --------------------------------------------------------
  rel_names <- with(db_tables, table[schema == "environment" &
    table %in% names(veg_obj$header)])
  veg_obj$relations <- list()
  if (length(rel_names) > 0) {
    for (i in rel_names) {
      veg_obj$relations[[i]] <- dbGetQuery(
        conn,
        paste(
          "select *",
          paste0("from environment.\"", i, "\"")
        )
      )
    }
  }
  # Importing samples ----------------------------------------------------------
  message("OK\nImporting samples' records and layers ... ", appendLF = FALSE)
  Query <- paste(
    "select *", "from records.samples",
    paste0(
      "where releve_id in (",
      paste0(veg_obj$header$ReleveID, collapse = ","), ")"
    )
  )
  veg_obj$samples <- dbGetQuery(conn, Query)
  Query <- paste(
    "select tax_id,taxon_usage_id",
    "from plant_taxonomy.names2concepts",
    paste0(
      "where tax_id in (",
      paste0(veg_obj$samples$tax_id, collapse = ","), ")"
    )
  )
  Tax <- dbGetQuery(conn, Query)
  veg_obj$samples$TaxonUsageID <- with(
    veg_obj$samples,
    Tax$taxon_usage_id[match(tax_id, Tax$tax_id)]
  )
  colnames(veg_obj$samples)[colnames(veg_obj$samples) == "releve_id"] <-
    "ReleveID"
  veg_obj$samples <- veg_obj$samples[, apply(
    veg_obj$samples, 2,
    function(x) !all(is.na(x))
  )]
  # Delete tax_id from samples
  veg_obj$samples <- veg_obj$samples[, names(veg_obj$samples) != "tax_id"]
  # Import layers --------------------------------------------------------------
  lay_names <- with(db_tables, table[schema == "records" &
    table %in% names(veg_obj$samples)])
  veg_obj$layers <- list()
  if (length(lay_names) > 0) {
    for (i in lay_names) {
      veg_obj$layers[[i]] <- dbGetQuery(
        conn,
        paste(
          "select *",
          paste0("from records.\"", i, "\"")
        )
      )
    }
  }
  # data sources ---------------------------------------------------------------
  if ("bibtexkey" %in% colnames(veg_obj$header)) {
    message("OK\nImporting source references ... ", appendLF = FALSE)
    veg_obj$relations$data_source <- cbind(
      data_source = NA,
      read_pg(conn, name = "bib_references", main_table = "main_table")
    )
    class(veg_obj$relations$data_source) <- c("lib_df", "data.frame")
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
  c_tables <- with(db_tables, table[schema == "coverconvert" &
    table %in% names(veg_obj$samples)])
  veg_obj$coverconvert <- new("coverconvert")
  if (length(c_tables) > 0) {
    for (i in c_tables) {
      Query <- paste("select *", paste0("from coverconvert.\"", i, "\""))
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
  if (!is.null(geometry)) {
    message("OK\nImporting geometries ... ", appendLF = FALSE)
    for (i in geometry) {
      # TODO: Test table with empty cells for geometry
      Query <- paste(
        paste0("select releve_id,\"", geometry, "\""),
        paste0("from environment.\"header\""),
        paste0("where releve_id in (", paste0(veg_obj$header$ReleveID,
          collapse = ","
        ), ")")
      )
      t_geometry <- st_read(conn, query = Query)
      st_geometry(t_geometry) <- "the_geometry"
      t_geometry[[i]] <- 1:nrow(t_geometry)
      veg_obj$header[, i] <- with(
        t_geometry,
        get(i)[match(veg_obj$header$releve_id, releve_id)]
      )
      veg_obj$relations[[i]] <- t_geometry[, c(i, "the_geometry")]
    }
  }
  # syntax ---------------------------------------------------------------------
  veg_obj$syntax <- list()
  t_syntax <- colnames(veg_obj$header)[grepl("syntax_",
    colnames(veg_obj$header),
    fixed = TRUE
  )]
  if (length(t_syntax) > 0) {
    message("OK\nImporting syntaxonomies ... ", appendLF = FALSE)
    t_syntax <- str_split(t_syntax, "_")
    t_syntax <- lapply(t_syntax, function(x) {
      c(
        name = paste(x[2], x[3], sep = "_"),
        approach = paste(x[1], x[2], sep = "_"),
        taxonomy = x[3],
        header = paste(x[1], x[2], x[3], sep = "_")
      )
    })
    t_syntax <- as.data.frame(do.call(rbind, t_syntax),
      stringsAsFactors = FALSE
    )
    for (i in 1:nrow(t_syntax)) {
      suppressMessages(veg_obj$syntax[[t_syntax$name[i]]] <- with(
        t_syntax,
        db2taxlist(conn, taxonomy = taxonomy[i], schema = approach[i])
      ))
      Tax <- veg_obj$header[[t_syntax$header[i]]][
        !is.na(veg_obj$header[[t_syntax$header[i]]])
      ]
      Query <- paste(
        "select tax_id,taxon_usage_id",
        paste0("from \"", t_syntax$approach[i], "\".names2concepts"),
        paste0("where tax_id in (", paste0(Tax, collapse = ","), ")")
      )
      Tax <- dbGetQuery(conn, Query)
      veg_obj$header[[t_syntax$header[i]]] <- Tax$taxon_usage_id[match(
        veg_obj$header[[t_syntax$header[i]]], Tax$tax_id
      )]
    }
  }
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
