#' @name insert_synonyms
#'
#' @title Insert synonym to existing taxon concepts in database
#'
#' @description
#' Adding synonyms to existing concepts in database.
#'
#' @param conn A database connection provided by [divDB::connect_db()].
#' @param schema Character value indicating the name of the schema containing
#'     all taxonomic tables in the database.
#' @param df A data frame with new names and related information. Three columns
#'     are mandatory, namely **taxon_concept_id**, **usage_name** and
#'     **author_name**. Alternatively a column **taxon_usage_id** can be added
#'     for names already existing in the database.
#' @param clean A logical value indicating cleaning of characters.
#' @param eval A logical value indicating whether the produced SQL commands
#'     should be sent to the database or not.
#' @param ... Further arguments passed to [insert_names()].
#'
#' @rdname insert_synonyms
#'
#' @export
insert_synonyms <- function(conn, ...) {
  UseMethod("insert_synonyms", conn)
}

#' @rdname insert_synonyms
#' @aliases insert_synonyms,PostgreSQLConnection-method
#' @export
insert_synonyms.PostgreSQLConnection <- function(conn,
                                                 df,
                                                 schema = "plant_taxonomy",
                                                 clean = TRUE,
                                                 eval = TRUE,
                                                 ...) {
  # Check for missing columns
  if (!"taxon_usage_id" %in% names(df)) {
    df$taxon_usage_id <- NA
  }
  mandatory_names <- c(
    "taxon_concept_id", "taxon_usage_id", "usage_name",
    "author_name"
  )
  if (any(!mandatory_names %in% names(df))) {
    missing_names <- mandatory_names[!mandatory_names %in% names(df)]
    stop(paste0(
      "Following mandatory columns are missing in 'df':\n  ",
      paste0(missing_names, collapse = ", "), "."
    ))
  }
  # Clean strings
  if (clean) {
    df <- clean_strings(df)
  }
  # Is usage ID already in the database?
  usage_ids <- df$taxon_usage_id[!is.na(df$taxon_usage_id)]
  if (length(usage_ids)) {
    usage_ids_db <- unlist(dbGetQuery(conn, statement = paste0(
      "SELECT taxon_usage_id\n",
      "FROM \"", schema, "\".taxon_names\n",
      "WHERE taxon_usage_id IN ('", paste0(usage_ids, collapse = "', '"), "')"
    )))
  }
  if (length(usage_ids) > length(usage_ids_db)) {
    stop(paste0(
      "Following usage ids declared in 'df' are not in database:\n  ",
      paste0(usage_ids[!usage_ids %in% usage_ids_db], collapse = ", ", ".")
    ))
  }
  # Check for missing author names in data frame
  missing_authors <- with(
    df,
    which(is.na(taxon_usage_id) & is.na(author_name))
  )
  if (length(missing_authors)) {
    stop(paste0(
      "Missing author names are not allowed for new names\n",
      "  Complement following names:\n  ",
      paste0(missing_authors, ": ", df[missing_authors, "usage_name"], "\n",
        collapse = ""
      )
    ))
  }
  # Check for names already existing in the database
  given_names <- df[is.na(df$usage_name), c("usage_name", "author_name")]
  test_query <- paste0(
    "SELECT taxon_usage_id, usage_name, author_name\n",
    "FROM \"", schema, "\".taxon_names\n",
    "WHERE (usage_name, author_name) IN (\n",
    with(given_names, paste0("  ('", usage_name, "','", author_name, "')\n",
      collapse = ""
    )),
    ")"
  )
  recycled_names <- DBI::dbGetQuery(conn, statement = test_query)
  if (nrow(recycled_names)) {
    df$taxon_usage_id <- recycled_names$taxon_usage_id[
      match(
        with(df(paste(usage_name, author_name))),
        with(recycled_names, paste(usage_name, author_name))
      )
    ]
  }
  # Test names already in use at database
  usage_ids <- df[!is.na(df$taxon_usage_id), ]
  if (nrow(usage_ids)) {
    test_query <- paste0(
      "SELECT n.taxon_usage_id,n.taxon_concept_id,c.top_view\n",
      "FROM \"", schema, "\".names2concepts AS n, \"", schema,
      "\".taxon_concepts AS c\n",
      "WHERE n.taxon_usage_id IN ('",
      paste0(usage_ids$taxon_usage_id, collapse = "','"), "')\n",
      "  OR n.taxon_concept_id IN ('",
      paste0(usage_ids$taxon_concept_id, collapse = "','"), "')"
    )
  }
  df$top_view <- usage_ids$top_view[match(
    df$taxon_concept_id,
    usage_ids$taxon_concept_id
  )]
  df$check <- NA
  for (i in seq_len(nrow(df))) {
    tmp_df <- usage_ids[usage_ids$taxon_usage_id == df$taxon_usage_id[i], ]
    df$check[i] <- df$top_view[i] %in% tmp_df$top_view
  }
  if (any(df$check)) {
    stop(
      paste0(
        "Following names are already synonyms in use:\n",
        with(
          df[df$check, ],
          paste0("    ", top_view, ": ", taxon_usage_id, " ", usage_name, " ",
            author_name, "\n",
            collapse = ""
          )
        )
      )
    )
  }
  # Assign new ids
  usage_id <- unlist(dbGetQuery(conn, paste(
    "SELECT taxon_usage_id",
    paste0("FROM \"", schema, "\".taxon_names")
  )))
  new_ids <- taxlist::id_solver(
    seq_len(sum(is.na(df$taxon_usage_id))),
    usage_id
  )
  df$taxon_usage_id[is.na(df$taxon_usage_id)] <- new_ids
  # Recheck double assignments
  double_synonyms <- df[duplicated(df[
    ,
    c("taxon_usage_id", "top_view")
  ]), ]
  if (ncol(double_synonyms)) {
    stop(paste0(
      "Following synonyms will cause duplicated use:\n",
      with(double_synonyms, paste0("  ", top_view, ": ", usage_name, " ",
        author_name, "\n",
        collapse = ""
      ))
    ))
  }
  # Add new names
  query <- divDB::insert_rows(conn, df[
    df$taxon_usage_id %in% new_ids,
    c("taxon_usage_id", "usage_name", "author_name")
  ],
  name = c(schema, "taxon_names"), eval = FALSE
  )
  # Insert synonyms
  tax_id <- unlist(dbGetQuery(conn, paste(
    "SELECT tax_id",
    paste0("FROM \"", schema, "\".names2concepts")
  )))
  df$tax_id <- taxlist::id_solver(seq_len(nrow(df)), tax_id)
  df$name_status <- "synonym"
  query <- c(query,
    divDB::insert_rows(
      conn,
      df[, c("taxon_concept_id", "taxon_usage_id", "name_status", "tax_id")]
    ),
    eval = FALSE
  )
  # Message recycled names
  if (nrow(recycled_names)) {
    message(paste0(
      "Following names will be recycled:\n",
      with(
        recycled_names,
        paste0("    ", usage_name, author_name, "\n", collapse = "")
      )
    ))
  }
  # Run query, if requested
  if (eval) {
    dbSendQuery(conn, query)
    message("DONE!")
  }
  # Return sql invisible
  invisible(query)
}
