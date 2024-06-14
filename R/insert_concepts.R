#' @name insert_concepts
#'
#' @title Insert taxonomic concepts in database
#'
#' @description
#' Insert new taxonomic concepts by using accepted names.
#' A previous use of the name in the taxonomy will retrieve an error message.
#' Names that are new for the database will be inserted in the main list of
#' names.
#'
#' @param conn A database connection provided by [dbConnect()].
#' @param taxonomy Character value with the name of the taxonomy in the
#'     database.
#' @param schema Character value indicating the name of the schema containing
#'     all taxonomic tables in the database.
#' @param df A data frame with new names and related information. Two columns
#'     are mandatory, namely **usage_name** and **author_name**, both as
#'     character vectors. Further important columns are **rank** (taxonomic
#'     rank), **parent_id** (concept ID of the parent taxon), and **view_key**
#'     (bibtexkey of the reference used as taxon view).
#' @param clean A logical value indicating cleaning of characters.
#' @param eval A logical value indicating whether the produced SQL commands
#'     should be sent to the database or not.
#' @param ... Further arguments passed among methods.
#'
#' @rdname insert_concepts
#'
#' @export
insert_concepts <- function(conn, ...) {
  UseMethod("insert_concepts", conn)
}

#' @rdname insert_concepts
#' @aliases insert_concepts,PostgreSQLConnection-method
#' @export
insert_concepts.PostgreSQLConnection <- function(conn,
                                                 taxonomy,
                                                 schema = "plant_taxonomy",
                                                 df,
                                                 clean = TRUE,
                                                 eval = TRUE,
                                                 ...) {
  # Clean strings
  if (clean) {
    df <- clean_strings(df)
  }
  # Check existence
  if (!dbExistsTable(conn, c(schema, "taxon_names"))) {
    stop("The input schema does not contain a table 'taxon_names'")
  }
  df_cols <- c("usage_name", "author_name")
  df_cols <- df_cols[!df_cols %in% names(df)]
  if (length(df_cols)) {
    stop(paste0(
      "Following mandatory columns are missing in 'df': '",
      paste0(df_cols, collapse = "', '"), "'."
    ))
  }
  # Check if the names already exists in database
  full_names <- with(df, paste(usage_name, author_name))
  names_in_db <- unlist(dbGetQuery(
    conn,
    paste0(
      "select usage_name||' '||author_name as full_name\n", "from \"",
      schema,
      "\".taxon_names\n", "where usage_name||' '||author_name in ('",
      paste0(full_names, collapse = "', '"), "')"
    )
  ))
  not_in_db <- full_names[!full_names %in% names_in_db]
  if (length(not_in_db)) {
    stop(paste0(
      "Following names are not yet in database:\n    ",
      paste0(not_in_db, collapse = "\n    "),
      "\nuse 'insert_names()' in advance!"
    ))
  }

  # Retrieve names IDs
  Names <- dbGetQuery(conn, paste(
    "select taxon_usage_id,usage_name,author_name",
    paste0("from \"", schema, "\".taxon_names"),
    paste0(
      "where usage_name || ' ' || author_name in ('",
      paste0(paste(df$usage_name, df$author_name), collapse = "','"),
      "')"
    )
  ))

  df$taxon_usage_id <- Names$taxon_usage_id[match(
    with(df, paste(usage_name, author_name)),
    with(Names, paste(usage_name, author_name))
  )]
  n2c <- dbGetQuery(conn, paste(
    "select *",
    paste0("from \"", schema, "\".names2concepts"),
    paste0("where taxon_usage_id in (", paste0(Names$taxon_usage_id,
      collapse = ","
    ), ")")
  ))
  if (nrow(n2c) > 0) {
    conc_id <- unlist(dbGetQuery(conn, paste(
      "select taxon_concept_id",
      paste0("from \"", schema, "\".taxon_concepts"),
      paste0("where taxon_concept_id in (", paste0(n2c$taxon_concept_id,
        collapse = ","
      ), ")"),
      paste0("and top_view = '", taxonomy, "'")
    )))
    n2c <- n2c[n2c$taxon_concept_id %in% conc_id, ]
  }
  if (nrow(n2c) > 0) {
    stop(paste0(
      "Following concepts are already using the requested names:\n",
      paste0(n2c$taxon_concept_id, collapse = ", ")
    ))
  }
  # Cross-check parents
  if ("parent_id" %in% names(df)) {
    p1 <- unique(df$parent_id[!is.na(df$parent_id)])
    p2 <- unlist(dbGetQuery(conn, paste(
      "select taxon_concept_id",
      paste0("from ", schema, ".taxon_concepts"),
      paste0("where taxon_concept_id in (", paste0(p1, collapse = ","), ")")
    )))
    p1 <- p1[!p1 %in% p2]
    if (length(p1) > 0) {
      stop(paste0(
        "Following 'parent_id' in 'df' are not included as concepts ",
        "in the database:\n", paste0(p1, collapse = ", ")
      ))
    }
  }
  # Cross-check taxonomic ranks
  if ("rank" %in% names(df)) {
    r1 <- unique(paste(df$rank[!is.na(df$rank)]))
    r2 <- unlist(dbGetQuery(conn, paste(
      "select \"rank\"",
      paste0("from \"", schema, "\".taxon_levels"),
      paste0(
        "where \"rank\" in ('", paste0(r1, collapse = "','"),
        "')"
      )
    )))
    r1 <- r1[!r1 %in% r2]
    if (length(r1) > 0) {
      stop(paste0(
        "Following values for 'rank' are not included",
        "in the database:\n",
        paste0(r2, collapse = ", ")
      ))
    }
  }
  # Import concept
  concept_id <- unlist(dbGetQuery(conn, paste(
    "select max(taxon_concept_id)",
    paste0("from \"", schema, "\".taxon_concepts")
  )))
  df$taxon_concept_id <- concept_id + 1:nrow(df)
  df$top_view <- taxonomy
  df$name_status <- "accepted"
  df$taxon_usage_id <- with(Names, taxon_usage_id[match(
    paste(df$usage_name, df$author_name), paste(
      usage_name,
      author_name
    )
  )])
  tax_id <- unlist(dbGetQuery(conn, paste(
    "select max(tax_id)",
    paste0("from \"", schema, "\".names2concepts")
  )))
  df$tax_id <- tax_id + 1:nrow(df)
  # Insert concepts
  tc_col_names <- unlist(dbGetQuery(conn, paste(
    "select column_name",
    "from information_schema.columns",
    paste0("where table_schema = '", schema, "'"),
    "and table_name = 'taxon_concepts'"
  )))
  n2c_col_names <- unlist(dbGetQuery(conn, paste(
    "select column_name",
    "from information_schema.columns",
    paste0("where table_schema = '", schema, "'"),
    "and table_name = 'names2concepts'"
  )))
  # Write queries
  query <- insert_rows(conn, df[, names(df) %in% tc_col_names],
    c(schema, "taxon_concepts"),
    eval = FALSE
  )
  query <- c(query, insert_rows(conn, df[, names(df) %in% n2c_col_names],
    c(schema, "names2concepts"),
    eval = FALSE
  ))
  class(query) <- c("sql", "character")
  # Run query, if requested
  if (eval) {
    dbSendQuery(conn, query)
    message("DONE!")
  }
  # Return sql invisible
  invisible(query)
}
