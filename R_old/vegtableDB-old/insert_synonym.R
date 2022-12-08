#' @name insert_synonym
#'
#' @title Insert synonym to existing taxon concepts in database
#'
#' @description
#' Adding synonyms to existing concepts in database.
#'
#' @param conn A database connection provided by [dbConnect()].
#' @param taxonomy Character value with the name of the taxonomy in the
#'     database.
#' @param schema Character value indicating the name of the schema containing
#'     all taxonomic tables in the database.
#' @param df A data frame with new names and related information. Two columns
#'     are mandatory, namely **taxon_concept_id**, **usage_name** and
#'     **author_name**, both as character vectors.
#' @param clean A logical value indicating cleaning of characters.
#' @param ... Further arguments passed among methods.
#'
#' @rdname insert_synonym
#'
#' @export
insert_synonym <- function(conn, ...) {
  UseMethod("insert_synonym", conn)
}

#' @rdname insert_synonym
#'
#' @aliases insert_synonym,PostgreSQLConnection-method
#'
#' @export
insert_synonym.PostgreSQLConnection <- function(conn,
                                                taxonomy,
                                                schema = "plant_taxonomy",
                                                df,
                                                clean = TRUE,
                                                ...) {
  if (any(!c("taxon_concept_id", "usage_name", "author_name") %in%
    colnames(df))) {
    stop(paste(
      "Columns 'taxon_concept_id', 'usage_name' and 'author_name'",
      "are mandatory in argument 'df'."
    ))
  }
  if (any(is.na(df$taxon_concept_id))) {
    stop("NA values are not allowed in column 'taxon_concept_id' in 'df'")
  }
  if (any(is.na(df$usage_name))) {
    stop("NA values are not allowed in column 'usage_name' in 'df'")
  }
  if (any(is.na(df$author_name))) {
    stop("NA values are not allowed in column 'author_name' in 'df'")
  }
  if (any(duplicated(df[, c("usage_name", "author_name")]))) {
    stop("Duplicated combinations detected in 'df'.")
  }
  if (clean) {
    df <- clean_strings(df)
  }
  # Compare names
  Names <- dbGetQuery(conn, paste(
    "select taxon_usage_id,usage_name,author_name",
    paste0("from \"", schema, "\".taxon_names"),
    paste0(
      "where usage_name || ' ' || author_name in ('",
      paste0(paste(df$usage_name, df$author_name), collapse = "','"),
      "')"
    )
  ))
  new_names <- with(df, {
    full_name <- paste(usage_name, author_name)
    df[
      !full_name %in% paste(Names$usage_name, Names$author_name),
      c("usage_name", "author_name")
    ]
  })
  if (nrow(new_names) > 0) {
    message("Following taxon usage names will be inserted in the database:")
    print(new_names)
    pgInsert(conn, c(schema, "taxon_names"), new_names, partial.match = TRUE)
    Names <- dbGetQuery(conn, paste(
      "select taxon_usage_id,usage_name,author_name",
      paste0("from \"", schema, "\".taxon_names"),
      paste0(
        "where usage_name || ' ' || author_name in ('",
        paste0(paste(df$usage_name, df$author_name), collapse = "','"),
        "')"
      )
    ))
  }
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
  # Insert concept
  df$taxon_usage_id <- with(Names, taxon_usage_id[match(paste(
    df$usage_name,
    df$author_name
  ), paste(usage_name, author_name))])
  df$name_status <- "synonym"
  # Insert concepts
  pgInsert(conn, c(schema, "names2concepts"), df, partial.match = TRUE)
  message("DONE!")
}
