#' @name insert_synonyms
#'
#' @title Insert synonym to existing taxon concepts in database
#'
#' @description
#' Adding synonyms to existing concepts in database.
#'
#' @param conn A database connection provided by [dbConnect()].
#' @param schema Character value indicating the name of the schema containing
#'     all taxonomic tables in the database.
#' @param df A data frame with new names and related information. Two columns
#'     are mandatory, namely **taxon_concept_id**, **usage_name** and
#'     **author_name**, both as character vectors.
#' @param clean A logical value indicating cleaning of characters.
#' @param ... Further arguments passed among methods.
#'
#' @rdname insert_synonyms
#'
#' @export
insert_synonyms <- function(conn, ...) {
  UseMethod("insert_synonyms", conn)
}

#' @rdname insert_synonyms
#'
#' @aliases insert_synonyms,PostgreSQLConnection-method
#'
#' @export
insert_synonyms.PostgreSQLConnection <- function(conn,
                                                 df,
                                                 schema = "plant_taxonomy",
                                                 clean = TRUE,
                                                 ...) {
  if (any(!c("taxon_concept_id", "usage_name", "author_name") %in%
    colnames(df))) {
    stop(paste(
      "Columns 'taxon_concept_id', 'usage_name' and 'author_name'",
      "are mandatory in argument 'df'."
    ))
  }
  if (any(is.na(df$author_name))) {
    stop("NA values are not allowed in column 'author_name' in 'df'")
  }
  if (any(duplicated(df[, c("usage_name", "author_name")]))) {
    stop("Duplicated combinations detected in 'df'.")
  }
  # Clean strings
  if (clean) {
    df <- clean_strings(df)
  }
  # Append names
  insert_names(conn, df, schema)
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
  df$name_status <- "synonym"
  # Used usages (a name cannot appear twice in a taxonomy)
  used_names <- dbGetQuery(conn, paste(
    "select taxon_concept_id,taxon_usage_id",
    paste0("from \"", schema, "\".names2concepts"),
    paste0(
      "where taxon_usage_id in (",
      paste0(df$taxon_usage_id, collapse = ","), ")"
    )
  ))
  if (nrow(used_names) > 0) {
    used_names <- merge(used_names, dbGetQuery(
      conn,
      paste(
        "select taxon_concept_id,top_view",
        paste0("from \"", schema, "\".taxon_concepts"),
        paste0(
          "where taxon_concept_id in (",
          paste0(used_names$taxon_concept_id, collapse = ","), ")"
        )
      )
    ))
    df <- merge(df, dbGetQuery(
      conn,
      paste(
        "select taxon_concept_id,top_view",
        paste0("from \"", schema, "\".taxon_concepts"),
        paste0(
          "where taxon_concept_id in (",
          paste0(df$taxon_concept_id, collapse = ","), ")"
        )
      )
    ))
    df2 <- df[with(df, paste(top_view, as.character(taxon_usage_id))) %in%
      with(used_names, paste(top_view, as.character(taxon_usage_id))), ]
    if (nrow(df2) > 0) {
      stop(paste0(
        "Following synonyms will cause duplicate usages:\n'",
        paste0(paste(df2$usage_name, df2$author_name, "in", df2$top_view),
          collapse = "'\n'"
        ), "'"
      ))
    }
  }
  # Create new IDs
  tax_id <- unlist(dbGetQuery(conn, paste(
              "select max(tax_id)",
              paste0("from \"", schema, "\".names2concepts")
          )))
  df$tax_id <- tax_id + seq_along(df$taxon_concept_id)
  dbWriteTable(conn, c(schema, "names2concepts"),
    df[, c("tax_id", "taxon_usage_id", "taxon_concept_id", "name_status")],
    append = TRUE, row.names = FALSE
  )
  message("DONE!")
}
