#' @name insert_concept
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
#'     character vectors.
#' @param clean A logical value indicating cleaning of characters.
#' @param ... Further arguments passed among methods.
#'
#' @rdname insert_concept
#'
#' @export
insert_concept <- function(conn, ...) {
  UseMethod("insert_concept", conn)
}

#' @rdname insert_concept
#'
#' @aliases insert_concept,PostgreSQLConnection-method
#'
#' @export
insert_concept.PostgreSQLConnection <- function(conn,
                                                taxonomy,
                                                schema = "plant_taxonomy",
                                                df,
                                                clean = TRUE,
                                                ...) {
  if (any(!c("usage_name", "author_name") %in% colnames(df))) {
    stop(paste(
      "Columns 'usage_name' and 'author_name'",
      "are mandatory in argument 'df'."
    ))
  }
  if (any(duplicated(df[, c("usage_name", "author_name")]))) {
    stop("Duplicated combinations detected in 'df'.")
  }
  if ("taxon_concept_id" %in% colnames(df)) {
    stop(paste(
      "Column 'taxon_concept_id' detected in 'df'.",
      "Use 'insert_synonym()' instead"
    ))
  }
  if (clean) {
    df <- clean_strings(df)
  }
  #

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
    n2c <- unlist(dbGetQuery(conn, paste(
      "select taxon_concept_id",
      paste0("from \"", schema, "\".taxon_concepts"),
      paste0("where taxon_concept_id in (", paste0(n2c$taxon_concept_id,
        collapse = ","
      ), ")"),
      paste0("and top_view = '", taxonomy, "'")
    )))
  }
  if (nrow(n2c) > 0) {
    stop(paste0(
      "Following concepts are already using the requested names:\n",
      paste0(n2c$taxon_concept_id, collapse = ", ")
    ))
  }
  # Cross-check parents
  if ("parent_id" %in% names(df)) {
    p1 <- df$parent_id[!is.na(df$parent_id)]
    p2 <- unlist(dbGetQuery(conn, paste(
      "select parent_id",
      paste0("where parent_id in (", paste0(p1, collapse = ","), ")")
    )))
    p1 <- p1[!p1 %in% p2]
    if (length(p1) > 0) {
      stop(paste0(
        "Following 'parent_id' in 'df' are notincluded as concepts ",
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
  old_concepts <- unlist(dbGetQuery(conn, paste(
    "select taxon_concept_id",
    paste0("from \"", schema, "\".taxon_concepts")
  )))
  df$taxon_concept_id <- max(old_concepts) + 1:nrow(df)
  df$top_view <- taxonomy
  df$name_status <- "accepted"
  df$taxon_usage_id <- with(Names, taxon_usage_id[match(
    paste(df$usage_name, df$author_name), paste(
      usage_name,
      author_name
    )
  )])
  # Insert concepts
  pgInsert(conn, c(schema, "taxon_concepts"), df, partial.match = TRUE)
  pgInsert(conn, c(schema, "names2concepts"), df, partial.match = TRUE)
  message("DONE!")
}
