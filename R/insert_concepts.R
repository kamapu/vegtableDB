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
#' @param ... Further arguments passed to [insert_names()].
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
  names_in_db <- dbGetQuery(
    conn,
    paste0(
      "select taxon_usage_id, usage_name||' '||author_name as full_name\n",
      "from \"",
      schema,
      "\".taxon_names\n", "where usage_name||' '||author_name in ('",
      paste0(full_names, collapse = "', '"), "')"
    )
  )
  # Update names
  sql <- insert_names(conn, df, schema, eval = FALSE, ...)
  # Retrieving IDs for names
  usage_id <- unlist(dbGetQuery(conn, paste(
              "select taxon_usage_id",
              paste0("from \"", schema, "\".taxon_names")
          )))
  idx <- !full_names %in% names_in_db$full_name
  df$taxon_usage_id[!idx] <- names_in_db$taxon_usage_id
  df$taxon_usage_id[idx] <- id_solver(c(1:sum(idx)), usage_id)
  # Retrieve further ids
  concept_id <- unlist(dbGetQuery(conn, paste(
              "select taxon_concept_id",
              paste0("from \"", schema, "\".taxon_concepts"),
              paste0("where top_view = '", taxonomy, "'"))))
  n2c <- dbGetQuery(conn, paste(
          "select tax_id, taxon_usage_id",
          paste0("from \"", schema, "\".names2concepts"),
          paste0("where taxon_concept_id in (", paste0(concept_id,
                  collapse = ","), ")")))
  # If name already in use
  in_use <- df$taxon_usage_id %in% n2c$taxon_usage_id
  if (any(in_use))
    stop(with(df, paste0("Names already used by taxonomy '", taxonomy,
                "':\n", paste0("    - ", usage_name[in_use],
                            " (", taxon_usage_id[in_use], ")",
                            collapse = "\n"))))
  # Solve further ids
  df$taxon_concept_id <- id_solver(c(1:nrow(df)), concept_id)
  df$tax_id <- id_solver(c(1:nrow(df)), n2c$tax_id)
  # Check ranks and parents
  if (all(c("parent_id", "rank") %in% names(df))) {
    parents_not_in_db <- with(df,
        parent_id[(!is.na(parent_id)) & (!parent_id %in% concept_id)])
    if (length(parents_not_in_db))
      stop(paste0("Following entries of 'parent_id' in 'df'",
              "does not exist in database:\n", paste0("    - ",
                  unique(parents_not_in_db), collapse = "\n")))
    rank_table <- dbGetQuery(conn, paste(
            "select rank, rank_idx",
            paste0("from \"", schema, "\".taxon_levels")))
    parents_in_df <- unique(df$parent_id[!is.na(df$parent_id)])
    rank_parents <- dbGetQuery(conn, paste(
            "select taxon_concept_id, rank",
            paste0("from \"", schema, "\".taxon_concepts"),
            paste0("where taxon_concept_id in (", paste0(parents_in_df,
                    collapse = ","), ")")))
    rank_parents$rank_idx <- with(rank_table,
        rank_idx[match(rank_parents$rank, rank)])
    df_rank_table <- data.frame(
        level = with(rank_table, rank_idx[match(df$rank, rank)]),
        parent_level = with(rank_parents,
            rank_idx[match(df$parent_id, taxon_concept_id)]))
    if(any(df_rank_table$level >= df_rank_table$parent_level))
      stop(paste("Some of the proposed parents have the same ",
              "or lower rank than the respective child"))
  }
  # Insert rows suppressing warnings
  suppressWarnings({
        sql <- c(sql, insert_rows(conn, df, c(schema, "taxon_concepts"),
                eval = FALSE))
        sql <- c(sql, insert_rows(conn, df, c(schema, "names2concepts"),
                eval = FALSE))
      })
  # TODO: Define a function for updating and inserting species attributes.
  class(sql) <- c("sql", "character")
  # Run query, if requested
  if (eval) {
    dbSendQuery(conn, query)
    message("DONE!")
  }
  # Return sql invisible
  invisible(query)
}
