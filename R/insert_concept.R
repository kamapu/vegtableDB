#' @name insert_concept
#'
#' @title Insert names or concepts in PostgreSQL taxonomic lists
#'
#' @description
#' Insert synonyms to existing taxa in a PostgreSQL version of [taxlist-class]
#' objects.
#'
#' This function is updating the tables `taxonNames` and `names2concepts` in
#' the PostgreSQL version of the database.
#'
#' @param conn A database connection provided by [dbConnect()].
#' @param taxon_names,taxon_relations,names2concepts,taxon_views,taxon_levels
#'     Character vectors indicating the name of the respective schemas and
#'     tables in database.
#' @param df A data frame with new names and related information (including
#'     taxon concept ID).
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
insert_concept.PostgreSQLConnection <- function(conn, taxon_names,
                                                taxon_relations, names2concepts, taxon_views, taxon_levels, df,
                                                clean = TRUE, ...) {
  if (clean) {
    df <- clean_strings(df)
  }
  if (any(!c("usage_name", "author_name") %in% colnames(df))) {
    stop(paste(
      "Columns 'usage_name' and 'author_name'",
      "are mandatory in argument 'df'."
    ))
  }
  if ("taxon_concept_id" %in% colnames(df)) {
    stop(paste(
      "Column 'taxon_concept_id' detected in 'df'.",
      "Use 'insert_synonym()' instead"
    ))
  }
  ## Cross-check
  # 0: Required assets
  Query <- paste0(
    "SELECT taxon_usage_id, taxon_concept_id\n",
    "FROM \"", paste0(names2concepts, collapse = "\".\""), "\";\n"
  )
  n2c <- dbGetQuery(conn, Query)
  Query <- paste0(
    "SELECT *\n",
    "FROM \"", paste0(taxon_names, collapse = "\".\""), "\";\n"
  )
  t_names <- dbGetQuery(conn, Query)
  Query <- paste0(
    "SELECT *\n",
    "FROM \"", paste0(taxon_levels, collapse = "\".\""), "\";\n"
  )
  t_levels <- dbGetQuery(conn, Query)
  Query <- paste0(
    "SELECT *\n",
    "FROM \"", paste0(taxon_relations, collapse = "\".\""), "\";\n"
  )
  t_concepts <- dbGetQuery(conn, Query)
  Query <- paste0(
    "SELECT bibtexkey\n",
    "FROM \"", paste0(taxon_views, collapse = "\".\""), "\";\n"
  )
  b_keys <- unlist(dbGetQuery(conn, Query))
  # 1: Check duplicated combinations in 'df'
  if (any(duplicated(df[, c("usage_name", "author_name")]))) {
    stop("Duplicated combinations detected in 'df'.")
  }
  # 2: Check combinations already existing in database and add IDs
  if (any(with(df, paste(usage_name, author_name)) %in%
    with(t_names, paste(usage_name, author_name)))) {
    message(paste(
      "Some combinations in 'df' already exist in the database",
      "and may be recycled."
    ))
  }
  df$taxon_usage_id <- with(t_names, taxon_usage_id[
    match(
      paste(df$usage_name, df$author_name),
      paste(usage_name, author_name)
    )
  ])
  new_ids <- sum(is.na(df$taxon_usage_id))
  if (new_ids > 0) {
    df$taxon_usage_id[is.na(df$taxon_usage_id)] <-
      max(t_names$taxon_usage_id) + 1:new_ids
  }
  # 3: Check that usage names are not already in use
  if (any(df$taxon_usage_id %in% n2c$taxon_usage_id)) {
    stop("Some usage names are already in use in the database.")
  }
  # 4: Check existence of parents in database
  if ("parent_id" %in% colnames(df) &
    !all(df$parent_id %in% n2c$taxon_concept_id)) {
    stop(paste(
      "Some entries for 'parent_id' in 'df' are not",
      "occurring in the database."
    ))
  }
  # 5: Check existence of levels in database
  if ("rank" %in% colnames(df)) {
    if (any(!df$rank %in% t_levels$rank)) {
      stop(paste(
        "Some entries for 'rank' in 'df' are not",
        "occurring in the database."
      ))
    }
  }
  # 6: Check parents higher than children
  if (all(c("rank", "parent_id") %in% colnames(df))) {
    parent_l <- with(t_concepts, rank[match(df$parent_id, taxon_concept_id)])
    parent_l <- with(t_levels, rank_idx[match(parent_l, rank)])
    child_l <- with(t_levels, rank_idx[match(df$rank, rank)])
    if (any(child_l >= parent_l)) {
      stop("All parents have to be of higher rank than the inserted children.")
    }
  }
  # 7: Check existence of taxon views
  if ("view_key" %in% colnames(df) & !all(df$view_key %in% b_keys)) {
    stop("Some values of 'view_key' in 'df' are not occurring in the database.")
  }
  ## TODO: Allow the possibility of inserting some taxon traits
  ## Prepare data frame
  df$taxon_concept_id <- max(t_concepts$taxon_concept_id) + 1:nrow(df)
  df$name_status <- "accepted"
  # 1: insert names
  pgInsert(
    conn, taxon_names,
    df[!df$taxon_usage_id %in% t_names$taxon_usage_id, colnames(df) %in%
      c("taxon_usage_id", "usage_name", "author_name")]
  )
  # 2: insert concepts
  pgInsert(conn, taxon_relations, df[, colnames(df) %in% c(
    "taxon_concept_id",
    "parent_id", "rank", "view_key"
  )])
  # 3: insert names2concepts
  pgInsert(conn, names2concepts, df[, colnames(df) %in%
    c("taxon_usage_id", "taxon_concept_id", "name_status")])
  message("DONE!")
}

#' @rdname insert_concept
#'
#' @export
insert_concept_swea <- function(conn, ...) {
  UseMethod("insert_concept_swea", conn)
}


#' @rdname insert_concept
#' @aliases insert_concept_swea insert_concept_swea,PostgreSQLConnection-method
#'
#' @export
insert_concept_swea.PostgreSQLConnection <- function(conn,
                                                     taxon_names = c("tax_commons", "taxon_names"),
                                                     taxon_relations = c("swea_dataveg", "taxon_concepts"),
                                                     names2concepts = c("swea_dataveg", "names2concepts"),
                                                     taxon_views = c("bib_references", "main_table"),
                                                     taxon_levels = c("tax_commons", "bb_levels"),
                                                     df, ...) {
  insert_concept(
    conn, taxon_names, taxon_relations, names2concepts,
    taxon_views, taxon_levels, df, ...
  )
}

#' @rdname insert_concept
#'
#' @export
insert_concept_ecoveg <- function(conn, ...) {
  UseMethod("insert_concept_ecoveg", conn)
}


#' @rdname insert_concept
#' @aliases insert_concept_ecoveg
#'   insert_concept_ecoveg,PostgreSQLConnection-method
#'
#' @export
insert_concept_ecoveg.PostgreSQLConnection <- function(conn,
                                                       taxon_names = c("tax_commons", "ecoveg_f_names"),
                                                       taxon_relations = c("syntax_ecoveg_f", "taxon_concepts"),
                                                       names2concepts = c("syntax_ecoveg_f", "names2concepts"),
                                                       taxon_views = c("bib_references", "main_table"),
                                                       taxon_levels = c("tax_commons", "ecoveg_f_levels"),
                                                       df, ...) {
  insert_concept(
    conn, taxon_names, taxon_relations, names2concepts,
    taxon_views, taxon_levels, df, ...
  )
}
