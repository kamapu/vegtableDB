#' @name insert_names
#' @rdname insert_names
#'
#' @title Insert new taxonomic names in database
#'
#' @description
#' Different routines may require to compare a list of new names with existing
#' database.
#' Names that are already in the database will be maintained and only new names
#' will get inserted in the database.
#'
#' For existing names, if further information is provided, this will be updated
#' in the database.
#'
#' @param conn A [PostgreSQLConnection-class] object connecting to a target
#'     database.
#' @param df A data frame with the list of names that will be inserted to the
#'     database. Two columns are mandatory in this data frame, namely
#'     **usage_name** and **author_name**.
#' @param schema A character value indicating the name of the schema containing
#'     the taxonomic list. If the table **taxon_names** does not exists in this
#'     schema, this function retrieves an error message.
#' @param ... Further arguments passed among methods (not in use).
#'
#' @exportMethod insert_names
setGeneric("insert_names", function(conn, df, schema, ...) {
  standardGeneric("insert_names")
})

#' @rdname insert_names
#' @aliases insert_names,PostgreSQL,data.frame,character-method
setMethod(
  "insert_names", signature(
    conn = "PostgreSQLConnection",
    df = "data.frame", schema = "character"
  ),
  function(conn, df, schema, ...) {
    if (!dbExistsTable(conn, c(schema, "taxon_names"))) {
      stop("The input schema does not contain a table 'taxon_names'")
    }
    df_cols <- c("usage_name", "author_name")
    df_cols <- df_cols[!df_cols %in% names(df)]
    if (length(df_cols) > 0) {
      stop(paste0(
        "Following mandatory columns are missing in 'df': '",
        paste0(df_cols, collapse = "', '"), "'."
      ))
    }
    # Check names in db
    db_names <- dbGetQuery(conn, paste(
      "select taxon_usage_id,usage_name,author_name",
      paste0("from \"", schema, "\".taxon_names")
    ))
    in_db <- with(df, paste(usage_name, author_name)) %in%
      with(db_names, paste(usage_name, author_name))
    message(paste0(
      sum(in_db), " names will be recycled\n", sum(!in_db),
      " names will be inserted in the database."
    ))
    # Get column names
    tn_col_names <- unlist(dbGetQuery(conn, paste(
      "select column_name",
      "from information_schema.columns",
      paste0("where table_schema = '", schema, "'"),
      "and table_name = 'taxon_names'"
    )))
    df <- split(df, in_db)
    un_id <- unlist(dbGetQuery(conn, paste(
      "select max(taxon_usage_id)",
      paste0("from \"", schema, "\".taxon_names")
    )))
    if (is.na(un_id)) un_id <- 0
    df$"FALSE"$taxon_usage_id <- un_id + seq_along(df$"FALSE"$usage_name)
    dbWriteTable(conn, c(schema, "taxon_names"),
      df$"FALSE"[, names(df$"FALSE") %in% tn_col_names],
      append = TRUE, row.names = FALSE
    )
    # Update existing names
    tn_col_names <- colnames(df$"TRUE")[colnames(df$"TRUE") %in% tn_col_names]
    tn_col_names <- tn_col_names[!tn_col_names %in%
      c("taxon_usage_id", "usage_name", "author_name")]
    if (length(tn_col_names) > 0) {
      db_names <- dbGetQuery(conn, paste(
        "select taxon_usage_id,usage_name,author_name",
        paste0("from \"", schema, "\".taxon_names")
      ))
      up_names <- df$"TRUE"
      up_names$taxon_usage_id <- db_names$taxon_usage_id[match(
        with(up_names, paste(usage_name, author_name)),
        with(db_names, paste(usage_name, author_name))
      )]
      up_names <- up_names[, colnames(up_names) %in% c(
        "taxon_usage_id",
        tn_col_names
      )]
      suppressWarnings(
        update_data(conn, up_names, "taxon_usage_id", c(schema, "taxon_names"),
          update = TRUE
        )
      )
    } else {
      message("DONE!")
    }
  }
)
