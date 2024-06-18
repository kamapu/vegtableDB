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
#' @param clean A logical value indicating cleaning of characters.
#' @param eval A logical value indicating whether the produced SQL commands
#'     should be sent to the database or not.
#' @param update A logical value indicating whether attributes of existing names
#'     (mentioned as recycled) should be updated according to the input data
#'     frame or not.
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
  function(conn, df, schema, clean = TRUE, eval = TRUE, update = FALSE, ...) {
    if (!dbExistsTable(conn, c(schema, "taxon_names"))) {
      stop("The input schema does not contain a table 'taxon_names'")
    }
    if (clean) {
      df <- clean_strings(df)
    }
    df_cols <- c("usage_name", "author_name")
    df_cols <- df_cols[!df_cols %in% names(df)]
    if (length(df_cols)) {
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
    full_names <- with(df, paste(usage_name, author_name))
    in_db <- full_names %in%
      with(db_names, paste(usage_name, author_name))
    if (sum(in_db)) {
      message(paste0(
        "Following names will be recycled:\n    ",
        paste0(full_names[in_db], collapse = "\n    ")
      ))
    }
    # split table
    df_recycle <- df[in_db, ]
    df <- df[!in_db, ]
    # empty query
    query <- character(0)
    # update existing names
    if (update) {
      query <- c(query, update_rows(conn, df_recycle,
        name = c(schema, "taxon_names"),
        key = c("usage_name", "author_name"),
        eval = FALSE
      ))
    }
    # assign new ids
    usage_id <- unlist(dbGetQuery(conn, paste(
      "select taxon_usage_id",
      "from plant_taxonomy.taxon_names"
    )))
    df$taxon_usage_id <- id_solver(c(1:nrow(df)), usage_id)
    # retrieve insert query for new names
    query <- c(query, insert_rows(conn, df,
      name = c(schema, "taxon_names"),
      eval = FALSE
    ))
    # convert to class sql
    class(query) <- c("sql", "character")
    # Run query, if requested
    if (eval) {
      dbSendQuery(conn, query)
      message("DONE!")
    }
    # Return sql invisible
    invisible(query)
  }
)

#' @rdname insert_names
#' @aliases insert_names,PostgreSQL,data.frame,missing-method
setMethod(
  "insert_names", signature(
    conn = "PostgreSQLConnection",
    df = "data.frame",
    schema = "missing"
  ),
  function(conn, df, schema = "plant_taxonomy", ...) {
    insert_names(conn = conn, df = df, schema = schema, ...)
  }
)
