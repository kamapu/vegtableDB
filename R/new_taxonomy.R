#' @name new_taxonomy
#' @rdname new_taxonomy
#'
#' @title Create new database from a taxlist object
#'
#' @description
#' Create a new schema in a connected database containing a taxonomic list from
#' a taxlist object.
#'
#' If 'obj' is missing an empty database will be created. You can than use
#' [append_taxonomy()] to insert a taxonomy in the empty database.
#'
#' @param conn A [RPostgreSQL::PostgreSQLConnection-class] connecting to a
#'     database, where the new list will be stored.
#' @param obj A [taxlist::taxlist-class] object containing the new database.
#' @param taxonomy A character value indicating the name (ID) of the new
#'     taxonomy in the database.
#' @param schema A character value with the name of the new schema containing
#'     the taxonomic list.
#' @param schema_refs A character value with the name of the new schema
#'     containing the references used as taxon views. It is recommended to have
#'     a separated schema for the references, especially if the views are
#'     formated as a BibTeX database ([biblio::lib_df-class] object).
#' @param add_attributes A data frame used to add further columns into the table
#'     **taxon_attributes** in the database. The data frame has three mandatory
#'     columns, **name**, **type** (type of variable, which may include
#'     constraints) and **comment**. See [divDB::add_columns()] for more
#'     details.
#' @param add_to_names A data frame used to add further columns into the table
#'     **taxon_names** in the database. See details for argument
#'     `'add_attributes'` and in [divDB::add_columns()].
#' @param ... Further arguments passed among methods (not in use).
#'
#' @exportMethod new_taxonomy
setGeneric(
  "new_taxonomy",
  function(conn, obj, ...) {
    standardGeneric("new_taxonomy")
  }
)

#' @rdname new_taxonomy
#' @aliases new_taxonomy,PostgreSQLConnection,missing-method
setMethod("new_taxonomy", signature(
  conn = "PostgreSQLConnection",
  obj = "missing"
), function(conn, taxonomy, schema, schema_refs, add_attributes,
            add_to_names, ...) {
  # Check for existing database
  if (dbExistsTable(conn, c(schema, "taxon_concepts"))) {
    stop("Database already exists. Use 'append_taxonomy()' instead.")
  }
  # Add references
  if (dbExistsTable(conn, c(schema_refs, "main_table"))) {
    message("Existing schema with bibliographic references will be used.")
  } else {
    bib2database(conn = conn, schema = schema_refs)
  }
  # Create empty schema
  dbSendQuery(conn, paste0("create schema if not exists \"", schema, "\""))
  # Create tables
  query <- gsub("<schema>", schema, new_taxonomy_sql)
  query <- gsub("<schema_bib_references>", schema_refs, query)
  dbSendQuery(conn, query)
  # Add additional columns
  if (!missing(add_attributes)) {
    add_columns(conn, df = add_attributes, name = c(
      schema,
      "taxon_attributes"
    ))
  }
  if (!missing(add_to_names)) {
    add_columns(conn, df = add_to_names, name = c(
      schema,
      "taxon_names"
    ))
  }
  message("DONE!")
})

#' @rdname new_taxonomy
#' @aliases new_taxonomy,PostgreSQLConnection,taxlist-method
setMethod("new_taxonomy", signature(
  conn = "PostgreSQLConnection",
  obj = "taxlist"
), function(conn, obj, ...) {
  # Creates empty database
  new_taxonomy(conn, ...)
  # Append data
  append_taxonomy(conn, obj = obj, ...)
})
