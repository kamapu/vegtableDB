#' @name get_description
#'
#' @title  Get descriptions of table columns from Postgres tables
#'
#' @description
#' Descriptions of variables stored in PostgreSQL tables.
#'
#' This function produces a data frame containing information on schemas,
#' tables and descriptions (i.e. comments) for every single column in tables
#' stored at a PostgreSQL database.
#'
#' @param conn A [PostgreSQLConnection-class] object.
#' @param ... Further arguments passed among methods.
#'
#' @return An object of class [data.frame].
#'
#' @author Miguel Alvarez \email{malvarez@@uni-bonn.de}
#'
#' @rdname get_description
#'
#' @export
get_description <- function(conn, ...) {
  UseMethod("get_description", conn)
}

#' @rdname get_description
#'
#' @aliases get_description,PostgreSQLConnection-method
get_description.PostgreSQLConnection <- function(conn, ...) {
  Query <- paste0(
    "SELECT c.table_schema,c.table_name,c.column_name,",
    "c.data_type,c.is_nullable,c.numeric_precision,",
    "c.numeric_scale,pgd.description\n",
    "FROM pg_catalog.pg_statio_all_tables as st\n",
    "INNER JOIN pg_catalog.pg_description pgd ON (pgd.objoid=st.relid)\n",
    "INNER JOIN information_schema.columns c ",
    "ON (pgd.objsubid=c.ordinal_position\n",
    "AND  c.table_schema=st.schemaname and c.table_name=st.relname);\n"
  )
  return(dbGetQuery(conn, Query, stringsAsFactors = FALSE))
}
