#' @name get_description
#' 
#' @title Produce a table of all variables from a database
#' 
#' @description 
#' Overview of all variables stored in a PostgreSQL database including the
#' respective schema and table as well as the descriptions stored as comments.
#' 
#' @param conn A database connection provided by [DBI::dbConnect()].
#' @param ... Further arguments passed to [dbGetQuery()].
#' 
#' @return A data frame.
#' 
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#' 
#' @rdname get_description
#' 
#' @export
get_description <- function (conn, ...) {
	UseMethod("get_description", conn)
}


#' @rdname get_description
#'
#' @aliases get_description,PostgreSQLConnection-method
#' 
#' @export
get_description.PostgreSQLConnection <- function(conn, ...) {
	Query <- paste0("SELECT c.table_schema,c.table_name,c.column_name,",
			"c.data_type,c.is_nullable,c.numeric_precision,c.numeric_scale,",
			"pgd.description\n", 
			"FROM pg_catalog.pg_statio_all_tables as st\n", 
			"INNER JOIN pg_catalog.pg_description pgd ",
			"ON (pgd.objoid=st.relid)\n",
			"INNER JOIN information_schema.columns c ", 
			"ON (pgd.objsubid=c.ordinal_position ",
			"AND c.table_schema=st.schemaname and c.table_name=st.relname);\n")
	OUT <- dbGetQuery(conn, Query, ...)
	colnames(OUT)[c(1, 2, 3, 6, 7)] <- c("schema", "table", "column", "length",
			"precision")
	return(OUT)
}
