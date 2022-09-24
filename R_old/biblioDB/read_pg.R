#' @name read_pg
#' 
#' @rdname read_pg
#' 
#' @title Read bibliographic databases from PostgreSQL
#' 
#' @description 
#' Databases tabulated in PostgreSQL will be imported into a [lib_df-class]
#' object.
#' 
#' @param conn A database connection established with [dbConnect()] or
#'     [dbaccess::connect_db2()].
#' @param name Name of the database corresponding to a schema in the PostgreSQL
#'     database.
#' @param main_table A character value indicating the name of the main table in
#'     in the schema 'name'.
#' @param file_list A character value indicating the name of the table in schema
#'     'name', which contains the names of the files and the respective
#'     attributes.
#' @param add_files Logical value indicating whether information in table
#'     'file_list' should be appended to the output or not.
#' @param simplify Logical value indicating whether empty columns should be
#'     skipped from output or not.
#' @param ... Further arguments passed among methods.
#' 
#' @return A [lib_df-class] object.
#' 
#' @export
#' 
read_pg <- function (conn, ...) {
	UseMethod("read_pg", conn)
}

#' @rdname read_pg
#' 
#' @method read_pg PostgreSQLConnection
#' @export
#' 
read_pg.PostgreSQLConnection <- function(conn, name, main_table = "main_table",
		file_list = "file_list", add_files = TRUE, simplify = FALSE, ...) {
	Refs <- dbReadTable(conn, c(name, main_table))
	if(simplify)
		Refs <- Refs[ , apply(Refs, 2, function(x) !all(is.na(x)))]
	class(Refs) <- c("lib_df", "data.frame")
	if(add_files) {
		Files <- dbReadTable(conn, c(name, file_list))
		file_list(Refs) <- Files
	}
	return(Refs)
}
