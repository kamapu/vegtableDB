#' @name check_files
#' 
#' @title Check for file existence
#' 
#' @description
#' Documents and other associated files are assumed in JabRef to exist in a
#' single folder (here indicated by 'path') and this function cross-check their
#' occurrence.
#' 
#' @param x Either a [lib_df-class] object or a [PostgreSQLConnection-class]
#'     including a variable listing associated files.
#' @param path A character values with the path of the folder containing the
#'     files, usually the documents as PDF files.
#' @param ... Further arguments passed to [list.files()].
#' @param name Character value indicating the name of the schema in the
#'     PostgreSQL database. It is passed to [read_pg()].
#' @param db_args A list with named arguments passed to [read_pg()].
#' 
#' @example 
#' check_files(x = openaccess, path = path.package("biblioDB"), pattern = "pdf")
#' 
#' @rdname check_files
#' 
#' @export
#' 
check_files <- function (x, ...) {
	UseMethod("check_files", x)
}

#' @rdname check_files
#' 
#' @method check_files lib_df
#' @export
#' 
check_files.lib_df <- function(x, path, ...) {
	path <- list.files(path, ...)
	x <- file_list(x)$file
	# List files
	not_in_db <- path[!path %in% x]
	not_in_folder <- x[!x %in% path]
	if(length(not_in_db) > 0)
		cat(paste0("## Files not included in database:\n   '",
						paste0(not_in_db, collapse = "' '"), "'\n"))
	if(length(not_in_folder) > 0)
		cat(paste0("## Files missing in the local folder:\n   '",
						paste0(not_in_folder, collapse = "' '"), "'\n"))
	if(length(not_in_db) == 0 & length(not_in_folder) == 0)
		cat("## Everything OK!\n")
	invisible(list(not_in_db = not_in_db, not_in_folder = not_in_folder))
}

#' @rdname check_files
#' 
#' @method check_files PostgreSQLConnection
#' @export
#' 
check_files.PostgreSQLConnection <- function(x, path, name, db_args = list(),
		...) {
	x <- do.call(read_pg, c(list(conn = x, name = name), db_args))
	check_files(x = x, path = path, ...)
}
