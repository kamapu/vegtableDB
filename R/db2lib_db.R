#' @name db2lib_db
#'
#' @rdname db2lib_db
#'
#' @title Read bibliographic databases from PostgreSQL
#'
#' @description
#' Databases tabulated in PostgreSQL will be imported into a [lib_df-class]
#' object.
#'
#' @param conn A [PostgreSQLConnection-class] retrieved either by [dbConnect()]
#'     or [connect_db()].
#' @param name A character value with the name of the schema containing the
#'     main table and the file list. If the length is 2, the first value will be
#'     assumed as the schema for the main table and the second, for the file
#'     list. If the length is 1, the value will be recycled.
#' @param main_table A character value indicating the name of the main table in
#'     in the schema 'name'.
#' @param file_list A character value indicating the name of the table in schema
#'     'name', which contains the names of the files and the respective
#'     attributes. It can be cancelled by `'file_list = NULL'`.
#' @param file_folder A character value showing the path to the linked documents
#'     (i.e. PDF files of publications).
#' @param simplify Logical value indicating whether empty columns should be
#'     skipped from output or not.
#' @param ... Further arguments passed among methods.
#'
#' @return A [lib_db-class] object.
#'
#' @export
db2lib_db <- function(conn, ...) {
  UseMethod("db2lib_db", conn)
}

#' @rdname db2lib_db
#' @method db2lib_db PostgreSQLConnection
#' @export
db2lib_db.PostgreSQLConnection <- function(conn, name,
                                           main_table = "main_table", file_list = "file_list",
                                           file_folder, simplify = FALSE, ...) {
  if (!missing(name)) {
    name <- rep_len(name, 2)
    main_table <- c(name[1], main_table)
    if (!is.null(file_list)) {
      file_list <- c(name[2], file_list)
    }
  }
  Refs <- new("lib_db")
  Refs@dir$connection <- conn
  if (!missing(file_folder)) {
    Refs@dir$folder <- file_folder
  }
  Refs@main_table <- {
    mt <- dbReadTable(conn, main_table)
    if (simplify) {
      mt <- mt[, apply(mt, 2, function(x) !all(is.na(x)))]
    }
    class(mt) <- c("lib_df", "data.frame")
    mt
  }
  if (!is.null(file_list)) {
    Refs@file_list <- dbReadTable(conn, file_list)
  }
  return(Refs)
}
