#' @name sql-class
#' @title A class for lists of SQL commands
#' @description
#' Multiple SQL commands can be read from a *.sql file by [read_sql()] and
#' stored as a character vector.
#' @exportClass sql
setOldClass(c("sql", "character"))
