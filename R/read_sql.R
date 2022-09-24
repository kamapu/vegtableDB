#' @name read_sql
#'
#' @title Read SQL scripts and split it in SQL statements
#'
#' @details
#' SQL scripts may mix SQL statements with comments. This function extract the
#' respective statements and split them into a list, where each element is a
#' statement.
#'
#' @param file A character value indicating the path to the script that have to
#'     be read.
#' @param end Symbol set at the end of a statement (semicolon by default).
#' @param comment Symbol used to start a comment in script (two dashes by
#'     default).
#' @param ... Further arguments passed to [readLines()].
#'
#' @author Miguel Alvarez
#'
#' @export read_sql
#'
read_sql <- function(file, end = ";", comment = "--", ...) {
  Query <- readLines(file, ...)
  Query <- Query[!(nchar(Query) == 0 | grepl(comment, Query, fixed = TRUE))]
  Query <- paste0(Query, "\n")
  idx <- grepl(end, Query, fixed = TRUE)
  idx <- c(0, cumsum(idx[-length(idx)]))
  Query <- split(Query, idx)
  names(Query) <- NULL
  Query <- sapply(Query, function(x) paste0(x, collapse = ""))
  Query <- structure(Query, class = c("sql", "character"))
  return(Query)
}
