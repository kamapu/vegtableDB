#' @name compare_df
#'
#' @rdname compare_df
#'
#' @title Comparison of data frames extended to electronic libraries
#'
#' @description
#' Extension of comparisons of data frames applied to relational models.
#'
#' @param x The reference data frame.
#' @param y The updated data frame.
#' @param name Character value indicating the name of the schema in the
#'     PostgreSQL database.
#' @param ... Further arguments passed to [read_pg()].
#'
#' @aliases compare_df,lib_db,lib_db,missing-method
setMethod(
  "compare_df", signature(
    x = "lib_db", y = "lib_db",
    key = "missing"
  ),
  function(x, y, name, ...) {
    OUT <- list()
    OUT$main_table <- compare_df(x = x@main_table, y = y@main_table)
    OUT$file_list <- compare_df(
      x = x@file_list, y = y@file_list,
      key = "file"
    )
    class(OUT) <- c("comp_db", "list")
    return(OUT)
  }
)
