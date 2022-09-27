#' @name print-methods
#' @docType methods
#' @rdname print
#'
#' @title Print comparisons of multiple data frames
#'
#' @description
#' Comparing multiple pairs of data frames or [lib_df-class] objects can be
#' assigned to a single list setting as class
#' `class(x) <- c("comp_df2", "list")`.
#'
#' @param x An object of class [comp_df2-class].
#' @param ... Further arguments passed among methods.
#'
#' @method print comp_db
#' @export
print.comp_db <- function(x, ...) {
  for (i in names(x)) {
    lab_i <- paste0("# Changes in '", i, "' #")
    bar_i <- paste0(rep("#", times = nchar(lab_i)), collapse = "")
    cat(paste0(bar_i, "\n", lab_i, "\n", bar_i, "\n\n"))
    print(x[[i]])
  }
  cat("\n")
}
