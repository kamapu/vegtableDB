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
#' @aliases compare_df,PostgreSQLConnection,lib_df,missing-method
#' 
setMethod("compare_df", signature(x = "PostgreSQLConnection", y = "lib_df",
				key = "missing"),
		function(x, y, name, ...) {
			x <- read_pg(conn = x, name = name, ...)
			# Extract file table
			x_files <- file_list(x = x)
			x <- x[ , colnames(x) != "file"]
			y_files <- file_list(x = y)
			y <- y[ , colnames(y) != "file"]
			# Do comparisons
			OUT <- list()
			OUT$main_table <- compare_df(x = x, y = y)
			OUT$file_list <- compare_df(x = x_files, y = y_files, key = "file")
			class(OUT) <- c("comp_df2", "list")
			return(OUT)
		})
