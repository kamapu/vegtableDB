#' @name release
#'
#' @rdname release
#'
#' @title Release a table for an herbarium
#'
#' @description
#' Processing data frame obtained by [read_spec()] to be released at a
#' respective herbarium.
#'
#' Some herbaria have a template for storage of data.
#'
#' @param x A [specimens-class] object retrieved by [read_spec()].
#' @param herb A character value containig the code of the herbarium for the
#'     release.
#' @param trans A list containing translations of variable names to the
#'     requested template. The name of the elements represent an herbarium
#'     (usually as code by the Index Herbariorum) and contain a database with
#'     two columns: **in** for the column name of the input object (after
#'     applying [as_data.frame()]), and the column **out** with the names
#'     respective column names in the output. For columns in the requested
#'     output without representative in the [specimens-class] object, you may
#'     indicate it with `NA` values at **in**. If not provided, this function
#'     will use a pre installed translator (check with
#'     `specimensDB:::translator`).
#' @param ... Further arguments passed among methods (not in use).
#'
#' @return
#' A data frame with columns sorted and named for release at a respective
#' herbarium.
#'
#' @export
release <- function(x, ...) {
  UseMethod("release", x)
}

#' @rdname release
#'
#' @aliases release,specimens-method
#'
#' @method release specimens
#'
#' @export
release.specimens <- function(x, herb, trans, ...) {
  if (missing(trans)) {
    trans <- translator
  }
  if (!herb[1] %in% names(trans)) {
    stop(paste0(
      "The herbarium '", herb[1],
      "' is not in the installed catalog."
    ))
  }
  x <- as_data.frame(x)
  names(x) <- with(
    trans[[herb]][!is.na(trans[[herb]]$"in"), ],
    replace_x(names(x), get("in"), get("out"))
  )
  for (i in trans[[herb]][is.na(trans[[herb]][, "in"]), "out"]) {
    x[[i]] <- rep(NA, length(x[[1]]))
  }
  no_name <- trans[[herb]][, "out"][!trans[[herb]][, "out"] %in%
    names(x)]
  if (length(no_name) > 0) {
    stop(paste0(
      "Following variables in 'x' may have wrong translation values: '",
      paste0(no_name, collapse = "','"), "'."
    ))
  }
  x <- x[, trans[[herb]][, "out"]]
  class(x) <- "data.frame"
  return(x)
}
