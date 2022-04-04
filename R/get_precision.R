#' @name get_precision
#'
#' @title Get length and precision for numeric variables
#'
#' @description
#' This function can be used for define data types in databases according to
#' vector properties (length and precision).
#'
#' Get precision to define numeric characters in databases (e.g.
#' `NUMERIC(5,3)`).
#'
#' @param x A numeric vector.
#'
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#'
#' @examples
#' data(iris)
#' get_precision(iris$Sepal.Length)
#' @export get_precision
#'
get_precision <- function(x) {
  LEN <- max(nchar(sub(".", "", paste(x), fixed = TRUE)))
  W <- paste(as.integer(x))
  W[W == "NA"] <- NA
  W <- max(nchar(W), na.rm = TRUE)
  PRES <- LEN - W
  cat("length:", LEN, "\n")
  cat("precision:", PRES, "\n")
}
