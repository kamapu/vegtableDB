#' @name render_desc
#'
#' @title Rendering bulk description.
#'
#' @description
#' Producing a PDF file with the description of the bulk (project) using
#' [write_rmd()] and [render_rmd()].
#'
#' @param db Connection to the database as [PostgreSQLConnection-class].
#' @param bulk The identifies of the bulk (project) in the database.
#' @param output Character value or list with the output settings for the yaml
#'     head. This is passed to [write_rmd()].
#' @param output_file Character value with the name and path to the ouput file.
#'     I is passed to [render_rmd()].
#' @param ... Further Arguments passed to [write_rmd()].
#'
#' @author Miguel Alvarez \email{kamapu@@posteo.com}
#'
#' @rdname render_desc
#'
#' @export
render_desc <- function(db, ...) {
  UseMethod("render_desc", db)
}

#' @rdname render_desc
#' @aliases render_desc,PostgreSQLConnection-method
#' @method render_desc PostgreSQLConnection
#' @export
render_desc.PostgreSQLConnection <- function(db, bulk, output = "pdf_document",
                                             output_file, ...) {
  query <- paste(
    "select project_name,description", "from specimens.projects",
    paste("where bulk =", bulk[1])
  )
  Descr <- dbGetQuery(db, query)
  if (nrow(Descr) == 0) {
    stop("Requested 'bulk' does not exist in the database.")
  }
  Descr <- write_rmd(
    title = Descr$project_name, output = output,
    body = Descr$description, ...
  )
  render_rmd(Descr, output_file = output_file)
}
