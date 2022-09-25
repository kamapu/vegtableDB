#' @name lib_db-class
#' @aliases lib_db
#'
#' @title Class lib_db.
#'
#' @description
#' A class created as intermediate format between [lib_df-class] objects and a
#' database.
#'
#' @slot dir A list including the folder containing files and a connection to
#'     a database.
#' @slot main_table A [lib_df-class] object.
#' @slot file_list A data frame with a list of files associated to a library
#'     entry.
#' @slot relations A list with data frames related to single columns in
#'     main_table.
#'
#' @examples
#' showClass("lib_db")
#'
#' @exportClass lib_db
setClass("lib_db",
  slots = c(
    "dir",
    "main_table",
    "file_list",
    "relations"
  ),
  prototype = list(
    dir = list(
      folder = character(0),
      connection = NA
    ),
    main_table = {
      mt <- data.frame(bibtexkey = character(0))
      class(mt) <- c("lib_df", "data.frame")
      mt
    },
    file_list = data.frame(
      file = character(0),
      bibtexkey = character(0)
    ),
    relations = list()
  ),
  validity = function(object) {
    # Mandatory names in dir
    d_names <- c("folder", "connection")
    if (!all(d_names %in% names(object@dir))) {
      d_names <- d_names[!d_names %in% names(object@dir)]
      return(paste0(
        "Following mandatory elements are missing in slot ",
        "'dir': ", paste0(d_names, collapse = ", "), "."
      ))
    }
    # Mandatory names in main_table
    if (!"bibtexkey" %in% names(object@main_table)) {
      return("Mandatory column 'bibtexkey' is missing in 'main_table'.")
    }
    # Mandatory names in file_list
    d_names <- c("file", "bibtexkey")
    if (!all(d_names %in% names(object@file_list))) {
      d_names <- d_names[!d_names %in% names(object@file_list)]
      return(paste0(
        "Following mandatory elements are missing in slot ",
        "'file_list': ", paste0(d_names, collapse = ", "), "."
      ))
    }
    # Duplicated IDs in main_table
    if (any(duplicated(object@main_table$bibtexkey))) {
      return(paste(
        "Duplicated values of 'bibtexkey' in 'main_table'",
        "are not allowed."
      ))
    }
    # Duplicated IDs in file_list
    if (any(duplicated(object@file_list$file))) {
      return(paste(
        "Duplicated values of 'file' in 'file_list'",
        "are not allowed."
      ))
    }
    # Relations between file_list and main_table
    if (nrow(object@file_list) > 0) {
      file_rel <- unique(object@file_list$bibtexkey)
      file_rel <- file_rel[!file_rel %in% object@main_table$bibtexkey]
      if (length(file_rel) > 0) {
        return(paste0(
          "Following values of 'bibtexkey' in 'file_list' ",
          "are missing in 'main_table': ",
          paste0(file_rel, collapse = ", "), "."
        ))
      }
    }
  }
)
