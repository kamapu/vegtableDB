#' @name file_list
#' @rdname file_list
#'
#' @title Description of files associated to entries in BibTeX databases
#'
#' @description
#' Main documents as well as supplementary material may be linked to the
#' respective bibliographic entries in a BibTeX database. File names, MIME-types
#' and descriptions are stored in a single column by JabRef, separating columns
#' by colons and multiple files by semicolons. In a relational database, file
#' information may be stored in a separated table and linked by bibtexkeys.
#'
#' @param x Either objects of class [lib_df-class], [lib_db-class],
#'     or a data.frame, depending on function and method.
#'     The function `file_list()` applied to a [lib_df-class] object retrieves
#'     a data frame with the details of linked documents. The function
#'     `file_list2string()` will do the back conversion from a data frame to
#'     the string required by **JabRef**.
#' @param ... Further parameters passed among methods.
#'
#' @return
#' For `file_list()` applied to a [lib_df-class] object, a data frame.
#' For `file_list2string()` applied to a data frame, a data frame with the
#' columns **bibtexkey** (the reference identifier) and **file** (the respective
#' string with linked files).
#'
#' @export
file_list <- function(x, ...) {
  UseMethod("file_list", x)
}

#' @rdname file_list
#' @aliases file_list,lib_df-method
#' @method file_list lib_df
#' @export
file_list.lib_df <- function(x, ...) {
  if (!"bibtexkey" %in% colnames(x)) {
    stop("Column 'bibtexkey' is mandatory in 'x'.")
  }
  if (!"file" %in% colnames(x)) {
    warning("Column 'file' is missing in 'x'.")
    return(NULL)
  } else {
    if (length(x$file[!is.na(x$file)]) == 0) {
      warning("No file information in 'x'.")
      return(NULL)
    } else {
      file_string <- strsplit(x$file, ";", fixed = TRUE)
      file_string <- data.frame(
        bibtexkey = rep(
          x$bibtexkey,
          sapply(file_string, length)
        ),
        file = unlist(file_string), stringsAsFactors = FALSE
      )
      file_string <- file_string[!is.na(file_string$file), ]
      file_string$file <- paste(file_string$bibtexkey, file_string$file,
        sep = ":"
      )
      file_string <- strsplit(file_string$file, ":", fixed = TRUE)
      file_string <- lapply(file_string, function(x) {
        if (length(x) == 3) {
          x <- c(x, NA)
        }
        return(x)
      })
      x <- as.data.frame(do.call(rbind, file_string))
      names(x) <- c("bibtexkey", "description", "file", "mime")
      for (i in c("description", "mime")) {
        x[x[, i] == "", i] <- NA
      }
      return(x[, c("bibtexkey", "file", "mime", "description")])
    }
  }
}

#' @rdname file_list
#' @export
file_list2string <- function(x, ...) {
  UseMethod("file_list2string", x)
}

#' @rdname file_list
#' @aliases file_list2string file_list2string,data.frame-method
#' @method file_list2string data.frame
#' @export
file_list2string.data.frame <- function(x, ...) {
  t_names <- c("bibtexkey", "file", "mime", "description")
  if (!all(t_names %in% names(x))) {
    t_names <- t_names[!t_names %in% names(x)]
    stop(paste0(
      "Following mandatory columns are missing in 'x': '",
      paste0(t_names, collapse = "', '"), "'."
    ))
  }
  # Replace NA characters
  for (i in names(x)) {
    x[is.na(x[, i]), i] <- ""
  }
  x$file <- paste(x$description, x$file, x$mime, sep = ":")
  x <- aggregate(file ~ bibtexkey,
    data = x,
    FUN = function(x) paste0(x, collapse = ";")
  )
  return(x)
}

#' @rdname file_list
#' @aliases file_list2string file_list2string,lib_db-method
#' @method file_list2string lib_db
#' @export
file_list2string.lib_db <- function(x, ...) {
  flist <- file_list2string(x@file_list)
  x@main_table$file <- flist$file[match(
    x@main_table$bibtexkey,
    flist$bibtexkey
  )]
  return(x)
}
