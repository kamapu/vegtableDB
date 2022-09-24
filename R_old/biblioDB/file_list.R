#' @name file_list
#' 
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
#' The function `file_list()` produce this table, while the replace method
#' `file_list<-` will use such table to write the column 'file' in a
#' [lib_df-class] object.
#' 
#' @param x An object of class [lib_df-class], usually reference entries
#'     imported from a BibTeX databases by [read_bib()].
#' @param value A data frame listing files with respective bibtexkey and
#'     MIME-Type. In this table the columns 'bibtexkey', 'file', and 'mime' are
#'     mandatory. The occurrence of all bibtexkey values in 'value' will be
#'     cross-checked and eventually retrieve an error message.
#' @param priority Character value. A keyword used as file description to define
#'     which is the main document in the entry. Files including this description
#'     will be listed first in the output bibtex file (JabRef style).
#' @param ... Further parameters passed among methods.
#' 
#' @return
#' A data frame by `file_list()` or a [lib_df-class] object by `file_list<-`.
#' 
#' @export
#' 
file_list <- function (x, ...) {
	UseMethod("file_list", x)
}

#' @rdname file_list
#' 
#' @method file_list lib_df
#' @export 
#' 
file_list.lib_df <- function(x, ...) {
	if(!"bibtexkey" %in% colnames(x))
		stop("Column 'bibtexkey' is mandatory in 'x'.")
	if(!"file" %in% colnames(x)) {
		warning("Column 'file' is missing in 'x'.")
		return(NULL)
	} else {
		if(length(x$file[!is.na(x$file)]) == 0) {
			warning("No file information in 'x'.")
			return(NULL)
		} else {
			file_string <- strsplit(x$file, ";", fixed=TRUE)
			file_string <- data.frame(bibtexkey=rep(x$bibtexkey,
							sapply(file_string, length)),
					file=unlist(file_string), stringsAsFactors=FALSE)
			x <- do.call(rbind, strsplit(file_string$file, ":", fixed=TRUE))
			file_string$file <- x[,2]
			file_string$mime <- x[,3]
			file_string$description <- x[,1]
			file_string$description[file_string$description == ""] <- NA
			return(file_string[!is.na(file_string$file),])
		}
	}
}

#' @rdname file_list
#' 
#' @aliases file_list<-
#' 
#' @exportMethod file_list<-
#' 
setGeneric("file_list<-", function(x, ..., value)
			standardGeneric("file_list<-"))

#' @rdname file_list
#' 
#' @aliases file_list<-,lib_df,data.frame-method
#' 
#' @export 
#' 
setReplaceMethod("file_list", signature(x = "lib_df", value = "data.frame"),
		function(x, priority = "main text", ..., value) {
			if(nrow(value) == 0) {
				warning("No files to add in 'x'.")
				return(x)
			} else {
				if(!"bibtexkey" %in% colnames(x))
					stop("'bibtexkey' is a mandatory column in 'x'.")
				if(any(!c("bibtexkey", "file", "mime") %in% colnames(value)))
					stop(paste("'bibtexkey', 'file' and 'mime' are mandatory",
									"columns in 'value'."))
				if(any(!value$bibtexkey %in% x$bibtexkey))
					stop(paste("Some values of 'bibtexkey' in 'value'",
									"are not present in 'x'."))
				if(!"description" %in% colnames(x))
					x$description <- NA
				value <- value[order(value$description == priority,
								decreasing = TRUE),]
				value$description[is.na(value$description)] <- ""
				value$file <- with(value, paste(description, file, mime, sep = ":"))
				value <- split(value, value$bibtexkey)
				value <- do.call(rbind, lapply(value, function(x)
									c(x$bibtexkey[1], paste0(x$file,
													collapse = ";"))))
				x$file <- value[match(x$bibtexkey, value[ , 1]), 2]
				return(x)
			}
		})
