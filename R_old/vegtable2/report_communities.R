#' @name report_communities
#' 
#' @title Summary on plant communities stored in vegtable objects
#' 
#' @description 
#' This function produces a report on plant communities stored in an object of
#' class [vegtable-class].
#' The function is adapted to our own databases and may not work in other data
#' sets.
#' 
#' This function generates a quick overview of references and communities
#' stored in a data set formatted as [vegtable-class] object.
#' 
#' @param veg An object of class [vegtable-class].
#' @param bib Name of bibliography file (.bib) as chararcter value.
#' @param filename A character value indicating the name of the resulting file
#'     without extension.
#' @param title,author Character values used in the title page.
#' @param date A value of class \code{\link{Date}} to be inserted in the title
#'     page.
#' @param date_format Character value indicating the format used for displaying
#'     the date in the title page.
#' @param papersize Character value indicating the paper format.
#' @param keep_rmd Logical value, whether the resulting Rmd file should be
#'     deleted after rendering or not.
#' @param biblio_style Character value indicating the name of the bibliography
#'     style in the package 'natbib'.
#' @param ... Further arguments passed among methods.
#' 
#' @return A PDF file.
#' 
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#' 
#' @rdname report_communities
#' 
#' @exportMethod report_communities
#' 
setGeneric("report_communities",
		function(veg, ...)
			standardGeneric("report_communities")
)

#' @rdname report_communities
#' 
#' @aliases report_communities,vegtable-method
#' 
setMethod("report_communities", signature(veg="vegtable"),
		function(veg, bib, filename, title, author, date,
				date_format="%d-%m-%Y", papersize="a4", keep_rmd=TRUE,
				biblio_style="abbrvnat", ...) {
			filename <- paste0(filename, ".Rmd")
			if(!missing(date))
				if(class(date) != "Date")
					stop("Argument 'date' has to be of class 'Date'")
			if(missing(date))
				date <- Sys.Date()
			# Structure Head of text
			Head <- paste0(
					"---\n",
					paste0("title: \"", title, "\"\n"),
					paste0("author: \"", author, "\"\n"),
					paste0("date: \"", format(date, date_format), "\"\n"),
					paste0("header-includes:\n",
							"  - \\usepackage[utf8]{inputenc}\n",
							"  - \\usepackage[T1]{fontenc}\n",
							"  - \\usepackage{bibentry}\n",
							"  - \\usepackage{hyperref}\n"),
					paste0("  - \\nobibliography*\n"),
					"output:\n  pdf_document:\n    citation_package: natbib\n",
					paste0("biblio-style: ", biblio_style, "\n"),
					paste0("bibliography: ", bib, "\n"),
					paste0("papersize: ", papersize, "\n"),
					"---\n",
					"\n")
			# Structure the content
			comm <- aggregate(ReleveID ~ data_source + community_type,
					veg@header, length)
			comm$community_name <- with(veg@relations$community_type,
					community_name[match(comm$community_type, community_type)])
			comm$bibtexkey <- with(veg@relations$data_source,
					bibtexkey[match(comm$data_source, data_source)])
			comm <- clean_strings(comm)
			comm <- split(comm, comm$bibtexkey)
			Content <- list()
			for(i in names(comm)) {
				Content[[i]] <- paste0("**[", i, "]** \\bibentry{", i, "}\n")
				for(j in 1:nrow(comm[[i]]))
					Content[[i]][j + 1] <- paste0("**",
							comm[[i]][j,"community_type"], ". ",
							comm[[i]][j,"community_name"],
							"** \\dotfill ", comm[[i]][j,"ReleveID"], "\n")
				Content[[i]][length(Content[[i]]) + 1] <- "\\vspace{5mm}"
			}
			## write(c(Head, unlist(Content)), filename)
			con <- file(filename, "wb")
			writeBin(charToRaw(do.call(paste0, list(c(Head, unlist(Content),
													"\\pagebreak"),
											collapse="\n"))),
					con, endian="little")
			close(con)
			## compile with rmarkdown
			render(filename, encoding="UTF-8")
			if(!keep_rmd) unlink(filename)
		}
)
