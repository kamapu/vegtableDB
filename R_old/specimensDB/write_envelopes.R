#' @name write_envelopes
#'
#' @rdname write_envelopes
#'
#' @title Writting labels for envelopes
#'
#' @description
#' Similarly as [write_labels()], placing the label in a position ideal to fold
#' the sheet as an envelope.
#' This may be usually done for collections of Bryophytes.
#'
#' @param x A [specimens-class] object retrieved by [read_spec()].
#' @param output_file A character value indicating the name of the file. It may
#'     include a relative path. This is passed to the homonymous parameter at
#'     [render_rmd()].
#' @param date_format A character value indicating the format used for the
#'     collection date. It is passed to the function [format.Date()].
#' @param mar A named vector indicating the margins. The mandatory names are
#'     **l** (left), **r** (right), **t** (top), **b** (bottom), and
#'     **u** (units).
#' @param frame A logical value indicating whether a frame around the label
#'     should be drawn or not.
#' @param classoption A character value to be inserted as 'classoption' in the
#'     yaml head for the Rmarkdown document.
#' @param ... Further arguments passed to [write_rmd()]. It works only if
#'     `'merge = TRUE'`.
#'
#' @return
#' An invisible [rmd_doc-class] object.
#'
#' @export
write_envelopes <- function(x, ...) {
  UseMethod("write_envelopes", x)
}

#' @rdname write_envelopes
#'
#' @aliases write_envelopes,specimens-method
#'
#' @method write_envelopes specimens
#'
#' @export
write_envelopes.specimens <- function(x, output_file,
                                      date_format = "%d.%m.%Y",
                                      mar = c(
                                        l = 31, r = 31, t = 43, b = 148,
                                        u = "mm"
                                      ),
                                      frame = FALSE,
                                      classoption = "a4paper", ...) {
  x <- as_data.frame(x)
  # get rid of extension
  if (substr(
    output_file, nchar(output_file) - 3,
    nchar(output_file)
  ) == ".pdf") {
    output_file <- substr(output_file, 1, nchar(output_file) - 4)
  }
  # Format variables
  N <- nrow(x)
  x$coll_date <- format(x$coll_date, date_format)
  x$taxon_name <- paste0("*", x$taxon_name, "*")
  x$taxon_name <- gsub(" f. ", "* f. *", x$taxon_name, fixed = TRUE)
  x$taxon_name <- gsub(" var. ", "* var. *", x$taxon_name, fixed = TRUE)
  x$taxon_name <- gsub(" ssp. ", "* ssp. *", x$taxon_name, fixed = TRUE)
  # Get rid of NA's
  for (i in names(x)) {
    x[[i]] <- paste(x[[i]])
    x[[i]][x[[i]] %in% c("NA", "*NA*")] <- ""
  }
  # Write label body
  Body <- with(x, cbind(
    rep("\\hfill\\vspace{0.1cm}\\centering\n\n\\normalsize", N),
    paste0("**", project_name, "** \\vspace{0.2cm}\n"),
    rep("\\raggedright\n\n\\leftskip=0.5cm\n\n", N),
    paste("**Familie:**", family, "\\"),
    paste("**Taxon:**", taxon_name, taxon_author, "\\"),
    rep("\\small \\vspace{0.2cm}\n\n", N),
    paste(
      "**Land:**", name_0, "\\hspace{0.5cm} **Provinz:**",
      name_1, "\\"
    ),
    paste("**Fundort:**", locality, "\\"),
    paste("**Standort:**", habitat, "\\"),
    paste("**Datum:**", coll_date, "\\"),
    paste(
      "**Sammler:**", leg, "\\hspace{0.5cm} **Sammelnr.:**",
      coll_nr, "\\"
    ),
    paste("**det.:**", det, "\\"),
    rep("\\vspace{0.2cm}\n", N),
    paste("**Anmerkungen:**", remarks, "\\"),
    rep("\\leftskip=0cm\n\n\\pagebreak", N)
  ))
  # For the geometry
  if (frame) {
    geometry <- paste0(c("bindingoffset=0mm", paste0(
      c(
        "left=", "right=",
        "top=", "bottom="
      ), mar[c("l", "r", "t", "b")],
      rep(mar["u"], 4)
    ), "footskip=0mm", "showframe"), collapse = ",")
  } else {
    geometry <- paste0(c("bindingoffset=0mm", paste0(
      c(
        "left=", "right=",
        "top=", "bottom="
      ), mar[c("l", "r", "t", "b")],
      rep(mar["u"], 4)
    ), "footskip=0mm"), collapse = ",")
  }
  # Produce single files
  Labels <- write_rmd(
    geometry = geometry,
    "header-includes" = c(
      # "- \\usepackage{showframe}",
      "- \\usepackage[english]{babel}",
      "- \\pagenumbering{gobble}"
    ),
    output = "pdf_document",
    body = txt_body(as.vector(t(Body))),
    classoption = classoption
  )
  out_file <- tempfile()
  render_rmd(Labels, output_file = out_file)
  file.copy(
    from = paste(out_file, "pdf", sep = "."),
    to = paste(output_file, "pdf", sep = "."),
    overwrite = TRUE
  )
  invisible(Labels)
}
