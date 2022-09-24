#' @name write_labels
#'
#' @rdname write_labels
#'
#' @title Writting Labels for Herbarium Vouchers
#'
#' @description
#' Generating labels for vouchers using exported tables.
#'
#' @param x A [specimens-class] object retrieved by [read_spec()].
#' @param output_file A character value indicating the name of the file. It may
#'     include a relative path. This is passed to the homonymous parameter at
#'     [render_rmd()].
#' @param merge A logical value indicating whether labels should be merged into
#'     one A4 sheet or remain as individual pages in the output PDF file.
#' @param frame A logical value indicating whether a frame should be drawn
#'     around single labels or not. It works only if `'merge = TRUE'`.
#' @param date_format A character value indicating the format used for the
#'     collection date. It is passed to the function [format.Date()].
#' @param dim A named vector indicating the dimensions of the single labels. You
#'     need to specify **w** (width), **h** (height), and **u** (units).
#' @param nup An integer vector of length 2 indicating the number of labels per
#'     sheet (if `'merge = TRUE'`). The first value are number of columns and
#'     the second, number of rows.
#' @param mar A named vector indicating the margins. The mandatory names are
#'     **l** (left), **r** (right), **t** (top), **b** (bottom), and
#'     **u** (units).
#' @param classoption A character value to be inserted as 'classoption' in the
#'     yaml head for the Rmarkdown document.
#' @param ... Further arguments passed to [write_rmd()]. It works only if
#'     `'merge = TRUE'`.
#'
#' @return
#' An invisible [rmd_doc-class] object.
#'
#' @export
write_labels <- function(x, ...) {
  UseMethod("write_labels", x)
}

#' @rdname write_labels
#'
#' @aliases write_labels,specimens-method
#'
#' @method write_labels specimens
#'
#' @export
write_labels.specimens <- function(x, output_file, merge = TRUE, frame = FALSE,
                                   date_format = "%d.%m.%Y",
                                   dim = c(h = 74, w = 105, u = "mm"),
                                   nup = c(2, 4),
                                   mar = c(
                                     l = 7, r = 7, t = 7, b = 7,
                                     u = "mm"
                                   ),
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
    rep("\\centering\n\n\\normalsize", N),
    paste0("**", project_name, "** \\vspace{0.2cm}\n"),
    rep("\\raggedright\n", N),
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
    rep("\\pagebreak", N)
  ))
  # Produce single files
  Labels <- write_rmd(
    geometry = paste0(
      c(
        paste0(
          c("paperheight=", "paperwidth="), dim[c("h", "w")],
          rep(dim["u"], 2)
        ),
        "bindingoffset=0mm",
        paste0(
          c("left=", "right=", "top=", "bottom="),
          mar[c("l", "r", "t", "b")], rep(mar["u"], 4)
        ),
        "footskip=0mm"
      ),
      collapse = ","
    ),
    "header-includes" = c(
      "- \\usepackage[english]{babel}",
      "- \\pagenumbering{gobble}"
    ),
    output = "pdf_document",
    body = txt_body(as.vector(t(Body)))
  )
  out_file <- tempfile()
  render_rmd(Labels, output_file = out_file)
  # Merge into
  if (merge) {
    Labels2 <- write_rmd(
      geometry = paste(
        "bindingoffset=0mm",
        "left=0mm",
        "right=0mm",
        "top=0mm",
        "bottom=0mm",
        "footskip=0mm",
        sep = ","
      ),
      "header-includes" = c(
        "- \\usepackage{pdfpages}"
      ),
      output = "pdf_document",
      body = paste0(
        paste0("\\includepdf[pages=-,nup=", nup[1], "x", nup[2], ",frame="),
        tolower(paste(frame)), "]{", out_file, ".pdf}"
      ),
      classoption = classoption,
      ...
    )
    # Render merged sheets
    render_rmd(Labels2, output_file = output_file)
  } else {
    file.copy(
      from = paste(out_file, "pdf", sep = "."),
      to = paste(output_file, "pdf", sep = "."),
      overwrite = TRUE
    )
  }
  invisible(Labels)
}
