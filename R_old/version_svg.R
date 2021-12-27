#' @name version_svg
#' 
#' @title Draw a SVG badge for database version
#' 
#' @description 
#' Function for convenience.
#' 
#' As version the date formatted accordingly but also another text is possible.
#' 
#' @param file A character value with the name of the badge without extension.
#' @param version A character value indicating the version of the database.
#' 
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#' 
#' @export version_svg
#' 
version_svg <- function(file, version) {
	file <- paste0(file, ".svg")
	write(paste("
							<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"108\" height=\"20\">
							<linearGradient id=\"b\" x2=\"0\" y2=\"100%\">
							<stop offset=\"0\" stop-color=\"#bbb\" stop-opacity=\".1\"/>
							<stop offset=\"1\" stop-opacity=\".1\"/>
							</linearGradient>
							<mask id=\"a\">
							<rect width=\"108\" height=\"20\" rx=\"3\" fill=\"#fff\"/>
							</mask>
							<g mask=\"url(#a)\"><path fill=\"#555\" d=\"M0 0h70v20H0z\"/>
							<path fill=\"#6472ba\" d=\"M30 0h78v20H30z\"/>
							<path fill=\"url(#b)\" d=\"M0 0h108v20H0z\"/>
							</g>
							<g fill=\"#fff\" text-anchor=\"middle\" font-family=\"DejaVu Sans,Verdana,Geneva,sans-serif\" font-size=\"11\">
							<text x=\"15\" y=\"15\" fill=\"#010101\" fill-opacity=\".3\">
							v.
							</text>
							<text x=\"15\" y=\"14\">
							v.
							</text>
							<text x=\"70\" y=\"15\" fill=\"#010101\" fill-opacity=\".3\">",
					version,
					"
							</text>
							<text x=\"70\" y=\"14\">",
					version,
					"
							</text>
							</g>
							</svg>", sep="\n"), file)
}
