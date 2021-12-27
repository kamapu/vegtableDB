#' @name draw_badge
#' 
#' @title Draw a GIVD or a version badge
#' 
#' @description 
#' Badges to insert in database pages for displaying the ID and link to the
#' respective entry at \href{https://www.givd.info/}{GIVD} (Global Index of
#' Vegetation-Plot Databases).
#' 
#' This function writes the respective svg badge and suggests the markdown code
#' needed to insert the badge in your site. Note that the markdown code may need
#' edition.
#' 
#' @param id A character value indicating the ID of the database in GIVD.
#' @param path A character value indicatin the path where to write the svg file,
#'     including the file name.
#' @param version A character value for the version, usually the date of the
#'     version.
#' @param link A character value with a url to link the version badge (usually
#'     a page for the database).
#' @param type The type of the badge, either "version" or "givd".
#' @param encoding Character value indicating the encoding of the output file,
#'     passed to [file()].
#' @param ... Further arguments passed to [file()].
#' 
#' @export 
#' 
draw_badge <- function(id, path, version = Sys.Date(), link, type = "version",
		encoding = "UTF-8", ...) {
	type <- pmatch(tolower(type), c("version", "givd"))
	if(!type %in% c(1, 2))
		stop("Invalid argument in parameter 'type'.")
	# Add extension if missing
	if(substr(path, nchar(path) - 4, nchar(path)) != ".svg")
		path <- paste0(path, ".svg")
	if(type == 1) {
		badge <- readLines(file.path(path.package("vegtable2"), "version.svg"))
		badge <- gsub("#VERSION", version, badge, ignore.case = FALSE)
		con <- file(path, "wb", encoding = encoding, ...)
		writeBin(charToRaw(paste0(badge, collapse = "\n")), con,
				endian = "little")
		close(con)
		md_code <- paste0("[![version](", path, ")](", link, ")")
		message(paste0("Markdown code:\n", md_code))
	}
	if(type == 2) {
		badge <- readLines(file.path(path.package("vegtable2"), "givd.svg"))
		badge <- gsub("#ID", id, badge, ignore.case = FALSE)
		con <- file(path, "wb", encoding = encoding, ...)
		writeBin(charToRaw(paste0(badge, collapse = "\n")), con,
				endian = "little")
		close(con)
		md_code <- paste0("[![GIVD](badges/", path,
				")](http://www.givd.info/ID/", id, ")")
		message(paste0("Markdown code:\n", md_code))
	}
}
