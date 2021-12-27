#' @name givd_summary
#' 
#' @title GIVD Metadata
#' 
#' @description 
#' Get metadata for the GIVD mask.
#' 
#' @param obj A vegtable object.
#' 
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#' 
#' @export givd_summary
#' 
givd_summary <- function(obj) {
	message(paste("## Number of Sources:", length(unique(obj$data_source))))
	message(paste("## Number of Observations:", nrow(obj@header)))
	size_range <- range(obj$plot_size, na.rm=TRUE)
	message(paste("## Plot Size Range (m^2):", paste(size_range,
							collapse=" -- ")))
	size_cuts <- c(0,1,10,100,1000,10000,1000000)
	size_range <- with(obj@header, round(summary(cut(plot_size, c(0,1,10,100,
													1000,10000,1000000)))*
							100/length(ReleveID)))
	for(i in 1:(length(size_cuts) - 1)) {
		if(i < (length(size_cuts) - 1)) {
			message(paste0("\t", as.integer(size_cuts[i]), "-",
							as.integer(size_cuts[i + 1]), ": ", size_range[i], 
							"%"))
		} else {
			message(paste0("\t> ", as.integer(size_cuts[i]), ": ",
							size_range[i], "%"))
		} 
	}
	if(length(size_range) == length(size_cuts))
		message(paste0("\tNA's: ", size_range[length(size_range)], "%"))
	record_year <- as.integer(format(obj$record_date, "%Y"))
	publ_year <- with(obj@relations$data_source,
			as.integer(year)[match(obj$data_source, data_source)])
	record_year <- replace_na(record_year, 1:length(record_year),
			1:length(publ_year), publ_year)
	year_range <- range(record_year, na.rm=TRUE)
	message(paste0("## Record Year Range: ", paste(year_range, collapse=" - ")))
	year_cuts <- c(0,1900 + seq(20, 120, 10))
	year_range <- round(summary(cut(record_year, year_cuts, include.lowest=TRUE,
							right=FALSE))*100/nrow(obj@header))
	for(i in 1:(length(year_cuts) - 1)) {
		if(i == 1) {
			message(paste0("\tolder than ", as.integer(year_cuts[i + 1]), ": ",
							year_range[i], "%"))
		} else {
			message(paste0("\t", as.integer(year_cuts[i]), "-",
							as.integer(year_cuts[i + 1]), ": ", year_range[i], 
							"%"))
		}
	}
	if(length(year_range) == length(year_cuts))
		message(paste0("\tNA's: ", year_range[length(year_range)], "%"))
	obj <- relation2header(obj, "country_code", vars="name_short")
	countries <- round(summary(as.factor(obj$name_short))*100/nrow(obj@header))
	message(paste0("## Country of Record:"))
	for(i in 1:length(countries))
		message(paste0("\t", names(countries)[i], ": ", countries[i], "%"))
	obj <- transform(obj, to="cover_percentage", rule="middle")
	message(paste0("## Records with abundance: ", with(obj@samples,
							round(sum(!is.na(cover_percentage))*
											100/length(cover_percentage))),
					"%"))
	message(paste0("## Records with presence-only: ", with(obj@samples,
							round(sum(!is.na(presence_only) & presence_only)*
											100/length(cover_percentage))),
					"%"))
	valid_coords <- with(obj@header, round(sum(!is.na(validation_coordinates) &
											validation_coordinates)*
							100/length(ReleveID)))
	message(paste0("## Recorded Location: ", valid_coords, "%"))
	message(paste0("## Estimated Location: ", 100 - valid_coords, "%"))
	message(paste0("## Number of taxon usage names: ",
					nrow(obj@species@taxonNames)))
	message(paste0("## Number of taxon concepts: ",
					nrow(obj@species@taxonRelations)))
}
