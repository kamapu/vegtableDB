#' @name db2vegtable
#' 
#' @title Import PostgreSQL databases into vegtable objects
#' 
#' @description 
#' Import and adaption of Postgres tables into objects of class
#' [vegtable-class].
#' 
#' In the case that some schemas are not mentioned, the function assumes such
#' tables are located in the same schema as the table header. Thus for
#' databases placed in just one schema, this need to be set only in argument
#' `header_schema`.
#' 
#' @param conn A database connection provided by [dbConnect()].
#' @param header,samples Character vectors indicating the schema and table
#'     containing header and samples information, respectively.
#' @param sql_header SQL statement to be used instead of `header`.
#' @param layers,coverconvert Lists of vectors for the respective slots,
#'     each containing schema and name of required table.
#' @param relations A list of vectors indicating the schema and table in the
#'     database, excluding 'data_source'.
#' @param geometry Name of the variable in header containing the geometry of
#'     the plots.
#' @param description Named vector with metadata.
#' @param as_list Logical value indicating whether a list or an object of class
#'     [vegtable-class] should be returned.
#' @param get_data_sources Logical argument whether references should be
#'     imported as data sources or not.
#' @param bib_args List of arguments passed to [biblio::read_bib()].
#' @param taxon_names,taxon_relations,taxon_traits,taxon_views,taxon_levels,names2concepts
#'     Arguments passed to [db2taxlist()].
#' @param ... Further arguments passed to [db2taxlist()].
#' @param get_countries Logical argument, specific for the databases
#'     'sudamerica' and 'SWEA-Dataveg', indicating whether country information
#'     should be reimported from integrated map.
#' @param head_cols Character vector indicating the header variables to be
#'     imported (except the coordinates).
#' @param samples_cols Character vector indicating the samples variables to be
#'     imported.
#' 
#' @rdname db2vegtable
#' 
#' @export
db2vegtable <- function(conn, ...) {
	UseMethod("db2vegtable", conn)
}

#' @rdname db2vegtable
#' @export
db2vegtable.PostgreSQLConnection <- function(conn, header, sql_header, samples,
		relations, layers, coverconvert, geometry, description, as_list = FALSE,
		...) {
	veg_obj <- list()
	# description
	if(!missing(description)) veg_obj$description <- description else
		veg_obj$description <- c(remark = "Object imported by 'db2vegtable()'.")
	# species
	veg_obj$species <- db2taxlist(conn = conn, ...)
	# header
	message("Importing vegtable body...")
	if(missing(sql_header) & missing(geometry)) {
		Query <- paste0("SELECT *\n",
				"FROM \"", paste0(header, collapse = "\".\""), "\";\n")
		veg_obj$header <- dbGetQuery(conn, Query)
	} else {
		if(!missing(sql_header)) {
			if(!missing(geometry))
				warning("Argument in parameter 'geometry' will be ignored!")
			veg_obj$header <- dbGetQuery(conn, sql_header)
		} else {
			Query <- paste0("SELECT *\n",
					"FROM information_schema.columns\n",
					"WHERE table_schema = '", header[1], "'\n",
					"AND table_name = '", header[2], "';\n")
			header_cols <- dbGetQuery(conn, Query)$column_name
			# Import with geometry
			Query <- paste0("SELECT \"",
					paste0(header_cols[header_cols != geometry],
							collapse = "\",\""),
					"\",ST_X(\"", geometry, "\") longitude,ST_Y(\"", geometry,
					"\") latitude\n",
					"FROM \"", paste(header, collapse = "\".\""), "\";\n")
			veg_obj$header <- dbGetQuery(conn, Query)
		}
	}
	# samples
	Query <- paste0("SELECT *\n",
			"FROM \"", paste(samples, collapse = "\".\""), "\"\n",
			"WHERE \"ReleveID\" IN (", paste0(veg_obj$header$ReleveID,
					collapse=","), ");\n")
	veg_obj$samples <- dbGetQuery(conn, Query)
	# layers
	if(!missing(layers)) {
		veg_obj$layers <- list()
		for(i in names(layers)) {
			Query <- paste0("SELECT *\n",
					"FROM \"", paste(layers[[i]], collapse = "\".\""), "\";\n")
			veg_obj$layers[[i]] <- dbGetQuery(conn, Query)
		}		
	}
	# relations
	if(!missing(relations)) {
		veg_obj$relations <- list()
		for(i in names(relations)) {
			Query <- paste0("SELECT *\n",
					"FROM \"", paste(relations[[i]], collapse = "\".\""),
					"\";\n")
			veg_obj$relations[[i]] <- dbGetQuery(conn, Query)
		}
	}
	# coverconvert
	if(!missing(coverconvert)) {
		veg_obj$coverconvert <- new("coverconvert")
		for(i in names(coverconvert)) {
			Query <- paste0("SELECT *\n",
					"FROM \"", paste(coverconvert[[i]], collapse = "\".\""),
					"\";\n")
			cover_tab <- dbGetQuery(conn, Query)
			veg_obj$coverconvert@value[[i]] <- with(cover_tab,
					factor(symbol, levels = symbol))
			veg_obj$coverconvert@conversion[[i]] <- with(cover_tab,
					c(bottom[1], top))
		}
	}
	# final output
	message("DONE!\n")
	if(as_list)
		invisible(veg_obj) else {
		veg_obj <- new("vegtable",
				description = clean_strings(veg_obj$description),
				samples = veg_obj$samples,
				header = clean_strings(veg_obj$header),
				species = veg_obj$species,
				relations = veg_obj$relations,
				coverconvert = veg_obj$coverconvert)
		return(veg_obj)
	}
}

#' @rdname db2vegtable
#' 
#' @aliases import_swea
#' 
#' @export
import_swea <- function(conn,
		header = c("swea_dataveg","header"),
		samples = c("swea_dataveg","samples"),
		relations = list(
				globe_plots = c("swea_dataveg","globe_plots"),
				swea1_code = c("swea_dataveg","swea1_code"),
				soil_moisture = c("swea_dataveg","soil_moisture"),
				soil_texture = c("swea_dataveg","soil_texture"),
				community_type = c("commons","community_type"),
				naturalness = c("swea_dataveg","naturalness"),
				record_type = c("swea_dataveg","record_type")
		),
		layers = list(
				veg_layer = c("swea_dataveg","veg_layer"),
				spec_miguel = c("specimens","specimens_miguel")
		),
		coverconvert = list(
				br_bl = c("commons","br_bl"),
				b_bbds = c("commons","b_bbds"),
				ordinal = c("commons","ordinal")
		),
		geometry = "plot_centroid",
		get_countries = TRUE,
		get_data_sources = TRUE,
		bib_args = list(),
		taxon_names = c("tax_commons","taxonNames"),
		taxon_relations = c("swea_dataveg","taxonRelations"),
		taxon_traits = c("swea_dataveg","taxonTraits"),
		taxon_views = c("bib_references", "main_table"),
		taxon_levels = c("tax_commons","taxonLevels"),
		names2concepts = c("swea_dataveg","names2concepts"),
		...) {
	# Final object
	message("Importing vegtable body...")
	suppressMessages(veg_obj <- db2vegtable(conn = conn, header = header,
					samples = samples, relations = relations, layers = layers,
					coverconvert = coverconvert, geometry = geometry,
					taxon_names = taxon_names,
					taxon_relations = taxon_relations,
					taxon_traits = taxon_traits, taxon_views = taxon_views,
					taxon_levels = taxon_levels,
					names2concepts = names2concepts, ...))
	# Adding Country codes
	message("Importing country codes...")
	if(get_countries) {
		Query <- paste0("SELECT \"ReleveID\",\"ADM0_A3\"\n",
				"FROM \"", paste0(header, collapse = "\".\""),
				"\",commons.countries_map\n",
				"WHERE ST_Intersects(commons.countries_map.unit,\"",
				paste0(header, collapse = "\".\""), "\".plot_centroid);\n")
		Countries <- dbGetQuery(conn, Query)
		veg_obj@header$country_code <- with(Countries,
				ADM0_A3[match(veg_obj@header$ReleveID, ReleveID)])
		Countries <- dbGetQuery(conn, "SELECT * FROM commons.countries;")
		colnames(Countries) <- c("country_code","name_short","name_long",
				"population","sov_code_1","sov_code_2","sov_state","continent")
		veg_obj@relations$country_code <- Countries
	}
	# Adding Data sources
	message("Importing data sources...")
	if(get_data_sources) {
		data_source <- do.call(read_pg, c(conn = conn, name = "bib_references",
						bib_args))
		data_source <- data_source[data_source$bibtexkey %in%
						veg_obj$bibtexkey, ]
		data_source$data_source <- seq_along(data_source$bibtexkey)
		veg_obj$data_source <- with(veg_obj@header,
				data_source$data_source[match(bibtexkey,
								data_source$bibtexkey)])
		# Delete bibtexkey from header
		veg_obj@header <- veg_obj@header[ , colnames(veg_obj@header) !=
						"bibtexkey"]
		# Delete empty columns
		data_source <- data_source[ , apply(data_source, 2,
						function(x) !all(is.na(x)))]
		# Insert to relations
		veg_obj@relations$data_source <- data_source[ , c("data_source",
						colnames(data_source)[colnames(data_source) !=
										"data_source"])]
	}
	message("DONE!\n")
	return(veg_obj)
}

#' @rdname db2vegtable
#' 
#' @aliases import_sam
#' 
#' @export
import_sam <- function(conn,
		header = c("sudamerica","header"),
		samples = c("sudamerica","samples"),
		relations = list(
				community_type = c("commons","community_type")
		),
		layers = list(
				spec_miguel = c("specimens","specimens_miguel")
		),
		coverconvert = list(
				br_bl = c("commons","br_bl"),
				b_bbds = c("commons","b_bbds"),
				ordinal = c("commons","ordinal")
		),
		geometry = "plot_centroid",
		get_countries = TRUE,
		get_data_sources = TRUE,
		bib_args = list(),
		taxon_names = c("tax_commons","taxonNames"),
		taxon_relations = c("sudamerica","taxonRelations"),
		taxon_traits = c("sudamerica","taxonTraits"),
		taxon_views = c("bib_references", "main_table"),
		taxon_levels = c("tax_commons","taxonLevels"),
		names2concepts = c("sudamerica","names2concepts"),
		...) {
	# Final object	
	veg_obj <- import_swea(conn = conn, header = header, samples = samples,
			relations = relations, layers = layers,
			coverconvert = coverconvert, geometry = geometry,
			taxon_names = taxon_names,
			taxon_relations = taxon_relations,
			taxon_traits = taxon_traits, taxon_views = taxon_views,
			taxon_levels = taxon_levels,
			names2concepts = names2concepts, get_countries = get_countries,
			get_data_sources = get_data_sources, bib_args = bib_args, ...)
}

#' @rdname db2vegtable
#' 
#' @aliases import_bernice
#' 
#' @export
import_bernice <- function(conn,
		description = c(
				Title = "Parthenium survey in Lake Baringo",
				Author = "Bernice Mereina Sainepo",
				Source = "SWEA-Dataveg (GIVD-AF-00-006)",
				Version = Sys.Date()),
		head_cols = c("ReleveID", "code_trr228", "original_number", "record_date",
				"plot_size", "data_source", "elevation"),
		samples_cols = c("record_id", "ReleveID", "quadrant", "TaxonUsageID",
				"misspelled_name", "cover_percentage", "frequency"),
		...) {
	# header
	sql_header <- paste0("SELECT \"", paste(head_cols, collapse = "\", \""),
			"\", ST_X(plot_centroid) longitude, ST_Y(plot_centroid) latitude\n",
			"FROM swea_dataveg.header\n", "WHERE data_source  =  98;\n")
	veg_obj <- import_swea(conn = conn, description = description,
			sql_header = sql_header, ...)
	veg_obj@samples <- veg_obj@samples[ , colnames(veg_obj@samples) %in%
					samples_cols]
	return(veg_obj)
}
