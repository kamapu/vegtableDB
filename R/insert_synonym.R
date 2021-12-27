#' @name insert_synonym
#' 
#' @title Insert synonyms in PostgreSQL taxonomic lists
#' 
#' @description 
#' Insert synonyms to existing taxa in a PostgreSQL version of [taxlist-class]
#' objects.
#' 
#' This function is updating the tables `taxonNames` and `names2concepts` in
#' the PostgreSQL version of the database.
#' 
#' @param conn A database connection provided by [dbConnect()].
#' @param taxon_names A character vector of length 2 indicating the name of the
#'     respecitve schema and table in Postgres.
#' @param taxon_relations taxon_names A character vector of length 2 indicating
#'     the name of the respecitve schema and table in Postgres.
#' @param names2concepts taxon_names A character vector of length 2 indicating
#'     the name of the respecitve schema and table in Postgres.
#' @param df A data frame with new names and related information (including
#'     taxon concept ID).
#' @param clean A logical value, whether strings in input 'df' should be cleaned
#'     or not (see [clean_strings()]).
#' @param ... Further arguments passed among methods.
#' 
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#' 
#' @rdname insert_synonym
#' 
#' @export
insert_synonym <- function (conn, ...) {
	UseMethod("insert_synonym", conn)
}

#' @rdname insert_synonym
#' 
#' @aliases insert_synonym,PostgreSQLConnection-method
#' 
#' @export
insert_synonym.PostgreSQLConnection <- function(conn, taxon_names,
		taxon_relations, names2concepts, df, clean = TRUE, ...) {
	if(clean)
		df <- clean_strings(df)
	if(any(!c("TaxonConceptID", "TaxonName", "AuthorName") %in% colnames(df)))
		stop(paste("Columns 'TaxonConceptID', 'TaxonName' and 'AuthorName'",
						"are mandatory in 'df'."))
	## Cross-check
	# Check duplicated combinations in 'df'
	if(any(duplicated(df[,c("TaxonName","AuthorName")])))
		stop("Duplicated combinations detected in 'df'.")
	## Extract names of database as reference
	## Insert synonyms in a loop
	for(i in 1:nrow(df)) {
		Query <- paste0("SELECT \"TaxonUsageID\", \"TaxonName\", \"AuthorName\"",
				"\n", "FROM \"", paste(taxon_names, collapse="\".\""), "\";")
		db_names <- dbGetQuery(conn, Query)
		if(paste(df$TaxonName[i], df$AuthorName[i]) %in%
				paste(db_names$TaxonName, db_names$AuthorName)) {
			message(paste0("Name '", paste(df$TaxonName[i], df$AuthorName[i]),
							"' is already in database and will be recycled"))
			usage_id <- db_names[paste(db_names$TaxonName,
									db_names$AuthorName) ==
							paste(df$TaxonName[i],
									df$AuthorName[i]), "TaxonUsageID"]
		} else {
			usage_id <- max(db_names$TaxonUsageID) + 1
			rpostgis::pgInsert(conn, taxon_names, data.frame(df[i,
									c("TaxonName", "AuthorName")],
							TaxonUsageID = usage_id))
		}
		used_usages <- dbGetQuery(conn, paste0("SELECT \"TaxonUsageID\"\n",
						"FROM \"", paste0(names2concepts, collapse = "\".\""),
						"\";\n"))$TaxonUsageID
		if(usage_id %in% used_usages) {
			message(paste0("Name '", paste(df$TaxonName[i], df$AuthorName[i]),
							"' is already in use and won't be inserted ",
							"to the taxlist objec."))
		} else {
			rpostgis::pgInsert(conn, names2concepts, data.frame(
							TaxonUsageID = usage_id,
							TaxonConceptID = df$TaxonConceptID[i],
							NameStatus = "synonym"))
		}
	}
	message("Done!")
}

#' @rdname insert_synonym
#' 
#' @aliases insert_synonym_swea
#' 
#' @export insert_synonym_swea
#' 
insert_synonym_swea <- function(conn,
		taxon_names=c("tax_commons", "taxonNames"),
		taxon_relations=c("swea_dataveg", "taxonRelations"),
		names2concepts=c("swea_dataveg", "names2concepts"),
		df, ...) {
	insert_synonym(conn, taxon_names, taxon_relations, names2concepts,
			df, ...)
}
