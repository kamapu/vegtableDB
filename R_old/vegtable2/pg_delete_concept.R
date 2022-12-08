#' @name pg_delete_concept
#' 
#' @title Delete Names or Concepts in PostgreSQL Taxonomic Lists
#' 
#' @description 
#' Deletion of synonyms or taxon concepts in a PostgreSQL version of
#' [taxlist-class] objects.
#' 
#' Take care of producing backups before starting the manipulation of
#' databases.
#' 
#' @param conn A database connection provided by [dbConnect()].
#' @param schema Name of the schema containing taxonomic tables.
#' @param concept_id,usage_id ID of taxon concept or taxon usage name to be
#'     deleted.
#' 
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#' 
#' @rdname pg_delete_concept
#' 
#' @export pg_delete_concept
#' 
pg_delete_concept <- function(conn, schema, concept_id) {
	query <- paste0("
select *
from ", paste0(schema, ".", "names2concepts", collapse=""), "
where \"TaxonConceptID\" in (", paste(concept_id, collapse=","),");
")
	names2concepts <- dbGetQuery(conn, query)
	response <- askYesNo(paste("Do you really like to delete",
					length(unique(concept_id)), "concept(s)?"))
	if(is.na(response))
		stop("Deletion cancelled.", call.=FALSE)
	if(response) {
		# Delete entries in names2concepts
		query <- paste0("
delete from ", paste0(schema, ".", "names2concepts", collapse=""),"
where \"TaxonConceptID\" in (", paste(concept_id, collapse=","),");
")
		dbSendQuery(conn, query)
		# Delete names
		query <- paste0("
delete from ", paste0(schema, ".", "\"taxonNames\"", collapse=""),"
where \"TaxonUsageID\" in (", paste(names2concepts$TaxonUsageID, collapse=","), ");
")
		dbSendQuery(conn, query)
		# Delete concept
		query <- paste0("
delete from ", paste0(schema, ".", "\"taxonRelations\"", collapse=""), "
where \"TaxonConceptID\" in (", paste(concept_id, collapse=","),");
")
		dbSendQuery(conn, query)
	}
	message("DONE")
}


#' @rdname pg_delete_concept
#' 
#' @aliases pg_delete_name
#' 
#' @export pg_delete_name
#' 
pg_delete_name <- function(conn, schema, usage_id) {
	query <- paste0("
					select *
					from ", paste0(schema, ".", "names2concepts", collapse=""), "
					where \"TaxonUsageID\" in (", paste(usage_id, collapse=","),");
					")
	names2concepts <- dbGetQuery(conn, query)
	if(any(names2concepts$NameStatus == "accepted"))
		stop("Attempting to delete accepted name. Use 'pg_delete_concept' instead.")
	response <- askYesNo(paste("Do you really like to delete",
					length(unique(usage_id)), "name(s)?"))
	if(is.na(response))
		stop("Deletion cancelled.", call.=FALSE)
	if(response) {
		# Delete entry in names2concepts
		query <- paste0("
						delete from ", paste0(schema, ".", "names2concepts", collapse=""),"
						where \"TaxonUsageID\" in (", paste(usage_id, collapse=","),");
						")
		dbSendQuery(conn, query)
		# Delete name
		query <- paste0("
						delete from ", paste0(schema, ".", "\"taxonNames\"", collapse=""), "
						where \"TaxonUsageID\" in (", paste(usage_id, collapse=","),");
						")
		dbSendQuery(conn, query)
	}
	message("DONE")
}

