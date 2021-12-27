#' @name merge_taxa
#' 
#' @title Merge multiple concepts into one
#' 
#' @description 
#' Different taxon concepts may be merged by taxonomic revisions.
#' 
#' All concepts indicated in argument `concept_id` will be set as
#' synonyms of the first concept in the vector.
#' 
#' Take care of producing backups before starting the manipulation of
#' databases.
#' 
#' @param object A database connection provided by [dbConnect()].
#' @param taxon_relations,taxon_traits,names2concepts Character vectors
#'     containing the name of the schema and for the respective information.
#' @param concepts ID of taxon concepts to be merged.
#' @param ... Further arguments passed among methods.
#' 
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#' 
#' @aliases merge_taxa,PostgreSQLConnection,numeric,missing-method
#' 
#' @exportMethod merge_taxa
#' 
setMethod("merge_taxa", signature(object = "PostgreSQLConnection",
				concepts = "numeric", level = "missing"),
		function(object, concepts, names2concepts,
				taxon_relations, taxon_traits, ...) {
			if(length(concepts) < 2)
				stop("Argument 'concepts' have to be of length 2 or higher.")
			# change status of names
			Query <- paste0("UPDATE \"", paste0(names2concepts,
							collapse = "\".\""),
					"\"\n",
					"SET \"NameStatus\" = 'synonym'\n",
					"WHERE \"TaxonConceptID\" IN (",
					paste(concepts[-1], collapse=","),");\n")
			dbSendQuery(object, Query)
			# change names to concept
			Query <- paste0("UPDATE \"", paste0(names2concepts,
							collapse = "\".\""),
					"\"\n",
					"SET \"TaxonConceptID\" = ", concepts[1], "\n",
					"WHERE \"TaxonConceptID\" IN (",
					paste(concepts[-1], collapse=","), ");\n")
			dbSendQuery(object, Query)
			# change entries as parent
			Query <- paste0("UPDATE \"", paste0(taxon_relations,
							collapse = "\".\""),
					"\"\n",
					"SET \"Parent\" = ", concepts[1], "\n",
					"WHERE \"Parent\" IN (", paste(concepts[-1],
							collapse=","),");\n")
			dbSendQuery(object, Query)
			# delete from taxonTraits before deleting concept
			Query <- paste0("DELETE FROM \"",
					paste0(taxon_traits, collapse = "\".\""), "\"\n",
					"WHERE \"TaxonConceptID\" IN (",
					paste(concepts[-1], collapse=","),");\n")
			dbSendQuery(object, Query)
			# delete old concepts
			Query <- paste0("DELETE FROM \"",
					paste0(taxon_relations, collapse = "\".\""), "\"\n",
					"WHERE \"TaxonConceptID\" IN (",
					paste(concepts[-1], collapse=","),");\n")
			dbSendQuery(object, Query)
			message("DONE!")
		}
)
