#' @name accepted_name
#' 
#' @title Switch accepted names by synonyms in a taxon
#' 
#' @description 
#' If necessary to set a synonym in a taxon concept as an accepted name.
#' 
#' @param taxlist An object of class [PostgreSQLConnection-class].
#' @param ConceptID Integer containing concept IDs where to request or set names
#'     for one category.
#' @param accepted Integer indicating the usage name ID to be set as accepted
#'     name. If this usage name does not belong to the concept indicated by
#'     'ConceptID', an error message will be retrieved.
#' @param names2concepts A character vector with two values, the name of the
#'     schema and the name of the table relating names with taxon concepts.
#' @param ... Further arguments passed among methods.
#' 
#' @aliases accepted_name,PostgreSQLConnection,numeric-method
#' 
#' @exportMethod accepted_name 
#' 
setMethod("accepted_name", signature(taxlist = "PostgreSQLConnection",
				ConceptID = "numeric"),
		function(taxlist, ConceptID, accepted, names2concepts, ...) {
			if(length(ConceptID) != length(accepted))
				stop(paste("Arguments for 'ConceptID' and 'accepted'",
								"have to be of identical length."))
			for(i in 1:length(ConceptID)) {
				Query <- paste0("SELECT *\n",
						"FROM \"", paste0(names2concepts, collapse = "\".\""),
						"\";\n")
				all_n2c <- dbGetQuery(taxlist, Query)
				if(!with(all_n2c, accepted[i] %in%
								TaxonUsageID[TaxonConceptID == ConceptID[i]])) {
					message(paste0("Name '", accepted[i], "' is not included",
									" in concept '", ConceptID[i], "'"))
				} else {
					# TODO: It may be adapted for additional status (e.g. basionym)
					Query <- paste0("UPDATE \"", paste0(names2concepts,
									collapse = "\".\""),"\"\n",
							"SET \"NameStatus\" = 'synonym'\n",
							"WHERE \"TaxonConceptID\" = ", ConceptID[i],";\n")
					dbSendQuery(conn, Query)
					Query <- paste0("UPDATE \"", paste0(names2concepts,
									collapse = "\".\""),"\"\n",
							"SET \"NameStatus\" = 'accepted'\n",
							"WHERE \"TaxonUsageID\" = ", accepted[i],";\n")
					dbSendQuery(conn, Query)
				}
					
			}
			message("DONE!")
		})
