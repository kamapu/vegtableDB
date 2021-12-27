#' @name db2taxlist
#' 
#' @title Import relational databases into taxlist objects
#' 
#' @description 
#' Ad-hoc function for importing Postgres tables into objects of class
#' [taxlist-class].
#' 
#' This function has been modified to a new version of the database.
#' 
#' @param conn A database connection provided by [dbConnect()].
#' @param taxon_names,taxon_relations,taxon_traits,taxon_views Character
#'     vectors indicating the name of the schema and the table containing the
#'     information for the respective slots.
#' @param taxon_levels,names2concepts Character vectors indicating the name of
#'     schema and table indicating the taxonomic ranks and the correspondence of
#'     names to taxonomic concepts.
#' @param subset_levels Logical value indicating whether taxonomic ranks should
#'     be restricted to the used ones or all ranks available in the database.
#' @param as_list Logical value indicating whether the output should be a list
#'     or a [taxlist-class] object.
#' @param ... Further arguments passed among methods. In the two wrappers the
#'     arguments are passed to `db2taxlist`.
#' 
#' @rdname db2taxlist
#' 
#' @export
db2taxlist <- function (conn, ...) {
	UseMethod("db2taxlist", conn)
}

#' @rdname db2taxlist
#' @export 
db2taxlist.PostgreSQLConnection <- function(conn, taxon_names, taxon_relations,
		taxon_traits, taxon_levels, taxon_views, names2concepts,
		subset_levels = TRUE, as_list = FALSE, ...) {
	species_obj <- list()
	# Import taxon names
	message("Importing taxon names...")
	Query <- paste0("SELECT *\n",
			"FROM \"", paste(taxon_names, collapse="\".\""), "\";\n")
	species_obj$taxonNames <- dbGetQuery(conn, Query)
	# Import taxon concepts
	message("Importing taxon concepts...")
	Query <- paste0("SELECT *\n",
			"FROM \"", paste(taxon_relations, collapse="\".\""), "\";\n")
	species_obj$taxonRelations <- dbGetQuery(conn, Query)
	# Link names and concepts
	Query <- paste0("SELECT *\n",
			"FROM \"", paste(names2concepts, collapse="\".\""), "\";\n")
	concepts <- dbGetQuery(conn, Query)
	species_obj$taxonNames$TaxonConceptID <-
			concepts$TaxonConceptID[match(species_obj$taxonNames$TaxonUsageID,
							concepts$TaxonUsageID)]
	species_obj$taxonNames <-
			species_obj$taxonNames[
					!is.na(species_obj$taxonNames$TaxonConceptID),]
	# Add status (accepted names)
	species_obj$taxonRelations$AcceptedName <-
			with(concepts[concepts$NameStatus == "accepted",],
					TaxonUsageID[
							match(species_obj$taxonRelations$TaxonConceptID,
									TaxonConceptID)])
	species_obj$taxonRelations$Basionym <-
			with(concepts[concepts$NameStatus == "basionym",],
					TaxonUsageID[
							match(species_obj$taxonRelations$TaxonConceptID,
									TaxonConceptID)])
	# Retrieve levels
	Query <-  paste0("SELECT *\n",
			"FROM \"", paste(taxon_levels, collapse="\".\""), "\";\n")
	tax_levels <- dbGetQuery(conn, Query)
	if(subset_levels) tax_levels <- tax_levels[tax_levels$Level %in%
						species_obj$taxonRelations$Level,]
	tax_levels <- tax_levels[order(tax_levels$rank),]
	species_obj$taxonRelations$Level <- factor(species_obj$taxonRelations$Level,
			tax_levels$Level)
	# Retrieve taxon traits
	if(!missing(taxon_traits)) {
		Query <-  paste0("SELECT *\n",
				"FROM \"", paste(taxon_traits, collapse="\".\""), "\";\n")
		species_obj$taxonTraits <- dbGetQuery(conn, Query)
	} else species_obj$taxonTraits <- data.frame(TaxonConceptID=integer(0))
	# Import taxon views
	message("Importing taxon views...")
	# TODO: Next command may need more arguments to be set
	species_obj$taxonViews <- biblioDB::read_pg(conn, name = taxon_views[1],
			main_table = taxon_views[2])
	species_obj$taxonViews <- with(species_obj, {
				taxonViews <- taxonViews[taxonViews$bibtexkey %in%
								taxonRelations$view_key, ]
				taxonViews <- taxonViews[ , apply(taxonViews, 2,
								function(x) !all(is.na(x)))]
				taxonViews
			})
	# Replace idx for taxon views
	species_obj$taxonViews$ViewID <- seq_along(species_obj$taxonViews[ , 1])
	species_obj$taxonRelations$ViewID <- with(species_obj,
			taxonViews[match(taxonRelations$view_key, taxonViews$bibtexkey),
					"ViewID"])
	# Delete column view_key from output
	species_obj$taxonRelations <- with(species_obj, taxonRelations[ ,
					colnames(taxonRelations) != "view_key"])
	# Set ViewID at the beginning of table
	species_obj$taxonViews <- with(species_obj, taxonViews[ ,
					c("ViewID", colnames(taxonViews)[colnames(taxonViews) !=
											"ViewID"])])
	message("DONE!\n")
	if(as_list) invisible(species_obj) else {
		species_obj <- with(species_obj,
				new("taxlist",
						taxonNames=clean_strings(taxonNames),
						taxonRelations=clean_strings(taxonRelations),
						## TODO: clean_strings to other objects
						## taxonViews=clean_strings(taxonViews),
						taxonViews=taxonViews,
						taxonTraits=clean_strings(taxonTraits)))
		return(species_obj)
	}
}

#' @rdname db2taxlist
#' 
#' @aliases swea_tax
#' 
#' @export
swea_tax <- function(conn,
		taxon_names = c("tax_commons","taxonNames"),
		taxon_relations = c("swea_dataveg","taxonRelations"),
		taxon_traits = c("swea_dataveg","taxonTraits"),
		taxon_views = c("bib_references", "main_table"),
		taxon_levels = c("tax_commons","taxonLevels"),
		names2concepts = c("swea_dataveg","names2concepts"),
		...) {
	db2taxlist(conn = conn, taxon_names = taxon_names,
			taxon_relations = taxon_relations, taxon_traits = taxon_traits,
			taxon_views = taxon_views, taxon_levels = taxon_levels,
			names2concepts = names2concepts, ...)
}

#' @rdname db2taxlist
#' 
#' @aliases sam_tax
#' 
#' @export
sam_tax <- function(conn,
		taxon_names=c("tax_commons","taxonNames"),
		taxon_relations=c("sudamerica","taxonRelations"),
		taxon_traits=c("sudamerica","taxonTraits"),
		taxon_views = c("bib_references", "main_table"),
		taxon_levels=c("tax_commons","taxonLevels"),
		names2concepts=c("sudamerica","names2concepts"),
		...) {
	db2taxlist(conn = conn, taxon_names = taxon_names,
			taxon_relations = taxon_relations, taxon_traits = taxon_traits,
			taxon_views = taxon_views, taxon_levels = taxon_levels,
			names2concepts = names2concepts, ...)
}
