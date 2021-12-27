#' @name pg_replace_community
#' 
#' @title Replace communities within veg_databases
#' 
#' @description 
#' This function replaces entries of 'old' by 'new' across all stored databases.
#' The community declared at 'old' will be deleted from the main table at the
#' end of the replacement.
#' 
#' @param conn A connection established by [dbConnect()].
#' @param old An integer value with the community ID (community_type) that will
#'     be replaced and deleted at the end.
#' @param new An integer value with the community ID that will be inserted in
#'     exchange to 'old'.
#' 
#' @export
#' 
pg_replace_community <- function(conn, old, new) {
	# TODO: some tests before carry out
	# Replace in sudamerica
	Query <- paste0("UPDATE sudamerica.header", "\n",
			"SET community_type = ", new, "\n",
			"WHERE community_type = ", old,";", "\n")
	dbSendQuery(conn, Query)
	# Replace in swea_dataveg
	Query <- paste0("UPDATE swea_dataveg.header", "\n",
			"SET community_type = ", new, "\n",
			"WHERE community_type = ", old,";", "\n")
	dbSendQuery(conn, Query)
	# Delete from community_type
	Query <- paste0("DELETE FROM commons.community_type", "\n",
			"WHERE community_type = ", old,";", "\n")
	dbSendQuery(conn, Query)
	# If nothing wrong
	message("DONE!")
}

