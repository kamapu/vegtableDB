#' @name sql_delete
#' 
#' @title Write query and execute it for deleted entries
#' 
#' @param conn A connection established by [dbConnect()].
#' @param obj A comparison object created by `compare_df()`.
#' @param name A character vector with the names of schema and table in
#'     PostgreSQL.
#' @param key A character value. The name of the primary key in the table.
#' @param ... Further arguments (not yet used).
#' 
#' @keywords internal
#' 
sql_delete <- function(conn, obj, name, key, ...) {
	Query <- paste0("DELETE FROM \"",
			paste0(name, collapse="\".\""),
			"\"\n",
			"WHERE \"", key,"\" IN ('", paste0(obj$deleted, collapse="','"),
			"');")
	dbSendQuery(conn, Query)
}

#' @name sql_update
#' 
#' @title Write query and execute it for updates
#' 
#' @description 
#' For arguments, see `sql_delete()`.
#' 
#' @keywords internal
#' 
sql_update <- function(conn, obj, name, key, ...) {
	for(i in rownames(obj$updated)) {
		Query <- paste0("UPDATE \"", paste0(name, collapse="\".\""), "\"\n",
				"SET \"", with(obj,
						paste0(paste0(colnames(updated)[updated[i,]],
										"\" = '", new_vals[i,updated[i,]]),
								collapse="', \n\"")), "'\n",
				"WHERE \"", key, "\" = '", i, "';")
		dbSendQuery(conn, Query)
	}
}

# TODO: before delete, check foreign keys (e.g. plant_taxonomy.view_key)

#' @name update
#' 
#' @rdname update
#' 
#' @title Compare Bibtex-files with Postgres databases and update
#' 
#' @description 
#' When changes done in the Bibtex duplicated file, they can be briefly
#' displayed before export.
#' 
#' In `update_pg` actions 'delete', 'add', and 'update' have to be accordingly
#' set as `TRUE`, otherwise only `print_report()` will be executed.
#' 
#' @param object A connection to a reference database established by
#'     [dbConnect()].
#' @param revision A [lib_df-class] object imported by [read_bib()]. This data
#'     set represent an updated version of 'object'.
#' @param key A character value indicating the name of the column used as
#'     identifier for references.
#' @param name Character value indicating the name of the schema in Postgres.
#'     This argument is passed to [read_pg()].
#' @param db_args List of named arguments passed to [read_pg()].
#' @param main_table Character value indicating the name of main table in
#'     Postgres (see [read_pg()]).
#' @param file_list Character value indicating the name of file list table in
#'     Postgres (see [read_pg()]).
#' @param delete Logical value indicating whether missing entries in 'bib' have
#'     to be deleted in 'db'.
#' @param add Logical value indicating whether new entries in 'bib' have to be
#'     inserted in 'db'.
#' @param update Logical value indicating whether entries modified in 'bib' have
#'     to be updated in 'db'.
#' @param ... Further arguments passed to [pgInsert()].
#' 
#' @method update PostgreSQLConnection
#' @export
#' 
update.PostgreSQLConnection <- function(object, revision, key = "bibtexkey",
		name, db_args = list(), delete = FALSE, add = FALSE,
		update = FALSE, main_table = "main_table", file_list = "file_list",
		...) {
	db_tab <- do.call(read_pg, c(list(conn = object, name = name), db_args))
	Comp_obj <- compare_df(x = object, y = revision, name = name)
	# TODO: Test databases using a different key
	if(all(!c(delete, add, update)))
		print(Comp_obj) else {
		db_fl <- file_list(db_tab)
		db_tab <- db_tab[ ,colnames(db_tab) != "file"]
		bib_fl <- file_list(revision)
		revision <- revision[ ,!colnames(revision) != "file"]
		# TODO: Look for options to replace pgInsert by dbWriteTable
		if(add) {
			if(nrow(Comp_obj[[main_table]]$added) > 0)
				pgInsert(object, c(name, main_table),
						Comp_obj[[main_table]]$added, partial.match = TRUE, ...)
			if(nrow(Comp_obj[[file_list]]$added) > 0)
				pgInsert(object, c(name, file_list),
						Comp_obj[[file_list]]$added, partial.match = TRUE, ...)
		}
		if(delete) {
			# Inverted order because of dependencies
			if(length(Comp_obj[[file_list]]$deleted) > 0)
				sql_delete(conn = object, obj = Comp_obj[[file_list]],
						c(name, file_list),
						"file")
			if(length(Comp_obj[[main_table]]$deleted) > 0)
				sql_delete(conn = object, obj = Comp_obj[[main_table]],
						c(name, main_table),
						key)
		}
		if(update) {
			if(nrow(Comp_obj[[main_table]]$updated) > 0) {
				for(i in colnames(Comp_obj[[main_table]]$new_vals))
					Comp_obj[[main_table]]$new_vals[,i] <- gsub("'", "\'\'",
							Comp_obj[[main_table]]$new_vals[,i], fixed=TRUE)
				sql_update(object, Comp_obj[[main_table]], c(name, main_table),
						key)
			}
			if(nrow(Comp_obj[[file_list]]$updated) > 0) {
				for(i in colnames(Comp_obj[[file_list]]$new_vals))
					Comp_obj[[file_list]]$new_vals[,i] <- gsub("'", "\'\'",
							Comp_obj[[file_list]]$new_vals[,i], fixed=TRUE)
				sql_update(object, Comp_obj[[file_list]], c(name, file_list),
						"file")
			}
		}
	}
	if(any(c(add, delete, update)))
		message("DONE!")
}
