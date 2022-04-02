#' @importFrom biblioDB read_pg
#' @importFrom DBI dbConnect dbGetQuery dbSendQuery
#' @importFrom methods new
#' @importFrom rpostgis pgInsert
#' @importFrom sf st_read st_geometry<-
#' @importFrom stringr str_split
#' @importFrom taxlist accepted_name clean_strings merge_taxa
#' @importFrom tcltk tclVar tclvalue tkbind tkbutton tkdestroy tkentry tkfocus
#'     tkgrid tkgrid.configure tklabel tktoplevel tkwait.window tkwm.title
#' @importClassesFrom RPostgreSQL PostgreSQLConnection
#' @importClassesFrom taxlist taxlist
#' @importClassesFrom vegtable vegtable
#' @import taxlist
#' @import vegtable
NULL
# TODO: replace read_pg for a general function working with any DB
