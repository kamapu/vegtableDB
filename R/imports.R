#' @importFrom methods new
#' @importFrom DBI dbGetQuery dbSendQuery
#' @importFrom biblioDB read_pg
#' @importFrom taxlist accepted_name clean_strings merge_taxa
#' @importFrom rpostgis pgInsert
#' @importFrom sf st_read st_geometry<-
#' @importFrom stringr str_split
#' @importClassesFrom RPostgreSQL PostgreSQLConnection
#' @importClassesFrom taxlist taxlist
#' @importClassesFrom vegtable vegtable
#' @import taxlist
#' @import vegtable
NULL
# TODO: replace read_pg for a general function working with any DB
