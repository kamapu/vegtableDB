#' @importFrom DBI dbGetQuery
#' @importFrom leaflet addCircleMarkers addMiniMap addProviderTiles addScaleBar
#'     leaflet
#' @importFrom magrittr %>%
#' @importFrom mapview mapshot
#' @importFrom methods as new setOldClass
#' @importFrom rpostgis pgInsert
#' @importFrom sf st_coordinates st_drop_geometry st_nearest_feature st_read
#' @importFrom specimens as_data.frame
#' @importFrom taxlist dissect_name replace_x
#' @importFrom utils askYesNo
#' @importFrom yamlme render_rmd txt_body write_rmd
#' @importClassesFrom specimens specimens
#' @importClassesFrom RPostgreSQL PostgreSQLConnection
#' @importClassesFrom yamlme rmd_doc
NULL
