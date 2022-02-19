#' @name get_countries
#'
#' @title  After import assignment of countries for vegtable objects
#'
#' @description
#' This function access to a map in the database and assign countries of
#' samples by overlying locations with the map.
#'
#' NA's are re-assigned by proximity.
#'
#' @param conn A [PostgreSQLConnection-class] object.
#' @param object A [vegtable-class] object.
#' @param rel_name A character value indicating the name of the new inserted
#'     relation.
#' @param geom_name A character value indicating the name of the geometry
#'     including plot coordinates. It is assumed to be a relation in the object.
#' @param ... Further arguments passed among methods.
#'
#' @return An object of class [vegtable-class] with added country codes.
#'
#' @author Miguel Alvarez \email{malvarez@@uni-bonn.de}
#'
#' @rdname get_countries
#'
#' @exportMethod get_countries
setGeneric(
  "get_countries",
  function(conn, object, ...) {
    standardGeneric("get_countries")
  }
)

#' @rdname get_countries
#'
#' @aliases get_countries,PostgreSQLConnection,vegtable-method
setMethod(
  "get_countries",
  signature(conn = "PostgreSQLConnection", object = "vegtable"),
  function(conn,
           object,
           rel_name = "country_code",
           geom_name = "plot_centroid",
           ...) {
    # Import map
    Query <- paste0(
      "SELECT adm0_a3, unit\n",
      "FROM commons.countries_map;\n"
    )
    Countries_map <- st_read(conn, query = Query)
    # Overlay with plot locations by proximity
    Countries <- list()
    Countries[[rel_name]] <- with(
      Countries_map,
      adm0_a3[st_nearest_feature(object@relations[[geom_name]], Countries_map)]
    )
    object@header[, rel_name] <- Countries[[rel_name]][match(
      object@header[, geom_name],
      as.data.frame(object@relations[[geom_name]])[, geom_name]
    )]
    # Import relation table
    Countries <- dbGetQuery(conn, "SELECT * FROM commons.countries;")
    colnames(Countries) <- c(
      rel_name, "name_short", "name_long",
      "population", "sov_code_1", "sov_code_2", "sov_state", "continent"
    )
    object@relations[[rel_name]] <- Countries
    return(object)
  }
)
