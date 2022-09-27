#' @name as
#' @docType methods
#' @rdname coerce-methods
#'
#' @title Coerce objects to lists
#'
#' @description
#' Coerce vegtable objects to a list with every slot as a component of the list.
#' This way allows to explore content and solve problems when validity checks
#' fail.
#'
#' Coercion is applied for different classes by vegtable.
#'
#' @param object An object to be coerced.
#' @param Class A character value with the target class (not used).
#' @param value A character value indicating the class to be coerced to. This is
#'     only required by the replacement methods.
#'
#' @return An object of class `list`.
#'
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#'
#' @aliases coerce coerce,lib_db,lib_df-method
setAs(
  from = "lib_db", to = "lib_df",
  def = function(from) {
    from <- file_list2string(from)
    return(from@main_table)
  }
)

#' @name as
#' @rdname coerce-methods
#' @aliases coerce,lib_df,lib_db-method
setAs("lib_df", "lib_db", function(from) {
  OUT <- new("lib_db")
  OUT@main_table <- from[, names(from) != "file"]
  OUT@file_list <- file_list(from)
  return(OUT)
})

#' @rdname coerce-methods
#' @aliases as<- as<-,lib_df,missing,character-method
setReplaceMethod(
  "as", signature(
    object = "lib_df", Class = "missing",
    value = "character"
  ),
  function(object, Class, value) {
    object <- as(object, value)
    return(object)
  }
)

#' @rdname coerce-methods
#' @aliases as<- as<-,lib_db,missing,character-method
setReplaceMethod(
  "as", signature(
    object = "lib_db", Class = "missing",
    value = "character"
  ),
  function(object, Class, value) {
    object <- as(object, value)
    return(object)
  }
)
